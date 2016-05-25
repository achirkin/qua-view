/** MIT License
 * @authors Lukas Treyer, Artem Chirkin
 * @date Apr 28, 2016
 * */

// these are global dependencies, only faultylabs is non-standard
/*global faultylabs, Blob, WebSocket, console*/

// Use this to hide console output in production using closure compiler flag "--define='DEBUG=false'"


// declare an isolated namespace Luci, and use strict language subset inside for better performance
var Luci = (function () {
    'use strict';
    
    // the code inside this function should be completely eliminated by closure compiler
    // if supplied with  "--define='DEBUG=false'"
    function logDebug() {
        return DEBUG && (typeof (console) !== 'undefined') && console.log.apply(console, arguments);
    }
    
    function typeof_o(o) {
        if (typeof (o) === "object") {
            /*jslint regexp: true */
            return Object.prototype.toString.call(o).replace(/\[object (.+)\]/, "$1");
        }
        return typeof (o);
    }

    function generateUUID() {
        var d = new Date().getTime(),
            uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
                /*jslint bitwise: true */
                var r = ((d + Math.random() * 16) % 16) | 0;
                d = Math.floor(d / 16);
                return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16);
            });
        return uuid;
    }
    
    var imageFormats = ['png', 'jpg', 'jpeg', 'svg', 'bmp', 'tif', 'tiff'],
        Image = (function () {
            /*jslint nomen: true*/
            var _src, _format, _width, _height,
                Image = function (src, format, width, height) {
                    if (imageFormats.indexOf(format) < 0) { throw new Error("given format is no image format"); }
                    _src = src;
                    _format = format;
                    _width = width;
                    _height = height;
                };
            Image.prototype.getSource = function () { return _src; };
            Image.prototype.getFormat = function () { return _format; };
            Image.prototype.getWidth  = function () { return _width; };
            Image.prototype.getHeight = function () { return _height; };
            return Image;
        }()),

        globalAttachmentID = 1,
        
        CustomEventQueue = (function () {
            var q = {}, q1 = {},
                CustomEventQueue = function () {
                    //constructor
                };

            CustomEventQueue.prototype.on = function (e, callback) {
                (q[e] = q[e] || []).push(callback);
            };

            CustomEventQueue.prototype.one = function (e, callback) {
                (q1[e] = q1[e] || []).push(callback);
            };

            CustomEventQueue.prototype.trigger = function (e, params) {
                var qe = q[e],
                    q1e = q1[e],
                    i;
                if (qe) {
                    for (i in qe) {
                        if (qe.hasOwnProperty(i)) { qe[i](params); }
                    }
                }
                if (q1e) {
                    // any callbacks added from within a callback will not be executed here since len is set only once
                    /*jslint plusplus: true */
                    for (i = 0; i < q1e.length; i++) {
                        q1e.shift()(params);
                    }
                }
            };
            return CustomEventQueue;
        }()),
        
        Attachment = (function () {
            /*jslint nomen: true*/
            var _this, id, headerDescr, format, name, position, blobOrFile;

            Attachment = function (obj, n, f) {
                _this = this;
                name = n;
                format = f;
                position = 0;
                /*jslint plusplus: true */
                id = globalAttachmentID++;
                var t = typeof_o(obj);
                if (t === "ArrayBuffer") {
                    headerDescr = {
                        'format': format,
                        'attachment': {
                            'length': blobOrFile.size,
                            'position': 0,
                            'checksum': faultylabs.MD5(obj)
                        }
                    };
                    blobOrFile = new Blob([obj]);
                } else if (t === "File") {
                    headerDescr = {
                        'format': format,
                        'POST': 0
                    };
                    blobOrFile = obj;
                }
            };

            Attachment.prototype.setPosition = function (p) {
                position = p;
                if (headerDescr.hasOwnProperty('attachment')) {
                    headerDescr.attachment.position = p;
                } else {
                    headerDescr.POST = p;
                }
            };

            Attachment.prototype.getFormat = function () {
                return format;
            };

            Attachment.prototype.getPosition = function () {
                return position;
            };

            Attachment.prototype.getHeaderDescription = function () {
                return headerDescr;
            };

            Attachment.prototype.getID = function () {
                return id;
            };

            Attachment.prototype.send = function (url, guid, onsent, onprogress) {
                var xhr = new XMLHttpRequest(),
                    h = this.getHeaderDescription();
                xhr.open('POST', url, true);
                xhr.setRequestHeader("guid", guid);
                xhr.setRequestHeader("name", name);
                xhr.setRequestHeader("Content-Length", blobOrFile.size.toString());
                xhr.setRequestHeader("Content-Type", blobOrFile.type);
                if (headerDescr.hasOwnProperty('attachment')) {
                    xhr.setRequestHeader("checksum", h.attachment.checksum);
                    xhr.setRequestHeader("position", h.attachment.position);
                } else {
                    xhr.setRequestHeader("position", h.POST);
                }
                xhr.onload = onsent;
                xhr.upload.onprogress = onprogress;
                xhr.send(blobOrFile);
            };
            return Attachment;
        }()),
        
        Message = (function () {
            /*jslint nomen: true*/
            var header,
                _this,
                attachmentDescriptions,
                attachments,
                _jadMap,
                _attMap,
                i,
                j;

            function checkForAttachments(obj) {
                var tj = typeof_o(obj),
                    index,
                    o;
                if (tj === "Object") {
                    if (obj.hasOwnProperty("format") && obj.hasOwnProperty("attachment")) {
                        if (obj.attachment.checksum !== "string") { _jadMap[obj.attachment.position] = obj; }
                    } else if (obj instanceof Attachment) {
                        _attMap[obj.getID()] = obj;
                        return obj.getHeaderDescription();
                    } else { // this branch equals exactly the previous existing branch tj === "Array"
                        for (index in obj) {
                            if (obj.hasOwnProperty(index)) {
                                o = checkForAttachments(obj[index]);
                                if (o !== undefined) { obj[index] = o; }
                            }
                        }
                    }
                }
            }

            Message = function (o) {
                _this = this;
                header = o;
                attachmentDescriptions = [];
                attachments = [];
                _jadMap = {};
                _attMap = {};

                checkForAttachments(header);

                // attachment descriptions
                var i, key, attachment, keys = Object.keys(_jadMap);
                keys.sort();
                /*jslint plusplus: true */
                for (i = 0; i < keys.length; i++) {
                    key = keys[i];
                    attachmentDescriptions.push(_jadMap[key]);
                }

                // attachments
                keys = Object.keys(_attMap);
                keys.sort();
                j = 1;
                for (i = 0; i < keys.length; i++) {
                    key = keys[i];
                    attachment = _attMap[key];
                    attachment.setPosition(j++);
                    attachments.push(attachment);
                }
                i = j = 0;
            };

            Message.prototype.hasAttachments = function () {
                return attachmentDescriptions.length > 0 || attachments.length > 0;
            };

            Message.prototype.getHeader = function () {
                return header;
            };

            Message.prototype.getCallID = function () {
                if (Object.keys(header).indexOf("callID") >= 0) {
                    return header.callID;
                } else { return 0; }
            };

            Message.prototype.hasMoreAttachmentDescriptions = function () {
                return i < attachmentDescriptions.length;
            };

            Message.prototype.nextAttachmentDescription = function () {
                /*jslint plusplus: true */
                return attachmentDescriptions[i++];
            };

            Message.prototype.hasMoreAttachments = function () {
                return j < attachments.length;
            };

            Message.prototype.nextAttachment = function () {
                /*jslint plusplus: true */
                return attachments[j++];
            };

            Message.prototype.rewindAttachmentDescriptionCounter = function () {
                i = 0;
            };

            Message.prototype.rewindAttachmentCounter = function () {
                j = 0;
            };

            Message.prototype.toString = function () {
                return JSON.stringify(header);
            };

            return Message;
        }()),

        ResponseHandler = (function () {
            ResponseHandler = function () {};
            ResponseHandler.prototype.onResult = function (message) {
                logDebug("RESULT: ", message.getHeader());
            };
            ResponseHandler.prototype.onProgress = function (message) {
                var header = message.getHeader();
                logDebug(header.percentage + "% on " + header.callID);
            };
            ResponseHandler.prototype.onError = function (message) {
                var header = message.getHeader();
                logDebug("ERROR: ", header.error);
            };
            return ResponseHandler;
        }()),

        LuciClient = (function () {
            /*jslint nomen: true*/
            var socket, socketURL, message, _this,
                host, port, postURL,
                callbacks,
                callbackQueue,
        //    var waitingForNewCallID = false;
        //    var responseWhileWaitingForNewCallID;
                err = "No valid key! ['newCallID','result','progress','error','run','cancel']";

            LuciClient = function (host, port, path, stdResponseHandler, postURL) {
                callbacks = {};
                callbackQueue = [];
                if (stdResponseHandler !== undefined && typeof (stdResponseHandler) !== "function") {
                    throw new Error("stdResponseHandler must be of type 'function'");
                }
                _this = this;
                this.path = path || "";
                this.port = port || 80;
                this.host = host;
                this.postURL = postURL;
                this.stdResponseHandler = stdResponseHandler || ResponseHandler;
                socketURL = "ws://" + this.host + ":" + this.port + "/" + this.path;
            };

            LuciClient.prototype = new CustomEventQueue(); // LuciClient extends CustomEventQueue

            
            function handle(responseHandler, message, callID) {
                var header = message.getHeader();
                if (header.hasOwnProperty("progress")) {
                    responseHandler.onProgress(message);
                    _this.trigger("progress", message);
                } else if (header.hasOwnProperty("result")) {
                    responseHandler.onResult(message);
                    _this.trigger("result", message);
                    delete callbacks[callID];
                } else if (header.hasOwnProperty("error")) {
                    responseHandler.onError(message);
                    _this.trigger("error", message);
                    delete callbacks[callID];
                } else {
                    throw new Error(err);
                    // this is unreachable: delete callbacks[callID];
                }
            }
            
            function do_onmessage(evt) {
                var msg, header, callID, responseHandler, msgData = evt.data;
                if (typeof (msgData) === "string") {
                    msg = JSON && JSON.parse(msgData);
                    message = new Message(msg);
                    header = message.getHeader();
                    if (header.hasOwnProperty("newCallID")) {
                        callID = header.newCallID;
                        callbacks[callID] = callbackQueue.shift() || new _this.stdResponseHandler();
                    } else if (header.hasOwnProperty("callID")) {
                        callID = header.callID;
                        responseHandler = callbacks[callID] || new _this.stdResponseHandler();
                        handle(responseHandler, message, callID);
                    } else if (header.hasOwnProperty("run")) {
                        _this.trigger("run", message);
                    } else if (header.hasOwnProperty("cancel")) {
                        _this.trigger("cancel", message);
                    } else { throw new Error(err); }
                } else { throw new Error("Received binary message!"); }
            }
            
            function send(obj) {
                if (!_this.isConnected()) { throw new Error("not connected!"); }
                var a, guid, m = new Message(obj),
                    reportSent = function () { logDebug("sent!"); };
                if (m.hasAttachments()) {
                    guid = generateUUID();
                    m.getHeader().guid = guid;
                    socket.send(m.toString());
                    // logDebug("guid", guid);
                    while (m.hasMoreAttachments()) {
                        a = m.nextAttachment();
                        logDebug("sending ", a);
                        a.send(_this.postURL, guid, reportSent);
                    }
                } else { socket.send(m.toString()); }
            }
            
            LuciClient.prototype.connect = function (do_onopen, do_onclose, do_onerror) {
                logDebug("connecting to " + socketURL);
                socket = new WebSocket(socketURL);
                if (do_onopen !== undefined) { socket.onopen = do_onopen; }
                if (do_onclose !== undefined) { socket.onclose = do_onclose; }
                if (do_onerror !== undefined) { socket.onerror = do_onerror; }
                socket.onmessage = do_onmessage;
            };

            LuciClient.prototype.isConnected = function () {
                return socket !== undefined;
            };

            LuciClient.prototype.disconnect = function (onClose) {
                if (onClose !== undefined) {
                    socket.onclose = function () {
                        onClose();
                        socket = undefined;
                    };
                }
                socket.close();
            };

            LuciClient.prototype.getCurrentlyReceivedMessage = function () {
                return message;
            };

            LuciClient.prototype.getSocketURL = function () {
                return socketURL;
            };

            LuciClient.prototype.sendAndReceive = function (obj, responseHandler) {
                if (!_this.isConnected()) { throw new Error("not connected!"); }
                if (responseHandler !== undefined) {
                    if (!(responseHandler instanceof ResponseHandler)) {
                        if (typeof (responseHandler) === "function") {
                            var resp = new _this.stdResponseHandler();
                            resp.onResult = responseHandler;
                            responseHandler = resp;
                        } else {
                            throw new Error("Given argument is no ResponseHandler or function: " + typeof_o(responseHandler));
                        }
                    }
                    callbackQueue.push(responseHandler);
                } else if (_this.stdResponseHandler === undefined) {
                    throw new Error("No standard ResponseHandler set!");
                }
                send(obj);
            };

            LuciClient.prototype.send = function (obj) {
                if (!_this.isConnected()) { throw new Error("not connected!"); }
                if (_this.stdResponseHandler === undefined) {
                    throw new Error("Cannot call 'send' without stdResponseHandler being set!");
                }
                // yes the standard response must be defined in order to reliably handle events
                callbackQueue.push(new _this.stdResponseHandler());
                send(obj);
            };

            return LuciClient;
        }());
    
    // these are the functions we expose in the namespace Luci
    return {
        imageFormats: imageFormats,
        Image: Image,
        CustomEventQueue: CustomEventQueue,
        Attachment: Attachment,
        Message: Message,
        ResponseHandler: ResponseHandler,
        Client: LuciClient
    };
}());
