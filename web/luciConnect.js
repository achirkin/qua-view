//#
//# Created: MAY 2014
//# Author: Lukas Treyer
//# License: MIT
//#
//# using -jQuery: parseJSON(), event.trigger(), on()
//#       -faultylabs.md5_compact_min (named md5.js in luci/web/js)
//#
//# test on js console with:  
//# 	var lc = new LuciClient("localhost", 8080, "ws/");
//#		lc.authenticate("lukas","1234", function(lc){console.log(lc.getMessage().result)})
//# 	--> [Log] User 'lukas' authenticated!

//Taken from https://mths.be/punycode
//function ucs2decode(string) {
//	var output = [];
//	var counter = 0;
//	var length = string.length;
//	var value;
//	var extra;
//	while (counter < length) {
//		value = string.charCodeAt(counter++);
//		if (value >= 0xD800 && value <= 0xDBFF && counter < length) {
//			// high surrogate, and there is a next character
//			extra = string.charCodeAt(counter++);
//			if ((extra & 0xFC00) == 0xDC00) { // low surrogate
//				output.push(((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000);
//			} else {
//				// unmatched surrogate; only append this code unit, in case the next
//				// code unit is the high surrogate of a surrogate pair
//				output.push(value);
//				counter--;
//			}
//		} else {
//			output.push(value);
//		}
//	}
//	return output;
//}

function typeof_o(o){
	if (typeof(o) == "object"){
		return Object.prototype.toString.call(o).replace(/\[object (.+)\]/, "$1");
	}
	return typeof(o);
}

//function isNativeLittle(){
//	var arr16 = new Uint16Array(1);
//	arr16[0] = 255;
//	var arr8 = new Uint8Array(arr16.buffer);
//	return arr8[0] == 255;
//}
//
//function packBigEndian(number){
//	var arr32 = new Uint32Array(2);
//	if (isNativeLittle()) {
//		arr32[0] = number;
//		var arr8 = new Uint8Array(arr32.buffer);
//		var arr = new Uint8Array(8);
//		for (var i = 7, j = 0; i >= 0; i--){
//			arr[i] = arr8[j++];
////			console.log(i + " - " + j);
//		}
//		return arr.buffer;
//	}
//	arr32[1] = number;
//	var arr8 = new Uint8Array(arr32.buffer);
//	return arr8.buffer;
//}

//function array2buffer(arr){
//	var buf = new Uint8Array(arr.length);
//	for (var i = 0; i < arr.length; i++){
//		buf[i] = arr[i];
//	}
//	return buf.buffer;
//}

//function buffer2array(buf){
//	var arr = [];
//	for (var i = 0; i < buf.byteLength; i++){
//		arr[i] = buf[i];
//	}
//	return arr;
//}

//function mergeBuffers(bufs){
//	var l  = 0;
//	for (var i = 0; i < bufs.length; i++){
//		var buf = bufs[i];
//		l += buf.byteLength;
//	}
//	var arr8 = new Uint8Array(l);
//	for (var i = 0, j = 0; i < bufs.length; i++){
////		console.log("  :  "+bufs[i][0]);
//		var buf = new Uint8Array(bufs[i]);
////		console.log("  -  "+buf[0]);
//		arr8.set(buf, j);
//		j += buf.byteLength;
//	}
////	console.log(arr8[0]);
//	return arr8.buffer;
//}

var CounterLatch = function(cb){
	this.callback = cb;
	this.count = 0;
	this.started = false;
	this.counted = false;

	this.countDown = function(amount){
		if (typeof(amount) == "undefined") amount = 1;
		this.count = this.count - amount;
		this.counted = true;
		if (this.started && this.count == 0) {
			this.callback();
		}
		return this;
	}
	this.countUp = function(amount){
		if (typeof(amount) == "undefined") amount = 1;
		this.count = this.count + amount;
		this.counted = true;
		if (this.started && this.count == 0) {
			this.callback();
		}
		return this;
	}
	this.reset = function(){
		this.count = 0;
		this.counted = false;
		this.started = false;
		return this;
	}
	
	this.start = function(){
		this.started = true;
		if (this.counted && this.count == 0) {
			this.callback();
		}
		return this;
	}
};

var CustomEventQueue = (function(){
	var q = {};
	var q1 = {};

	CustomEventQueue = function(){
		//constructor
	}

	CustomEventQueue.prototype.on = function(e,callback) {
		(q[e] = q[e] || []).push( callback );
	},

	CustomEventQueue.prototype.one = function(e,callback) {
		(q1[e] = q1[e] || []).push( callback );
	},

	CustomEventQueue.prototype.trigger = function(e,params) {
		var qe = q[e];
		var q1e = q1[e];

		if( qe ) {
			for( i in qe ){
				qe[i](this,params);
			}
		}
		if( q1e ) {
			var len = q1e.length;
			// any callbacks added from within a callback will not be executed here since len is set only once
			for(var i = 0; i < len; i++){
				q1e.shift()(this,params);
			}
		}
	}
	return CustomEventQueue;
})();

var LuciClient = (function(){
	var socket;
	var _bytes_in = {};
	var blocked = false;
	var msgLatch;
	var filesReadLatch;
	var _onpost_msg = [];
	var message;
	var order = 1;
	var _this;
	var isAuth = false;
	var host;
	var port;
	var clientID;
	
	var file_lengths = {};
	var file_progress = {};
	var total_length = 0;
	var receive_progress = 0;

	LuciClient = function(host, port, path, do_onopen, do_onclose, do_onerror){
		this.path = path || "";
		this.port = port || 80;
		this.host = host;
		socket = new WebSocket("ws://"+this.host+":"+this.port+"/"+path);
		socket.binaryType = "arraybuffer";
		if (do_onopen !== undefined) socket.onopen = do_onopen;
		if (do_onclose !== undefined) {
			// TODO: reset clientID to null;
			socket.onclose = do_onclose;
		}
		if (do_onerror !== undefined) socket.onerror = do_onerror;
		socket.onmessage = do_onmessage;
		msgLatch = new CounterLatch(function(){
			_streaminfo_check(message);
			blocked = false;
			_this.trigger("onmessage");
		}).start();
		_this = this;
	}

	LuciClient.prototype = new CustomEventQueue(); // LuciClient extends CustomEventQueue

	LuciClient.prototype.getMessage = function(){
		return message;
	}

	function do_onmessage(evt){
		var msg = evt.data;
		if (typeof(msg) == "string") {
			if (clientID === undefined || clientID == null) clientID = msg;
			else {
				message = JSON && JSON.parse(msg) || $.parseJSON(msg);
				if ("progress" in message) _this.trigger("onprogress");
				else msgLatch.countUp(_streaminfo_count(message));
			}
		} else {
			throw new Error("Received binary message!")
		}
	}

	function _streaminfo_count(j){
		if ("format" in j && "streaminfo" in j){
			var chck = j["streaminfo"]["checksum"];
			if (chck != "string") {
				total_length += file_lengths[chck] = j["streaminfo"]["length"];
				file_progress[chck] = 0;
				_download(chck, j["format"]);
				return 1;
			} else return 0;
		} else {
			var i = 0;
			for (var k in j){
				var v = j[k];
				if (typeof_o(v) == "Object"){
					i += _streaminfo_count(v);
				}
			}
			return i;
		}
	}

	function _streaminfo_check(j){
		if ("format" in j && "streaminfo" in j){
			var chksum = j["streaminfo"]["checksum"];
			if (chksum !== "string")
			if (chksum in _bytes_in) j["bytes"] = _bytes_in[chksum];
			else throw new Error("byte-array with checksum '" + chksum + "' not received!");
		} else {
			for (var k in j){
				var v = j[k];
				if (typeof_o(v) == "Object"){
					_streaminfo_check(v);
				}
			}
		}
	}
	
	function _download(checksum, format){
		var url = "http://" + _this.host + ":" + _this.port +"/download/"+checksum+"."+format;
		console.log(url);
		var xhr = new XMLHttpRequest();
		xhr.open('GET', url, true); // parameter 3 = asynchronous
		xhr.responseType = 'arraybuffer';

		xhr.onload = function(e) {
			var vbuf = this.response;
			// _bytes_in[faultylabs.MD5(buf)] = buf;
			_bytes_in[faultylabs.MD5(vbuf)] = vbuf;
			msgLatch.countDown();
		};
		xhr.onprogress = function(e){
			_accumulate_progress(e, checksum, "onprogress_receive");
		}

		xhr.send();
	}
	
	function _accumulate_progress(e, checksum, eventName){
		// accumulate onprogress calls in onprogress_receive
		if (e.lengthComputable) {
		    var percentComplete = e.loaded / e.total;
		    file_progress[checksum] = e.loaded;
		    
		    var p;
		    for (var chck in file_progress){
		    	p += file_progress[chck];
		    }
		    var n = Math.round(total_length / p * 100)/100;
		    if (receive_progress != n) {
		    	receive_progress = n;
		    	_this.trigger(eventName, 
		    			{"lengthComputable":true, "loaded":p, "total":total_length});
		    }
		}
	}
	
	// note: luci's websocket compared to luci's socket does not send the size of the bytearray at the beginning
	function _do_onbinary(buf){
		_bytes_in[faultylabs.MD5(buf)] = buf;
	}

	LuciClient.prototype.authenticate = function(user, pwd, cllbk){
		if (!isAuth){
			var auth = {"action": "authenticate", "username": user, "userpasswd": pwd};
			var cllbks = [function(lc){
//				console.log(lc.getMessage());
				isAuth = true;
			}];
			if (cllbk != undefined) cllbks.push(cllbk);
			this.sendAndReceive(auth, cllbks);
		} else {
			throw new Error("Already authenticated; authentication call omitted!");
		}
	}

	LuciClient.prototype.logout = function(cllbk){
		if (isAuth){
			var lout = {"action": "logout"};
			var cllbks = [function(lc){
				console.log(lc.getMessage());
				isAuth = false;
			}];
			if (cllbk != undefined) cllbks.push(cllbk);
			this.sendAndReceive(lout, cllbks);
		} else {
			throw new Error("Logout: not authenticated; logout call omitted!");
		}
	}
	
	LuciClient.prototype.createScenario = function(name, callback, geometry, projection){
		var action = {"action":"create_scenario","name":name};
		if (geometry !== undefined) action.geometry = geometry;
		if (projection !== undefined) action.projection = projection;
		if (callback !== undefined) this.sendAndReceive(action, [callback]);
		else this.sendAndReceive(action);
	}
	
	LuciClient.prototype.updateScenario = function(ScID, callback, name, geometry, bbox){
		var action = {"action":"update_scenario","ScID":ScID};
		if (geometry !== undefined && geometry != null) action.geometry = geometry;
		if (name !== undefined && name != null) action.name = name;
		if (bbox !== undefined && bbox != null) action.bbox = bbox;
		if (callback !== undefined && callback != null) this.sendAndReceive(action, [callback]);
		else this.sendAndReceive(action);
	}

	LuciClient.prototype.send = function(obj){
//		console.log("send");
		if (!blocked){
			// reset members
			_bytes_in = {};
			message = {};
			blocked = true;
			msgLatch.reset().start();
			
			
			
			var hashed_bytes;
			filesReadLatch = new CounterLatch(function(){
				var bytes = [];
				var chcks = Object.keys(hashed_bytes);
				for (var i = 0; i <chcks.length ; i++){
					var chck = chcks[i];
					var info = hashed_bytes[chck];
					bytes.push(info["buf"]);
					delete info["buf"];
					info["order"] = i + 1;
				}
				socket.send(JSON.stringify(obj));
				_onpost_msg = [];
				file_lengths = {};
				file_progress = {};
				total_length = 0;
				receive_progress = 0;
				var onload = function(e) {
					// add response to a response array and raise an onsent event if 
					// we 've got equal amount of answers as amount of bytes sent
					_onpost_msg.push(e);
					if (_onpost_msg.length == bytes.length) _this.trigger("onsent", _onpost_msg);
				};
				for (var i = 0; i < bytes.length; i++){
					var arr = bytes[i];
					var xhr = new XMLHttpRequest();
					xhr.open('POST', '/upload?clientID='+clientID+'&checksum='+chcks[i], true);
//					xhr.setRequestHeader("clientID", "123");
//					xhr.setRequestHeader("checksum", chcks[i]);
					xhr.onload = onload;
					xhr.onprogress = function(e){
						// handle progress and forward it to a onprogress event of _this
						// that accumulates all onprogress messages in onprogress_send
						if (e.type == "load") onload(e);
						else _accumulate_progress(e, chcks[i], "onprogress_send");
					}
					// TODO: replace ArrayBuffer with Blob
					xhr.send(new Blob([arr]));
				}
				if (_onpost_msg.length == bytes.length) _this.trigger("onsent");
			});
			hashed_bytes = _attachment_check(obj);
			filesReadLatch.countDown(0).start();

		} else {
			throw new Error("LuciClient is blocked waiting for an answer from '"+socket.url+"'!");
		}
	}

	LuciClient.prototype.sendAndReceive = function(obj, handlers){
		if (handlers !== undefined){
			for (var i in handlers){
				_this.one("onmessage", handlers[i]);
			}
		}
		this.send(obj);
	}

	function _attachment_check(obj){
		var bytes = {};
		for (var k in obj){
			var v = obj[k];
			var t = typeof_o(v);
			if (t == "Object"){
				var sub = _attachment_check(v);
				for (var checksum in sub){
					bytes[checksum] = sub[checksum];
				}
			} else if (k == "bytes" && t == "ArrayBuffer"){
				// TODO: replace ArrayBuffer with Blob
				var len = v.byteLength;
				_attachment_pack(obj, v, len, bytes);
				delete obj["bytes"];
			} else if (k == "file" && t == "File") {
				filesReadLatch.countUp(1);
				var len = v.size;
				var reader = new FileReader();
				reader.onload = function(e){
					_attachment_pack(obj, v, len, bytes);
					delete obj["file"];
				}
				reader.readAsArrayBuffer(v);
			}
		}
		return bytes;
	}
	
	function _attachment_pack(obj, buf, len, bytes){
		if (len < Math.pow(2,32)){
			var checksum = faultylabs.MD5(buf);
			var s = obj["streaminfo"] = {"checksum":checksum, "length":len, "buf":buf};
			bytes[checksum] = s;
		} else throw new Error("Javascript numbers are limited to 32bit; file sizes to 4GB");
	}
	
	LuciClient.prototype.isValidLuciJSON = function(j){
		if (!("action" in j || "result" in j || "error" in j || "progress" in j))
			return false;
		return true;
	}

	return LuciClient;

}());



//var LuciRemoteService = (function(){
//	var inputIndex = {};
//	var outputIndex = {};
//	var servicename;
//	var machinename;
//	var description;
//	var version;
//	var inputs;
//	var outputs;
//	var hashcode;
//	var sobjid;
//	var scid;
//	var scenario_timestamp;
//	var outputStreams;
//	var registered;
//	var task;
//	var _this;

//	LuciRemoteService = function(
//			s, // servicename
//			i, // inputIndex / input description (javascript object following the Luci Meta JSON Spec)
//			o, // outputIndex / output desciption (javascript object following the Luci Meta JSON Spec)
//			m, // machinename
//			d, // description (what the service is doing)
//			v, // version
//			t  // task / callback
//		){
//		if (typeof_o(i) != "Object") throw new Error("inputIndex must be of type 'object' {}");
//		if (typeof_o(o) != "Object") throw new Error("outputIndex must be of type 'object' {}");
//		inputIndex = i;
//		outputIndex = o;
//		servicename = s;
//		machinename = m;
//		description = d;
//		version = v;
//		task = t;
//		_this = this;
//	}

//	LuciRemoteService.prototype = LuciClient; // inheritance

//	LuciRemoteService.prototype.getTask = function(){
//		return task;
//	}

//	LuciRemoteService.prototype.register = function(onsuccess, onfailure){
//		var r = {"action":"remote_register", "machinename":this.machinename, "service":{
//			"classname":this.servicename, "version":this.version, "description":this.description,
//			"inputs":this.inputIndex, "outputs":this.outputIndex
//		}
//		}
//		_this.one("onregister_success", onsuccess);
//		_this.one("onregister_failure", onfailure);
//		_this.on("onmessage", function(e){
//			e.stopPropagation();
//			var run = e.target.message;
//			if ("action" in run ){
//				var action = run["action"];
//				if(action == "run"){
//					if ("SObjID" in run){
//						if ("input_hashcode" in run){
//							var result = {"result":"any","input_hashcode":run["input_hashcode"],"SObjID":run["SObjID"]}
//							var o = this.task(run.inputs);
//							if (typeof_o(o) != "Object") {
//								result["outputs"] = o;
//								this.send(result);
//							} else this.send({"error":"LuciRemoteService: invalid task return type!"});
//						} else {
//							this.send({"error":"remote service '"+this.servicename+"' is missing the input_hashcode"});
//						}
//					} else {
//						this.send({"error":"remote service '"+this.servicename+"' is missing the SObjID"});
//					}
//				} else if (action == "cancel") {
//					this.send({"error":"LuciConnect.js is being developed under the assumption of a " +
//						"single threaded event queue environment as it is typical for browsers " +
//						"and most situations in node.js. Adapt it to your needs if you wish to " +
//						"use javascript in a multi-threaded environment. You may want to consider " +
//					"also this post: http://stackoverflow.com/a/8845948"});
//				} else {
//					this.send({"error":"remote service '"+this.servicename+"': invalid action '"+action+"'."});
//				}
//			} else if ("result" in run){

//			} else if ("error" in run){

//			} else {
//				this.send({"error":"json sent to '"+this.servicename+"' is missing an action"});
//			}
//		});
//		this.sendAndReceive(r, [function(lc){
//			var answer = lc.getMessage();
//			if ("result" in answer){
//				this.registered = true;
//				this.trigger("onregister_success");
//			} else {
//				this.trigger("onregister_failure");
//			}
//		}]);
//	}

//	LuciRemoteService.prototype.unregister = function(onsuccess, onfailure){
//		var u = {"action":"remote_unregister"};
//		this.one("onunregister_success", onsuccess);
//		this.one("onunregister_failure", onfailure);
//		if (this.registered){
//			this.sendAndReceive(u, [function(lc){
//				var answer = lc.getMessage();
//				if ("result" in answer){
//					this.trigger("onunregister_success");
//					this.registered = false;
//				} else {
//					this.trigger("onunregister_failure");
//				}
//			}]);
//		} else {
//			this.trigger("onunregister_failure");
//		}
//	}

//	return LuciRemoteService;
//})();
