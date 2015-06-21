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

// Taken from https://mths.be/punycode
function ucs2decode(string) {
	var output = [];
	var counter = 0;
	var length = string.length;
	var value;
	var extra;
	while (counter < length) {
		value = string.charCodeAt(counter++);
		if (value >= 0xD800 && value <= 0xDBFF && counter < length) {
			// high surrogate, and there is a next character
			extra = string.charCodeAt(counter++);
			if ((extra & 0xFC00) == 0xDC00) { // low surrogate
				output.push(((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000);
			} else {
				// unmatched surrogate; only append this code unit, in case the next
				// code unit is the high surrogate of a surrogate pair
				output.push(value);
				counter--;
			}
		} else {
			output.push(value);
		}
	}
	return output;
}

function typeof_o(o){
	if (typeof(o) == "object"){
		return Object.prototype.toString.call(o).replace(/\[object (.+)\]/, "$1");
	}
	return typeof(o);
}

function isNativeLittle(){
	var arr16 = new Uint16Array(1);
	arr16[0] = 255;
	var arr8 = new Uint8Array(arr16.buffer);
	return arr8[0] == 255;
}

function packBigEndian(number){
	var arr32 = new Uint32Array(2);
	if (isNativeLittle()) {
		arr32[0] = number;
		var arr8 = new Uint8Array(arr32.buffer);
		var arr = new Uint8Array(8);
		for (var i = 7, j = 0; i >= 0; i--){
			arr[i] = arr8[j++];
//			console.log(i + " - " + j);
		}
		return arr.buffer;
	}
	arr32[1] = number;
	var arr8 = new Uint8Array(arr32.buffer);
	return arr8.buffer;
}

function array2buffer(arr){
	var buf = new Uint8Array(arr.length);
	for (var i = 0; i < arr.length; i++){
		buf[i] = arr[i];
	}
	return buf.buffer;
}

function buffer2array(buf){
	var arr = [];
	for (var i = 0; i < buf.byteLength; i++){
		arr[i] = buf[i];
	}
	return arr;
}

function mergeBuffers(bufs){
	var l  = 0;
	for (var i = 0; i < bufs.length; i++){
		var buf = bufs[i];
		l += buf.byteLength;
	}
	var arr8 = new Uint8Array(l);
	for (var i = 0, j = 0; i < bufs.length; i++){
//		console.log("  :  "+bufs[i][0]);
		var buf = new Uint8Array(bufs[i]);
//		console.log("  -  "+buf[0]);
		arr8.set(buf, j);
		j += buf.byteLength;
	}
//	console.log(arr8[0]);
	return arr8.buffer;
}

var CounterLatch = (function(){
	var callback;
	var count = 0;
	
	CounterLatch = function(cb){
		callback = cb;
	}
	
	CounterLatch.prototype.countDown = function(amount){
		if (typeof(amount) == "undefined") amount = 1;
		count = count - amount;
		if (count == 0) callback();
	}
	CounterLatch.prototype.countUp = function(amount){
		if (typeof(amount) == "undefined") amount = 1;
		count = count + amount;
		if (count == 0) callback();
	}
	CounterLatch.prototype.reset = function(){
		count = 0;
	}
	
	return CounterLatch;
	
})();

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
    	if (!q1[e]) q1[e] = [];
    	q1[e].push( callback );
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
			while(q1e.length > 0){
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
	var message;
	var order = 1;
	var _this;
	var isAuth = false;
	
	LuciClient = function(host, port, path, do_onopen){
		path = path || "";
		port = port || 80;
		socket = new WebSocket("ws://"+host+":"+port+"/"+path);
		socket.binaryType = "arraybuffer";
		if (do_onopen !== undefined) socket.onopen = do_onopen;
		socket.onmessage = do_onmessage;
		msgLatch = new CounterLatch(function(){
			_streaminfo_check(message);
			blocked = false;
			_this.trigger("onmessage");
		});
		_this = this;
	}
	
	LuciClient.prototype = new CustomEventQueue(); // LuciClient extends CustomEventQueue
	
	LuciClient.prototype.getMessage = function(){
		return message;
	}

	function do_onmessage(evt){
		var msg = evt.data;
		if (typeof(msg) == "string") {
			_do_ontext(msg);
		} else if (typeof_o(msg) == "ArrayBuffer"){
			_do_onbinary(msg);
		}
	}
	
	function _do_ontext(msg){
		message = JSON && JSON.parse(msg) || $.parseJSON(msg);
		msgLatch.countUp(_streaminfo_count(message));
	}
	
	function _streaminfo_count(j){
		if ("format" in j && "streaminfo" in j){
			return 1;
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
	
	function _do_onbinary(buf){
		var chksum = faultylabs.MD5(buf);
		_bytes_in[chksum] = buf.slice(8);
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
//				console.log(lc.getMessage());
				isAuth = false;
			}];
			if (cllbk != undefined) cllbks.push(cllbk);
			this.sendAndReceive(lout, cllbks);
		} else {
			throw new Error("Logout: not authenticated; logout call omitted!");
		}
		
	}
	
	LuciClient.prototype.send = function(obj){
		if (!blocked){
			// reset members
			_bytes_in = {};
			message = {};
			blocked = true;
			msgLatch.reset();
			// send
			order = 1;
			var bytes = _ArrayBuffer_check(obj);
//			for (var i = 0; i < bytes.length; i++){
//				console.log("t: "+typeof_o(bytes[i]));
//				console.log(new Uint8Array(bytes[i])[0]);
//			}
			if (bytes.length > 0){
				var jsonBytes = ucs2decode(JSON.stringify(obj));
				jsonBytes.push(10);
//				console.log(jsonBytes);
//				var buf = array2buffer(jsonBytes);
//				console.log(buf);
//				console.log(buffer2array(buf))
				bytes.splice(0,0,array2buffer(jsonBytes));
				var arr = mergeBuffers(bytes);
				socket.send(arr);
				
			} else {
				socket.send(JSON.stringify(obj));
			}
		} else {
			throw new Error("LuciClient is blocked waiting for an answer from '"+socket.url+"'!");
		}
	}
	
	LuciClient.prototype.sendAndReceive = function(obj, handlers){
		for (var i in handlers){
			this.one("onmessage", handlers[i]);
		}
		this.send(obj);
	}
	
	function _ArrayBuffer_check(obj){
		var bytes = [];
		for (var k in obj){
			var v = obj[k];
			var t = typeof_o(v);
			if (t == "Object"){
				bytes = bytes.concat(_ArrayBuffer_check(v));
			} else if (k == "bytes" && t == "ArrayBuffer"){
				var len = v.byteLength;
				if (len < Math.pow(2,32)){
					var checksum = faultylabs.MD5(v);
					obj["streaminfo"] = {"checksum":checksum, "order":order++};
					delete obj["bytes"];
					bytes.push(packBigEndian(len));
					bytes.push(v);
				} else throw new Error("Javascript numbers are limited to 32bit; file sizes to 4GB");
			}
		}
		return bytes;
	}
	
	LuciClient.prototype.isValidLuciJSON = function(j){
		if (!("action" in j || "result" in j || "error" in j || "progress" in j))
			return false;
		return true;
	}
	
	return LuciClient;
	
}());



var LuciRemoteService = (function(){
	var inputIndex = {};
	var outputIndex = {};
	var servicename;
	var machinename;
	var description;
	var version;
	var inputs;
	var outputs;
	var hashcode;
	var sobjid;
	var scid;
	var scenario_timestamp;
	var outputStreams;
	var registered;
	var task;
	
	LuciRemoteService = function(servicname, i, o, m, d, v, t){
		if (typeof_o(i) != "Object") throw new Error("inputIndex must be of type 'object' {}");
		if (typeof_o(o) != "Object") throw new Error("outputIndex must be of type 'object' {}");
		inputIndex = i;
		outputIndex = o;
		servicename = servicename;
		machinename = m;
		description = d;
		version = v;
		task = t;
	}
	
	LuciRemoteService.prototype = LuciClient; // inheritance
	
	LuciRemoteService.prototype.getTask = function(){
		return task;
	}
	
	LuciRemoteService.prototype.register = function(onsuccess, onfailure){
		var r = {"action":"remote_register", "machinename":this.machinename, "service":{
				"classname":this.servicename, "version":this.version, "description":this.description,
				"inputs":this.inputIndex, "outputs":this.outputIndex
			}
		}
		this.one("onregister_success", onsuccess);
		this.one("onregister_failure", onfailure);
		this.on("onmessage", function(e){
			e.stopPropagation();
			var run = e.target.message;
			if ("action" in run ){
				var action = run["action"];
				if(action == "run"){
					if ("SObjID" in run){
						if ("input_hashcode" in run){
							var result = {"result":"any","input_hashcode":run["input_hashcode"],"SObjID":run["SObjID"]}
							var o = this.task(run.inputs);
							if (typeof_o(o) != "Object") {
								result["outputs"] = o;
								this.send(result);
							} else this.send({"error":"LuciRemoteService: invalid task return type!"});
						} else {
							this.send({"error":"remote service '"+this.servicename+"' is missing the input_hashcode"});
						}
					} else {
						this.send({"error":"remote service '"+this.servicename+"' is missing the SObjID"});
					}
				} else if (action == "cancel") {
					this.send({"error":"LuciConnect.js is being developed under the assumption of a " +
							"single threaded event queue environment as it is typical for browsers " +
							"and most situations in node.js. Adapt it to your needs if you wish to " +
							"use javascript in a multi-threaded environment. You may want to consider " +
							"also this post: http://stackoverflow.com/a/8845948"});
				} else {
					this.send({"error":"remote service '"+this.servicename+"': invalid action '"+action+"'."});
				}
			} else if ("result" in run){
				
			} else if ("error" in run){
				
			} else {
				this.send({"error":"json sent to '"+this.servicename+"' is missing an action"});
			}
		});
		this.sendAndReceive(r, [function(lc){
			var answer = lc.getMessage();
			if ("result" in answer){
				this.registered = true;
				this.trigger("onregister_success");
			} else {
				this.trigger("onregister_failure");
			}
		}]);
	}
	
	LuciRemoteService.prototype.unregister = function(onsuccess, onfailure){
		var u = {"action":"remote_unregister"};
		this.one("onunregister_success", onsuccess);
		this.one("onunregister_failure", onfailure);
		if (this.registered){
			this.sendAndReceive(u, [function(lc){
				var answer = lc.getMessage();
				if ("result" in answer){
					this.trigger("onunregister_success");
					this.registered = false;
				} else {
					this.trigger("onunregister_failure");
				}
			}]);
		} else {
			this.trigger("onunregister_failure");
		}
	}
	
	return LuciRemoteService;
})();

