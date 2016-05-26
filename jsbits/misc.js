// Log messages to in-app text console
logText = function(text) {
    var elem = document.getElementById('consolecontent');
    var n = elem.children.length;
    var panelr = document.getElementById('guipanel').getBoundingClientRect();
    var z = panelr.top + 0.6*panelr.height;
    while(n > 0 && elem.children[0].getBoundingClientRect().top < z) {
      elem.removeChild(elem.children[0]); n--;
    }
    for(var i = 0; i < n; i++) {
       elem.children[i].className = 'consolem' + Math.max(i-n+9,0);
    }
    var newDiv = document.createElement('div');
    newDiv.innerHTML = text;
    newDiv.className = 'consolem9';
    if (arguments.length > 1) {
      newDiv.id = arguments[1];
    }
    elem.appendChild(newDiv);
}

var httpArgs = (function() {
  // This function is anonymous, is executed immediately and
  // the return value is assigned to QueryString!
  var query_string = new Object();
  var query = window.location.search.substring(1);
  var vars = query.split("&");
  for (var i=0;i<vars.length;i++) {
    var pair = vars[i].split("=");
        // If first entry with this name
    if (typeof query_string[pair[0]] === "undefined") {
      query_string[pair[0]] = decodeURIComponent(pair[1]);
        // If second entry with this name
    } else if (typeof query_string[pair[0]] === "string") {
      var arr = [ query_string[pair[0]],decodeURIComponent(pair[1]) ];
      query_string[pair[0]] = arr;
        // If third or later entry with this name
    } else {
      query_string[pair[0]].push(decodeURIComponent(pair[1]));
    }
  }
  return query_string;
})();

var internalProcessId = 0;
function logExternalProcess(text) {
  var ls = document.getElementById('loadingSplash').cloneNode(true);
  ls.id =  "progress_" + internalProcessId;
  ls.style.display = 'inline';
  ls.style.width = '1em';
  ls.style.height = '1em';
  ls.style.transform = 'translate(0%, 20%)';
  internalProcessId++;
  logText(text, "log_" + ls.id);
  document.getElementById("log_" + ls.id).appendChild(ls);
  return ls.id;
}

function notifyFinishExternalProcess(pId) {
  document.getElementById(pId).style.display = 'none';
}

// Haskell Luci wrapper. Needs luciConnect.js to be included first.
// Pushes all Luci messages into haskell engine via LikeHS.Either.
// Requires haskell return callback to be specified.
var QLuciHandler = (function() {
    QLuciHandler = function (hsCallback) {
      this.haskellCallback = hsCallback;
      if (arguments.length > 1) {
        this.requiredFields = arguments[1];
      }
    };
    QLuciHandler.prototype = new Luci.ResponseHandler();
    QLuciHandler.prototype.requiredFields = [];
    QLuciHandler.prototype.haskellCallback = function (eitherRez) {
      if (eitherRez.isRight()) {
        logText("Luci answers: " + JSON.stringify(eitherRez.right));
      } else {
        logText("Luci says: " + eitherRez.left);
      }
    }
    QLuciHandler.prototype.onResult = function (message) {
      var m = message.getHeader();
      DEBUG && console.log("QLuciHandler.onResult:");
      DEBUG && console.log(m);
      if (m['result']) {
        if (this.requiredFields.length > 0) {
          for (var i = 0; i < this.requiredFields.length; i++) {
            if (!m['result'].hasOwnProperty(this.requiredFields[i])) {
              return this.haskellCallback(new LikeHS.Either(
                                'Luci answer does not have property "'
                                 + this.requiredFields[i]
                                 + '". MSG: '
                                 + JSON.stringify(m['result']), false));
            }
          }
          if (this.requiredFields.length === 1) {
            return this.haskellCallback(new LikeHS.Either(m['result'][this.requiredFields[0]], true))
          }
        }
        return this.haskellCallback(new LikeHS.Either(m['result'], true));
      } else if (m['error']) {
        if (typeof m['error'] === "string") {
          return this.haskellCallback(new LikeHS.Either('Luci says: ' + m['error'],false));
        } else {
          return this.haskellCallback(new LikeHS.Either('Luci says: ' + JSON.stringify(m['error']),false));
        }
      } else {
        return this.haskellCallback(new LikeHS.Either(
            'Luci answer contains neither a result nor an error. MSG: '
             + JSON.stringify(m), false));
      }
    };
    QLuciHandler.prototype.onProgress = function (message) {
      var header = message.getHeader();
      if (header.percentage > 0) {
        logText("Luci reports progress: " + header.percentage + "%");
      }
    };
    QLuciHandler.prototype.onError = function (message) {
      var header = message.getHeader();
      DEBUG && console.log("QLuciHandler.onError:");
      DEBUG && console.log(header);
      if (typeof header.error === "string") {
        this.haskellCallback(new LikeHS.Either('Luci says: ' + header.error,false));
      } else {
        this.haskellCallback(new LikeHS.Either('Luci says: ' + JSON.stringify(header.error),false));
      }
    };
    return QLuciHandler;
})();
