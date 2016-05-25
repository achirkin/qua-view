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

var QLuciHandler = (function() {
    QLuciHandler = function () {
        if (arguments.length > 0) {
            this.onResult = arguments[0];
        }
    };
    QLuciHandler.prototype = new Luci.ResponseHandler();
    QLuciHandler.prototype.onResult = function (message) {
        logText("Luci on result: " + message.getHeader());
    };
    QLuciHandler.prototype.onProgress = function (message) {
        var header = message.getHeader();
        logText("Luci in progress: " + header.percentage + "% on " + header.callID);
    };
    QLuciHandler.prototype.onError = function (message) {
        var header = message.getHeader();
        logText("Luci error: " + header.error);
    };
    return QLuciHandler;
})();

