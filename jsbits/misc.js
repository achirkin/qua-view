// Log messages to in-app text console
function logText(text) {
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

/**
  * Create rectangular preview out of the game canvas
 **/
function h$makePreview(sCanvas) {
  var dCanvas = document.createElement("canvas"),
      dCtx = dCanvas.getContext("2d"),
      sw = sCanvas.width,
      sh = sCanvas.height,
      ss = Math.min(sw,sh),
      sx = (sw - ss)/2,
      sy = (sh - ss)/2;
  dCanvas.width = 800;
  dCanvas.height = 800;
  dCtx.drawImage(sCanvas, sx, sy, ss, ss, 0, 0, 800, 800);
  return dCanvas.toDataURL("image/png", 1);
}
