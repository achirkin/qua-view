#!/bin/sh
BUILD_FOLDER=$(stack path --dist-dir)/build/qua-view/qua-view.jsexe
BUILD_FOLDER_GLOADER=$(stack path --dist-dir)/build/qua-worker-loadgeometry/qua-worker-loadgeometry.jsexe

# minify the code of qua-view
cat << EOF > web/qua-view.js
var global = this;
function runQuaView(){
"use strict"
EOF
closure-compiler --warning_level=QUIET \
                 --language_in=ECMASCRIPT5 \
                 --compilation_level=ADVANCED_OPTIMIZATIONS \
                 --externs=$BUILD_FOLDER/all.js.externs \
                 $BUILD_FOLDER/all.js >> web/qua-view.js
cat << EOF >> web/qua-view.js
}
if (document.readyState === 'complete') {
  runQuaView.bind(global)();
} else {
  window.onload = runQuaView.bind(global);
}
EOF
# minify the code of qua-worker-loadgeometry
closure-compiler --warning_level=QUIET \
                 --language_in=ECMASCRIPT5 \
                 --compilation_level=ADVANCED_OPTIMIZATIONS \
                 --externs=$BUILD_FOLDER_GLOADER/all.js.externs \
                 $BUILD_FOLDER_GLOADER/all.js > web/qua-worker-loadgeometry.js

# copy qua-view.js to qua-server if possible
if [ -d "../qua-server/static/js" ] ; then
    cp web/qua-view.js ../qua-server/static/js/qua-view.js
fi
# copy qua-view.css to qua-server if possible
if [ -d "../qua-server/static/css" ] ; then
    cp web/qua-view.css ../qua-server/static/css/qua-view.css
fi
# copy qua-worker-loadgeometry.js to qua-server if possible
if [ -d "../qua-server/static/js" ] ; then
    cp web/qua-worker-loadgeometry.js ../qua-server/static/js/qua-worker-loadgeometry.js
fi
