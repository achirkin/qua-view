# minify the code
echo "var global = this;" > web/qua-view.js
echo "function runQuaView(){" >> web/qua-view.js
closure-compiler --warning_level=QUIET\
                 --language_in=ECMASCRIPT5\
                 --compilation_level=ADVANCED_OPTIMIZATIONS\
                 --define='DEBUG=false'\
                 --externs= $(stack path --dist-dir)/build/qua-view/qua-view.jsexe/all.js.externs\
 $(stack path --dist-dir)/build/qua-view/qua-view.jsexe/all.js >> web/qua-view.js
echo "}" >> web/qua-view.js
echo "window.onload = runQuaView.bind(this);" >> web/qua-view.js

# copy qua-view.js to qua-server if possible
if [ -d "../qua-server/static/js" ] ; then
    cp web/qua-view.js ../qua-server/static/js/qua-view.js
fi
