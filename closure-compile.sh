# minify the code
closure-compiler --warning_level=QUIET\
                 --language_in=ECMASCRIPT5\
                 --compilation_level=ADVANCED_OPTIMIZATIONS\
                 --define='DEBUG=false'\
                 --externs=web/numeric.min.js\
                 --externs=jsbits/qua-server-externs.js\
 dist/build/qua-view/qua-view.jsexe/all.js > web/qua-view.js

# copy qua-view.js to qua-server if possible
if [ -d "../qua-server/static/js" ] ; then
    cp web/qua-view.js ../qua-server/static/js/qua-view.js
fi
