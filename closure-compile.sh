closure-compiler --warning_level=QUIET\
                 --language_in=ECMASCRIPT5\
                 --compilation_level=ADVANCED_OPTIMIZATIONS\
                 --externs=web/numeric.min.js\
        dist/build/qua-view/qua-view.jsexe/all.js\
        web/misc.js web/luciConnect.js\
        web/faultylabs.md5.js > web/qua-view.js
