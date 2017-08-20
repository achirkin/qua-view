

function h$geojson_parseVec3(x) {
    var r = new Float32Array(3);
    r[0] = x[0]||0;
    r[1] = x[1]||0;
    r[2] = x[2]||0;
    return [r, x.length < 3];
}

function h$geojson_parseVec4(x) {
    var r = new Float32Array(4);
    r[0] = x[0]||0;
    r[1] = x[1]||0;
    r[2] = x[2]||0;
    r[3] = 1;
    return [r, x.length < 3];
}

function h$geojson_nestingLvl(x) {
    var n = 0, a = x;
    while(Array.isArray(a)) {
      a = a[0]; n++;
    }
    return n;
}

function h$geojson_parsePointSeq(arr) {
    var x, ans = arr.reduce(
        function(a,e){
           x = [e[0]||0,e[1]||0,e[2]||0,1];
           return [a[0].concat(x), a[1] || e.length < 3 ];
        }, [[], false]
      );

    return [new Float32Array(ans[0]), arr.length, ans[1]];
}

function h$geojson_parseLinearRing(arr) {
    var x, ans = arr.slice(0, arr.length - 1).reduce(
        function(a,e){
           x = [e[0]||0,e[1]||0,e[2]||0,1];
           return [a[0].concat(x), a[1] || e.length < 3 ];
        }, [[], false]
      );
    return [new Float32Array(ans[0]), arr.length, ans[1]];
}
