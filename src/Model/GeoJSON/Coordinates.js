

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


// Get a flat list of 2D points corresponding to every Geometry in the GeoJSON file
function h$geojson_getObjectCentres(x) {
  "use strict"
  if (Array.isArray(x)) {
    // if this is a coordinate, just return it (the only place where result is not wrapped)
    if (typeof x[0] === "number" && typeof x[1] === "number") {
      return [x[0], x[1]];
    } else if (Array.isArray(x[0])) {
      // flatten one layer of multi-layered array
      if (Array.isArray(x[0][0])) {
        return h$geojson_getObjectCentres([].concat.apply([],x));
      }
      // compute avgs and wrap them into one more layer of a list
      var sum = x.reduce(
                function(a,e) {
                  var r = h$geojson_getObjectCentres(e);
                  if(a == null) {
                    return r;
                  } else {
                    return [a[0] + r[0], a[1] + r[1]];
                  }
                }
                , null
             ),
          n = x.length;
      return (n == 0 || sum == null) ? [] : [[sum[0]/n, sum[1]/n]];
    } else { // map join results from all features / geometries
        return [].concat.apply([],x.map(h$geojson_getObjectCentres));
    }
  } else if (x.hasOwnProperty('features')) {
      return h$geojson_getObjectCentres(x['features']);
  } else if (x.hasOwnProperty('geometry')) {
      return h$geojson_getObjectCentres(x['geometry']);
  } else if (x.hasOwnProperty('geometries')) {
      return h$geojson_getObjectCentres(x['geometries']);
  } else if (x.hasOwnProperty('coordinates')) {
      return h$geojson_getObjectCentres(x['coordinates']);
  } else {
      return [];
  }
}
