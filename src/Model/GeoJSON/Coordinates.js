

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

// Parse MultiPoint or LineString
// Returns: flattened array, its length, and is every point padded with zeros
function h$geojson_parsePointSeq(arr) {
    var x, ans = arr.reduce(
        function(a,e){
           x = [e[0]||0,e[1]||0,e[2]||0,1];
           return [a[0].concat(x), a[1] && e.length < 3 ];
        }, [[], true]
      );
    return [ans[0], arr.length, ans[1]];
}

// Parse MultiLineString and pack it into a single ArrayBuffer.
// then return lists of data frames, their sizes, and is every point padded with zeros
function h$geojson_parseMultiLineString(arr) {
    var x, df, dfs = [], points = [], sizes = [], padded = true, shifts = [0];
    for(var i = 0; i < arr.length; i++) {
      x = h$geojson_parsePointSeq(arr[i]);
      points = points.concat(x[0]);
      sizes.push(x[1]);
      padded = padded && x[2];
      shifts.push(points.length);
    }
    df = new Float32Array(points);
    for(var i = 0; i < shifts.length - 1; i++) {
      x = df.subarray(shifts[i],shifts[i+1]);
      dfs.push(x);
    }
    return [dfs, sizes, padded];
}

// This function parses an array of nesting level 2
// and returns a triple
// [flat coordinates, number of points, is every point padded with zeroes]
function h$geojson_parseLinearRing(arr) {
    var x, points = [], size = arr.length - 1, padded = true;
    for(var i = 0; i < size; i++) {
        points.push(arr[i][0]||0);
        points.push(arr[i][1]||0);
        points.push(arr[i][2]||0);
        points.push(1);
        points.push(0);
        points.push(0);
        points.push(1);
        points.push(1);
        padded = padded && arr[i].length < 3;
    }
    return [points, size, padded];
}

// This function parses an array of nesting level 3
// and returns a list
//  [ array (flat coordinates)
//  , number of points
//  , hole indices
//  , is every point padded with zeroes
//  ]
function h$geojson_parsePolygon(arr) {
    var points = [], size = 0, padded = true, holes = [0], x;
    for(var i = 0; i < arr.length; i++) {
        x = h$geojson_parseLinearRing(arr[i]);
        points = points.concat(x[0]);
        size += x[1];
        holes.push(size);
        padded = padded && x[2];
    }
    holes.pop();
    return [points, size, holes.slice(1), padded];
}

// This function parses an array of nesting level 4
function h$geojson_parseMultiPolygon(arr) {
    var points = [], sizes = [], padded = true, holes = [], x, df, dfs = [], shifts = [0];
    for(var i = 0; i < arr.length; i++) {
      x = h$geojson_parsePolygon(arr[i]);
      points = points.concat(x[0]);
      sizes.push(x[1]);
      holes.push(x[2]);
      padded = padded && x[3];
      shifts.push(points.length);
    }
    df = new Float32Array(points);
    for(var i = 0; i < shifts.length - 1; i++) {
      x = df.subarray(shifts[i],shifts[i+1]);
      dfs.push(x);
    }
    return [dfs, sizes, holes, padded];
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
