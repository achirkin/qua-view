"use strict";


/**
 * Normalize all values in an array to be a four-element vector in range [0..1]
 * Substitue a fiven number without any processing instead of null.
 *
 * @param sourceArray -- array of numbers or strings
 * @param nullSub
 * @returns {Array}
 */
function gm$smartNormalizeValues(sourceArray, nullSub) {
    var sorted = sourceArray.slice().sort(),
        uniques = sorted.map(function(e,i){if(e==sorted[i+1]){return null;}else{return e;}}).filter(function(e){return e!=null;}),
        categorical = uniques.some(function(e){return e != null && e.constructor == String});
    if(categorical){
        if (uniques.every(function(s) {return (/^(#[A-Fa-f0-9]{6})$/).test(s);})) {
          return [sourceArray.map(function(e){
               if (e) {
                 var x = parseInt(e.substr(1), 16);
                 return [((x & 0xff0000) >> 16) / 255.0, ((x & 0x00ff00) >> 8) / 255.0, (x & 0x0000ff) / 255.0, 1];
               } else {
                 return [100/255.0, 100/255.0, 100/255.0, 200/255.0];
               }
            }), false];
        } else {
          return [gm$normalizeValues(sourceArray.map(function(e){ return e != null ? uniques.indexOf(e) : null;}), nullSub), true];
        }
    } else {
        return [gm$normalizeValues(sourceArray, nullSub), true];
    }
}

/**
 * Fetch RGBA color from a city object if available
 * @param obj -- City Object
 * @return Array - four-element normalized vector.
 */
function gm$smartCityObjectColor(obj) {
    if(obj.hasOwnProperty('properties') && 
            obj['properties'].hasOwnProperty('viewColor')) {
        var color = obj['properties']['viewColor'];
        return gm$smartConvertHexToRgba(color);
    }
}

/**
 * Convert an input color to RGBA if it satisfies Hex format.
 * @param color 
 * @return Array - four-element normalized vector.
 */
function gm$smartConvertHexToRgba(color) {
    if (color.match(/^(#[A-Fa-f0-9]{6})$/)) {
        var x = parseInt(color.substr(1), 16);
        return [((x & 0xff0000) >> 16) / 255.0, 
                ((x & 0x00ff00) >> 8) / 255.0, 
                (x & 0x0000ff) / 255.0, 1];
    }
}

/**
 * Normalize all values in an array to be a four-element vector in range [0..1]
 * Substitue a fiven number without any processing instead of null.
 *
 * @param sourceArray -- array of numbers
 * @param nullSub
 * @returns {Array}
 */
function gm$normalizeValues(sourceArray, nullSub) {
    var bs = Array.prototype.reduce.call(sourceArray, function(a,x) {
                return x != null ? [Math.min(a[0],x),Math.max(a[1],x)] : a;
            }, [Infinity,-Infinity]),
        xspan = Math.max(bs[1] - bs[0], 0.000001),
        f = function(e) {return e != null ? Math.min(1,Math.max(0,(e - bs[0]) / xspan)) : null;},
        t = 0;
    return Array.prototype.map.call(sourceArray, function(n) {
            if(n != null) {
                t = f(n);
                return [t, t, t, t];
            } else {
                return [nullSub, nullSub, nullSub, nullSub];
            }
        });
}

/**
 *
 * @param bArray -- collection of buildings
 * @param updateArray -- collection of buildings to update
 * @param deleteArray -- array of building ids to delete
 * @returns {Array.<*>} -- updated collection of buildings
 */
function gm$smartUpdateBArray(bArray, updateArray, deleteArray) {
    var i,t,emptys = [],rez1 = bArray.map(function (b,j) {
        for(i = 0; i < updateArray.length; i++){
            if (updateArray[i] && b['properties']['geomID'] === updateArray[i]['properties']['geomID']) {
                t = updateArray[i];
                updateArray[i] = null;
                return t;
            }
        }
        for(i = 0; i < deleteArray.length; i++) {
            if (b['properties']['geomID'] === deleteArray[i]) {
                emptys.push(j);
                return null;
            }
        }
        return b;
    });
    updateArray.forEach(function(e){
       if(e != null){
           if(emptys.length > 0){
               rez1[emptys.pop()] = e;
           } else {
               rez1.push(e);
           }
       }
    });
    return rez1.filter(function(e){return e != null;});
}

/**
 *
 * @param bArray -- collection of buildings
 * @param values -- collection of feature properties
 * @returns {Array.<*>} -- updated collection of buildings
 */
function gm$updateProps(bArray, values) {
    return bArray.map(function (b,i) {
        b['properties']['value'] = values[i];
        return b;
    });
}

/**
 * Parse a geometry input.
 *
 * Returns a feature collection, and latitude, longitude, altitude, srid if available.
 *
 * @param gi - Geometry Input JSON
 * @returns {[featureCollection:FeatureCollection,errors:string,originLatLonAlt,srid,blockColor,staticColor,lineColor]}
 */
function gm$smartProcessGeometryInput(gi, defVec) {
    'use strict';
    if (!gi) {
        return [null,["Scenario is null."]];
    }
    var fc = gi['geometry']
    var lat, lon, alt, srid;
    var blockColor, staticColor, lineColor;
    if (gi['lat'] && gi['lat'].constructor === Number &&
            gi['lon'] && gi['lon'].constructor === Number) {
        lat = gi['lat'];
        lon = gi['lon'];
        alt = 0;
        if (gi['alt'] && gi['alt'].constructor ===  Number) {
            alt = gi['alt'];
        }
    }
    if (gi['srid'] && gi['srid'].constructor === Number) {
        srid = gi['srid']
    }
    if (gi.hasOwnProperty('properties')) {
        blockColor = gi['properties']['defaultBlockColor'];
        staticColor = gi['properties']['defaultStaticColor'];
        lineColor = gi['properties']['defaultLineColor'];
    }
    return [fc,[],[lat,lon,alt],srid,blockColor,staticColor,lineColor];
}

/**
 * Parse a feature collection.
 *
 * Returns nicely sorted points, lines, surfaces + deletes and errors.
 * Add geomID to all features that miss it.
 *
 * @param fc - FeatureCollection
 * @returns {[points:Feature,lines:Feature,surfaces:Feature,deletes:Number(geomID),errors:string,cmin,cmax,dims]}
 */

function gm$smartProcessFeatureCollection(fc, coorSys, defVec, maxGeomId) {
    'use strict';
    if (!fc) {
        return [[],[],[],[],["FeatureCollection is null."]];
    }
    if (fc['type'] !== "FeatureCollection")  {
        return [[],[],[],[],["No valid 'obj.type = \"FeatureCollection\"'"]];
    }
    if (!fc['features'] || fc['features'].constructor !== Array)  {
        return [[],[],[],[],["No valid 'obj.features':array"]];
    }
    // so now we have a more-or-less valid feature collection
    var points = [],lines = [],surfaces = [],deletes = [],errors = [],
        f, cmin = [], cmax = [], dims = 0, i;
    fc['features'].forEach(function(feature, n) {
        try{
            f = gm$smartProcessFeature(feature, defVec);
            switch (f['type'] ) {
                case "Delete":
                    f['properties']['deleted_geomIDs'].forEach(function(gId){
                        maxGeomId = Math.max(gId, maxGeomId);
                        deletes.push(gId);
                    });
                    break;
                case "Error":
                    errors.push("(" + n + ") " + f['properties']['error']);
                    break;
                case "Feature":
                    if (f['properties']['geomID']) {
                        maxGeomId = Math.max(f['properties']['geomID'], maxGeomId);
                    }
                    if (f['geometry']['type'] == "Polygon" || f['geometry']['type'] == "MultiPolygon")
                    {
                      for(i = 0; i < Math.min(dims, f.dim); i++) {
                          cmin[i] = Math.min(cmin[i],f.min[i]);
                          cmax[i] = Math.max(cmax[i],f.max[i]);
                      }
                      for(i = dims; i < f.dim; i++) {
                          cmin[i] = f.min[i];
                          cmax[i] = f.max[i];
                      }
                      dims = Math.max(dims,f.dim);
                    }
                    switch (f['geometry']['type']) {
                        case "Point":
                        case "MultiPoint":
                            points.push(f);
                            break;
                        case "LineString":
                        case "MultiLineString":
                            lines.push(f);
                            break;
                        case "Polygon":
                        case "MultiPolygon":
                            surfaces.push(f);
                            break;
                        default:
                            return errors.push("(" + n + ") Unknown feature geometry type: " + f['geometry']['type']);
                    }
                    break;
                default:
                    errors.push("(" + n + ") Unknown feature type: " + f['type']);
            }
        } catch (ex) {
            errors.push("(" + n + ") JavaScript error: " + JSON.stringify(ex));
        }
    });
    var addGeomID = function(f) {
        if (!f['properties'].hasOwnProperty('geomID') || f['properties']['geomID'] < 0) {
            f['properties']['geomID'] = ++maxGeomId;
        }
    };
    points.forEach(addGeomID);
    lines.forEach(addGeomID);
    surfaces.forEach(addGeomID);

    var transform = false;
    if (coorSys === "WGS84") {
        transform = true;
    } else if (coorSys === "Metric") {
        transform = false;
    } else  {
        if (cmin.length >= 2 && cmax.length >= 2 && 
                cmin[0] > -360 && cmax[0] < 360 && 
                cmin[1] > -180 && cmax[1] < 180) {
            var xbound = cmax[0] - cmin[0], ybound = cmax[1] - cmin[1];
            if ((xbound < 1 && ybound < 1 && fc['features'].length < 10) ||
                    (xbound < 3 && ybound < 3 && fc['features'].length >= 10 && fc['features'].length < 100) ||
                    (xbound < 5 && ybound < 5) && fc['features'].length >= 100){
                if(cmin.length >= 3 && cmax.length >= 3) {
                    var zbound = cmax[2] - cmin[2];
                    if(zbound/(xbound+ybound) > 100) {
                        transform = true;
                    }
                }
            }
        }
    }
    // transform everything from WGS84 to a metric reference system if needed
    // when there is no lat+lon+alt or srid specified
    if(transform) {
       var center = [(cmax[0] + cmin[0])/2, (cmax[1] + cmin[1])/2]
         , transformFunc = gm$createWGS84toUTMTransform(center[0], center[1]);
       return [ gm$mapPoints(transformFunc, points)
              , gm$mapPoints(transformFunc, lines)
              , gm$mapPoints(transformFunc, surfaces)
              , deletes
              , errors
              , gm$resizeX(defVec, transformFunc(cmin))
              , gm$resizeX(defVec, transformFunc(cmax))
              , dims
              , center
              ];
    } else {
      return [points,lines,surfaces,deletes,errors,gm$resizeX(defVec, cmin),gm$resizeX(defVec, cmax),dims, null];
    }
}

function gm$mapPoints(f, x) {
    'use strict';
    if (!x) {
      return x;
    }
    if (x.constructor === Array && x.length > 0) {
      if(x[0].constructor === Number) {
        return f(x);
      } else {
        return x.map(function(e){return gm$mapPoints(f, e);});
      }
    }
    if (x['geometry'] && x['geometry']['coordinates']) {
      x['geometry']['coordinates'] = gm$mapPoints(f, x['geometry']['coordinates']);
      if (x['max'])
        x['max'] = gm$mapPoints(f, x['max']);
      if (x['min'])
      x['min'] = gm$mapPoints(f, x['min']);
      return x;
    }
    return x;
}


/** Process geometry in a smart way to make sure it is valid.
 *
 *  Return feature types are a little bit different from standard GeoJSON; possible values:
 *  * Feature - standard type, geometries are (MultiPolygon, MultiLineString, MultiPoint);
 *  * Error - there was some error while processing; then has an explanatory property "properties.error":text;
 *  * Delete - empty geometry in original feature means delete object; then must have "properties.geomID":number to delete.
 *
 * @param feature - GeoJSON Feature
 * @returns {rez} - updated augmented GeoJSON Feature
 */
function gm$smartProcessFeature(feature, defVec) {
    'use strict';
    if (!feature) {
        return gm$createErrorFeature("Feature is null.");
    }
    if (feature['type'] !== "Feature") {
        return gm$createErrorFeature("No valid 'obj.type = \"Feature\"'");
    }
    if (!feature['geometry']) { // parse Delete Feature
        if(!feature['properties'] || !feature['properties']['deleted_geomIDs'] || feature['properties']['deleted_geomIDs'].constructor !== Array){
            return gm$createErrorFeature("No valid 'obj.properties.deleted_geomIDs':array");
        }
        return gm$createDeleteFeature(feature['properties']['deleted_geomIDs']);
    }
    if (!feature['geometry']['type'] || feature['geometry']['type'].constructor !== String) {
        return gm$createErrorFeature("No valid 'obj.geometry.type':string");
    }
    // parse geometry and check it for errors
    var geominfo = gm$smartProcessGeometry(feature['geometry']['coordinates'], defVec);
    if (geominfo.error) {
        return gm$createErrorFeature("Invalid feature geometry: " + geominfo.error);
    }
    // by this time we have a proper feature with geometry and some info about coordinates
    // let's test all advances properties!
    switch (feature['geometry']['type']) {
        case "Point":
            if (geominfo.nesting !== 1) {
                return gm$createErrorFeature("Point feature must have nesting 1, but has " + geominfo.nesting);
            }
            break;
        case "MultiPoint":
            if (geominfo.nesting !== 2) {
                return  gm$createErrorFeature("MultiPoint feature must have nesting 2, but has " + geominfo.nesting);
            }
            break;
        case "LineString":
            if (geominfo.nesting !== 2) {
                return gm$createErrorFeature("LineString feature must have nesting 2, but has " + geominfo.nesting);
            }
            if (geominfo.dim[1] < 2) {
                return gm$createErrorFeature("LineString feature must have at least 2 points " + geominfo.dim[1]);
            }
            break;
        case "MultiLineString":
            if (geominfo.nesting !== 3) {
                return  gm$createErrorFeature("MultiLineString feature must have nesting 3, but has " + geominfo.nesting);
            }
            if (geominfo.dim[1] < 2) {
                return gm$createErrorFeature("MultiLineString feature must have at least 2 points inside each LineString, but has " + geominfo.dim[1]);
            }
            break;
        case "Polygon":
            if (geominfo.nesting !== 3) {
                return gm$createErrorFeature("Polygon feature must have nesting 3, but has " + geominfo.nesting);
            }
            if (geominfo.dim[1] < 4) {
                return gm$createErrorFeature("Polygon feature must have at least 4 points inside each LinearRing, but has " + geominfo.dim[1]);
            }
            break;
        case "MultiPolygon":
            if (geominfo.nesting !== 4) {
                return  gm$createErrorFeature("MultiPolygon feature must have nesting 4, but has " + geominfo.nesting);
            }
            if (geominfo.dim[1] < 4) {
                return gm$createErrorFeature("Polygon feature must have at least 4 points inside each LinearRing, but has " + geominfo.dim[1]);
            }
            break;
        default:
            return gm$createErrorFeature("Unknown feature geometry type: " + feature['geometry']['type']);
    }

    var rez = {};
    rez['properties'] = feature['properties'];
    rez['type'] = "Feature";
    rez['geometry'] = {};
    rez['geometry']['coordinates'] = feature['geometry']['coordinates']; // feature['geometry']['coordinates']; // geominfo.val;
    rez['geometry']['type'] = feature['geometry']['type'];
    rez.min = geominfo.min;
    rez.max = geominfo.max;
    rez.dim = geominfo.dim[0];
    rez.nesting = geominfo.nesting;
    return rez;
}

function gm$createErrorFeature(err) {
    'use strict';
    var rez = {};
    rez['type'] = "Error";
    rez['properties'] = {};
    rez['properties']['error'] = err;
    return rez;
}

function gm$createDeleteFeature(geomIDs) {
    'use strict';
    var rez = {};
    rez['type'] = "Delete";
    rez['properties'] = {};
    rez['properties']['deleted_geomIDs'] = geomIDs;
    return rez;
}

/** Checks validity of a geometry array;
 * evaluates bounding box (min, max);
 * evaluates nesting level of an array (nesting);
 * evaluates dimensionality of an array (dim).
 *
 * @param x - array with unknown nesting
 * @returns {*}
 */
function gm$smartProcessGeometry(x, defVec) {
    'use strict';
    if (!x || x.constructor !== Array || x.length === 0) {
        return { error : "Invalid coordinates (level 0)" };
    } else {
        return gm$smartProcessGeometryInner(x, 1, defVec);
    }
}

function gm$smartProcessGeometryInner(x, i, defVec) {
    'use strict';
    if ( x[0].constructor === Number ) {
        return {
            nesting: i,
            dim: [x.length],
            min: x,
            max: x,
            val: gm$resizeX(defVec,x)
        };
    }
    if ( x[0].constructor !== Array ) {
        return { error : "Invalid coordinates (level " + i + "): wrong element data type '" + typeof x[0] + "'." };
    }
    if ( x[0].length === 0 ) {
        return { error : "Invalid coordinates (level " + i + "): zero-length array." };
    }
    var re, z, z0;
    z0 = gm$smartProcessGeometryInner(x[0], i+1, defVec);
    z0.val = [z0.val];
    z = x.slice(1).reduce(function(r,e) {
        // at this moment we assume the elements are non-empty arrays
        if (r.error) {
            return r;
        }
        re = gm$smartProcessGeometryInner(e, i+1, defVec);
        if (re.error) {
            return re;
        }
        if (re.dim[0] !== r.dim[0]) {
            return { error : "Invalid coordinates (level " + i + "): dimension mismatch." };
        }
        if ( re.nesting !== r.nesting) {
            return { error : "Invalid coordinates (level " + i + "): nesting mismatch." };
        }
        // now both r and re are valid result objects
        return {
            nesting: r.nesting,
            dim: r.dim.map(function(e,i){return Math.min(e,re.dim[i]);}),
            min: r.min.map(function(e,i){return Math.min(e,re.min[i]);}),
            max: r.max.map(function(e,i){return Math.max(e,re.max[i]);}),
            val: r.val.concat([re.val])
        }
    }, z0);
    if (!z.error) {
        z.dim.push(x.length);
    }
    return z;
}


// cloning

function gm$cloneCityObject(obj) {
    'use strict';
    if (!obj) {return null;}
    var rez = {};
    rez['properties'] = JSON.parse(JSON.stringify(obj['properties']));
    rez['pointData'] = obj['pointData']; // pointData is not cloned! just copy ref
    rez['geometry'] = gm$cloneGeometry(obj['geometry']);
    return rez;
}


function gm$cloneNestedArray(x) {
    'use strict';
    if (!x || x.constructor !== Array || x.length === 0) { return []; }
    if (x[0].constructor === Number) { return x.slice(); }
    if (x[0].constructor === Array)  { return x.map(gm$cloneNestedArray); }
    else                             { return []; }
}


function gm$cloneGeometry(geom) {
    'use strict';
    if (!geom) {return null;}
    var rez = {};
    rez['type'] = geom['type'];
    rez['coordinates'] = gm$cloneNestedArray(geom['coordinates']);
    return rez;
}


// bounding

function gm$boundX(r,x) {
    'use strict';
    if(!r){
        return x.map(function(e) {
            return [e,e];
        });
    }
    return r.map(function(e,i) {
        return [Math.min(e[0],x[i]), Math.max(e[1],x[i])];
    });
}

function gm$boundCombine(r,q) {
    'use strict';
    if(!r && !q){ return null; }
    if(!r){ return q; }
    if(!q){ return r; }
    return r.map(function(e,i) {
        return [Math.min(e[0],q[i][0]), Math.max(e[1],q[i][1])];
    });
}

function gm$boundNestedArrayInner(x) {
    'use strict';
    return x.reduce(function(r,e) {
        if(e.constructor !== Array || e.length === 0) {return null;}
        if(e[0].constructor === Number) {
            return gm$boundX(r,e);
        } else {
            return gm$boundCombine(r, gm$boundNestedArrayInner(e));
        }
    }, null);
}

function gm$boundNestedArray(x) {
    'use strict';
    if (!x || x.constructor !== Array || x.length === 0) {
        return null;
    } else {
        var rez = gm$boundNestedArrayInner(x);
        if(!rez || rez.constructor !== Array) {return null;}
        return [rez.map(function(e){return e[0];}), rez.map(function(e){return e[1];})];
    }
}

// resizing


function gm$resizeX(r,x) {
    'use strict';
    if(r.length === x.length){return x;}
    if(r.length < x.length){return x.slice(0,r.length);}
    return x.concat(r.slice(x.length));
}

function gm$resizeNestedArrayInner(r,x) {
    'use strict';
    return x.map(function(e) {
        if(e.constructor !== Array) {return null;}
        if(e[0].constructor === Number) {
            return gm$resizeX(r,e);
        } else {
            return gm$resizeNestedArrayInner(r,e);
        }
    });
}

function gm$resizeNestedArray(r,x) {
    'use strict';
    if (!x || x.constructor !== Array || x.length === 0) {
        return null;
    }
    if (x[0].constructor === Number){return gm$resizeX(r,x);}
    return gm$resizeNestedArrayInner(r, x);
}


// guess wgs84 and transform it to metric


// got this formula here http://www.linz.govt.nz/data/geodetic-services/coordinate-conversion/projection-conversions/transverse-mercator-transformation-formulae
function gm$createWGS84toUTMTransform(lon0, lat0) {
 'use strict'
  var a = 6378137 // equatorial radius - Semi-major axis of reference ellipsoid
    , f = 1 / 298.257223563 // Ellipsoidal flattening
    , k = 1  // Central meridian scale factor
    , b = a * (1 - f)
    , e2 = (2 - f) * f
    , e4 = e2*e2
    , e6 = e4*e2
    , A0 = 1 - e2*0.25 - e4*3/64 - e6*5/256
    , A2 = 0.375 * (e2 + e4/4 + e6*15/128)
    , A4 = 15/256 * ( e4 + 0.75*e6)
    , A6 = e6 * 35/3072
    , xr0 = lon0 * Math.PI / 180
    , yr0 = lat0 * Math.PI / 180
    , fm = function(y) { return a * ( A0 * y - A2 * Math.sin(2 * y) + A4 * Math.sin(4 * y) - A6 * Math.sin(6 * y) ); }
    , m0 = fm(yr0);
  return function(xs) {
    var xr = xs[0] * Math.PI / 180
      , yr = xs[1] * Math.PI / 180
      , siny = Math.sin(yr)
      , cosy = Math.cos(yr)
      , p = a * (1 - e2) / Math.pow(1 - e2 * siny*siny, 1.5)
      , v = a / Math.sqrt(1 - e2 * siny * siny)
      , psi = v / p
      , t = Math.tan(yr)
      , w = xr - xr0
      , w2 = w * w
      , w4 = w2*w2
      , w6 = w4*w2
      , w8 = w4*w4;
    return [ k * v * w * cosy * ( 1
                                + w2 / 6 * cosy * cosy * (psi - t*t)
                                + w4 / 120 * Math.pow(cosy, 4) * ( 4*psi*psi*psi + ( 1 - 6*t*t ) + psi*psi*(1+8*t*t) - psi*2*t*t + Math.pow(t,4) )
                                + w6 / 5040 * Math.pow(cosy,6) * ( 61 - 479*t*t + 179 * Math.pow(t,4) - Math.pow(t,6) )
                                )
           , k * ( fm(yr) - m0
                 + w2 * 0.5 * v * siny * cosy
                 + w4/24 * v * siny * cosy*cosy*cosy * ( 4*psi*psi + psi - t*t)
                 + w6/720* v * siny * Math.pow(cosy, 5) * ( 8 * Math.pow(psi, 4) * (11 - 24*t*t) - 28*Math.pow(psi, 3)*(1-6*t*t) + psi*psi*(1 - 32*t*t) - psi*2*t*t + Math.pow(t,4) )
                 + w8/40320*v* siny * Math.pow(cosy, 7) * (1285 - 3111 * t*t + 543 * Math.pow(t,4) - Math.pow(t,6) )
                 )
           ].concat(xs.splice(2));
  };
}




