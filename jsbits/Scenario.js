"use strict";

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
 * Parse a feature collection.
 *
 * Returns nicely sorted points, lines, surfaces + deletes and errors.
 * Add geomID to all features that miss it.
 *
 * @param fc - FeatureCollection
 * @returns {[points:Feature,lines:Feature,surfaces:Feature,deletes:Number(geomID),errors:string,cmin,cmax,dims]}
 */
function gm$smartProcessFeatureCollection(fc, defVec, maxGeomId) {
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
                    for(i = 0; i < Math.min(dims, f.dim); i++) {
                        cmin[i] = Math.min(cmin[i],f.min[i]);
                        cmax[i] = Math.max(cmax[i],f.max[i]);
                    }
                    for(i = dims; i < f.dim; i++) {
                        cmin[i] = f.min[i];
                        cmax[i] = f.max[i];
                    }
                    dims = Math.max(dims,f.dim);

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
    return [points,lines,surfaces,deletes,errors,gm$resizeX(defVec, cmin),gm$resizeX(defVec, cmax),dims];
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
