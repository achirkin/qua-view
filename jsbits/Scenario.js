"use strict";

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



/*
    cycle.js
    2016-05-01
    Public Domain.
    NO WARRANTY EXPRESSED OR IMPLIED. USE AT YOUR OWN RISK.
    This code should be minified before deployment.
    See http://javascript.crockford.com/jsmin.html
    USE YOUR OWN COPY. IT IS EXTREMELY UNWISE TO LOAD CODE FROM SERVERS YOU DO
    NOT CONTROL.
*/

/*jslint eval, for */

/*property
    $ref, decycle, forEach, isArray, keys, length, push, retrocycle, stringify,
    test
*/

if (typeof JSON.decycle !== "function") {
    JSON.decycle = function decycle(object, replacer) {
        "use strict";

// Make a deep copy of an object or array, assuring that there is at most
// one instance of each object or array in the resulting structure. The
// duplicate references (which might be forming cycles) are replaced with
// an object of the form

//      {"$ref": PATH}

// where the PATH is a JSONPath string that locates the first occurance.

// So,

//      var a = [];
//      a[0] = a;
//      return JSON.stringify(JSON.decycle(a));

// produces the string '[{"$ref":"$"}]'.

// If a replacer function is provided, then it will be called for each value.
// A replacer function receives a value and returns a replacement value.

// JSONPath is used to locate the unique object. $ indicates the top level of
// the object or array. [NUMBER] or [STRING] indicates a child element or
// property.

        var objects = [];   // Keep a reference to each unique object or array
        var paths = [];     // Keep the path to each unique object or array

        return (function derez(value, path) {

// The derez function recurses through the object, producing the deep copy.

            var i;          // The loop counter
            var nu;         // The new object or array

// If a replacer function was provided, then call it to get a replacement value.

            if (replacer !== undefined) {
                value = replacer(value);
            }

// typeof null === "object", so go on if this value is really an object but not
// one of the weird builtin objects.

            if (
                typeof value === "object" && value !== null &&
                !(value instanceof Boolean) &&
                !(value instanceof Date) &&
                !(value instanceof Number) &&
                !(value instanceof RegExp) &&
                !(value instanceof String)
            ) {

// If the value is an object or array, look to see if we have already
// encountered it. If so, return a {"$ref":PATH} object. This is a hard
// linear search that will get slower as the number of unique objects grows.
// Someday, this should be replaced with an ES6 WeakMap.

                i = objects.indexOf(value);
                if (i >= 0) {
                    return {$ref: paths[i]};
                }

// Otherwise, accumulate the unique value and its path.

                objects.push(value);
                paths.push(path);

// If it is an array, replicate the array.

                if (Array.isArray(value)) {
                    nu = [];
                    value.forEach(function (element, i) {
                        nu[i] = derez(element, path + "[" + i + "]");
                    });
                } else {

// If it is an object, replicate the object.

                    nu = {};
                    Object.keys(value).forEach(function (name) {
                        nu[name] = derez(
                            value[name],
                            path + "[" + JSON.stringify(name) + "]"
                        );
                    });
                }
                return nu;
            }
            return value;
        }(object, "$"));
    };
}


if (typeof JSON.retrocycle !== "function") {
    JSON.retrocycle = function retrocycle($) {
        "use strict";

// Restore an object that was reduced by decycle. Members whose values are
// objects of the form
//      {$ref: PATH}
// are replaced with references to the value found by the PATH. This will
// restore cycles. The object will be mutated.

// The eval function is used to locate the values described by a PATH. The
// root object is kept in a $ variable. A regular expression is used to
// assure that the PATH is extremely well formed. The regexp contains nested
// * quantifiers. That has been known to have extremely bad performance
// problems on some browsers for very long strings. A PATH is expected to be
// reasonably short. A PATH is allowed to belong to a very restricted subset of
// Goessner's JSONPath.

// So,
//      var s = '[{"$ref":"$"}]';
//      return JSON.retrocycle(JSON.parse(s));
// produces an array containing a single element which is the array itself.

        var px = /^\$(?:\[(?:\d+|\"(?:[^\\\"\u0000-\u001f]|\\([\\\"\/bfnrt]|u[0-9a-zA-Z]{4}))*\")\])*$/;

        (function rez(value) {

// The rez function walks recursively through the object looking for $ref
// properties. When it finds one that has a value that is a path, then it
// replaces the $ref object with a reference to the value that is found by
// the path.

            if (value && typeof value === "object") {
                if (Array.isArray(value)) {
                    value.forEach(function (element, i) {
                        if (typeof element === "object" && element !== null) {
                            var path = element.$ref;
                            if (typeof path === "string" && px.test(path)) {
                                value[i] = eval(path);
                            } else {
                                rez(element);
                            }
                        }
                    });
                } else {
                    Object.keys(value).forEach(function (name) {
                        var item = value[name];
                        if (typeof item === "object" && item !== null) {
                            var path = item.$ref;
                            if (typeof path === "string" && px.test(path)) {
                                value[name] = eval(path);
                            } else {
                                rez(item);
                            }
                        }
                    });
                }
            }
        }($));
        return $;
    };
}
