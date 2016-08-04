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
