"use strict";

function gm$mean(arr) {
    'use strict';
    var n = arr.length;
    if(n === 0){return [];}
    var m = arr[0].length;
    var rez = arr.reduce(function (r, v) {
            return r.map(function (e, i) {return e + v[i];});
        }
        , Array.apply(null, Array(m)).map(Number.prototype.valueOf,0)
        );
    return rez.map(function (e) {return e/n;});
}

function gm$cov(arr) {
    'use strict';
    var n = arr.length;
    if(n === 0){return [];}
    var av = gm$mean(arr);
    var m = av.length;
    var d = Array.apply(null, Array(m)).map(Number.prototype.valueOf,0);
    var t = Array.apply(null, Array(m*m)).map(Number.prototype.valueOf,0);
    var rez = arr.reduce(function (r, v) {
            for(var i = 0; i < m; i++) {d[i] = v[i] - av[i];}
            for(var i = 0; i < m; i++) {
                for(var j = i; j < m; j++) {
                    t[i+j*m] = d[i]*d[j];
                }
            }
            return r.map(function (e, i) {return e + t[i];});
        }
        , Array.apply(null, Array(m*m)).map(Number.prototype.valueOf,0)
        );
    for(var i = 0; i < m; i++) {
        for(var j = i; j < m; j++) {
            rez[i+j*m] /= Math.max(n-1,1);
        }
        for(var j = i+1; j < m; j++) {
            rez[i*m+j] = rez[i+j*m];
        }
    }
    return rez;
}

function gm$GeometryDims(x) {
    'use strict';
    if (x.length === 0) { return 0; }
    if (x[0].constructor === Number) { return x.length; }
    var rez = 0, i = 0;
    while (rez === 0 && i < x.length) {
        rez = gm$GeometryDims(x[i++]);
    }
    return rez;
}

// check the area and ccw/cw order
function gm$shoelace(points) {
    'use strict';
    var n = points.length;
    var det2f = function (p,q){ return p[0]*q[1] - p[1]*q[0];};
    return 0.5* points.reduce(function(r, e, i){return r + det2f(points[(i === 0 ? n : i) - 1],e);}, 0);
}

// dilate a convex hull by given
function gm$resizeConvexHull(d, points) {
    'use strict';
    var n = points.length;
    var s = gm$shoelace(points) <= 0 ? 1 : -1;
    var vecs = points.map(function(e,i){var v = minusJSVec(e,points[(i === 0 ? n : i) - 1]); var l = s/Math.hypot(v[0],v[1]);
                                        return [-v[1]*l,v[0]*l];});
    vecs.push(vecs[0]); // now vecs is an array of length (n+1), stores directions orthogonal to lines i and i-1 (cycled)
    return points.map(function(p,i) {
        var v = plusJSVec(vecs[i],vecs[i+1]);
        var xd = d*2/(v[0]*v[0] + v[1]*v[1]);
        return plusJSVec(p,[v[0]*xd,v[1]*xd]);
    });
}



// find a convex hull for 2-dimensional array
// https://en.wikipedia.org/wiki/Graham_scan
function gm$GrahamScan(arr) {
    'use strict';
    var n = arr.length;
    // triangles are always convex
    if(n <= 3){return arr;}
    // from here we start an inplace algorithm
    var hull = arr.slice();
    // closure to swap two elements in array
    var swap = function(i, j) { var t = hull[i]; hull[i] = hull[j]; hull[j] = t; };
    // check if three points are counter-clockwise (ccw > 0), clockwise (ccw < 0), or collinear (ccw == 0)
    var ccw = function (i1,i2,i3) {return (hull[i2][0] - hull[i1][0])*(hull[i3][1] - hull[i1][1]) - (hull[i2][1] - hull[i1][1])*(hull[i3][0] - hull[i1][0]);};
    // get the point with the lowest y-coordinate
    var start = hull.splice(hull.reduce(function(r,e,i){return (hull[r][1] > e[1] || (hull[r][1] === e[1] && hull[r][0] > e[0] )) ? i : r; }, 0), 1)[0];
    // sort by angle to starting point
    hull.sort(function(a,b){return Math.atan2(a[1]-start[1],a[0]-start[0]) - Math.atan2(b[1]-start[1],b[0]-start[0]);});
    // two beginning points are last and first ones
    var end = hull[hull.length-1].slice();
    hull.splice(0,0,end,start);
    // number of points in the convex hull
    var m = 1;
    for(var i = 2; i <= n; i++) {
        while (ccw(m-1,m,i) <= 0) { // Find next valid point on convex hull
            if (m > 1) {
                m--;
            } else if (i === n) { // All points are collinear
                break;
            } else {
                i++;
            }
        }
        m++;
        swap( m, i );
    }
    return hull.slice(1,m+1);
}

function gm$GrahamScanIds(arr) {
    'use strict';
    var n = arr.length;
    // triangles are always convex
    if(n <= 3){return arr;}
    // from here we start an inplace algorithm
    var hull = arr.map(function(e,i){return e.concat([i]);});
    // closure to swap two elements in array
    var swap = function(i, j) { var t = hull[i]; hull[i] = hull[j]; hull[j] = t;};
    // check if three points are counter-clockwise (ccw > 0), clockwise (ccw < 0), or collinear (ccw == 0)
    var ccw = function (i1,i2,i3) {return (hull[i2][0] - hull[i1][0])*(hull[i3][1] - hull[i1][1]) - (hull[i2][1] - hull[i1][1])*(hull[i3][0] - hull[i1][0]);};
    // get the point with the lowest y-coordinate
    var start = hull.splice(hull.reduce(function(r,e,i){return (hull[r][1] > e[1] || (hull[r][1] === e[1] && hull[r][0] > e[0] )) ? i : r; }, 0), 1)[0];
    // sort by angle to starting point
    hull.sort(function(a,b){return Math.atan2(a[1]-start[1],a[0]-start[0]) - Math.atan2(b[1]-start[1],b[0]-start[0]);});
    // two beginning points are last and first ones
    var end = hull[hull.length-1].slice();
    hull.splice(0,0,end,start);
    // number of points in the convex hull
    var m = 1;
    for(var i = 2; i <= n; i++) {
        while (ccw(m-1,m,i) <= 0) { // Find next valid point on convex hull
            if (m > 1) {
                m--;
            } else if (i === n) { // All points are collinear
                break;
            } else {
                i++;
            }
        }
        m++;
        swap( m, i );
    }
    return hull.slice(1,m+1).map(function(e){return e[2];});
}

// calculate minimum bounding rectangle for convex ccw polygon (output of gm$GrahamScan)
// output: [angle, area, center point, dir1*w/2, dir2*h/2]
// height of the rectangle is always not less then width
function gm$minRectAngle(arr) {
    'use strict';
    var n = arr.length;
    // empty
    if(n === 0){return [0,0,[0,0],[0,0],[0,0]];}
    // just a point
    if(n <= 1){return [0,0,arr[0].slice(),[0,0],[0,0]];}
    // line segment, area = 0
    var d;
    if(n === 2){d = [arr[1][1] - arr[0][1],arr[1][0] - arr[0][0]];
                return [ Math.atan2(d[1],d[0])
                       , 0
                       , arr[0].slice()
                       , d, [0,0] ];}
    // precompute angle of a segment for each point
    var points = [[],[],[],[]];
    var PI2 = Math.PI/2;
    arr.forEach(function(e,i){
        var j = i+1 === n ? 0 : i+1;
        var a = Math.atan2( arr[j][1] - e[1], arr[j][0] - e[0]);
        if (a < - PI2) { points[0].push(e.concat([a+Math.PI]));}
        else if (a < 0) { points[1].push(e.concat([a+PI2]));}
        else if (a < PI2) { points[2].push(e.concat([a]));}
        else { points[3].push(e.concat([a-PI2]));}
    });
    // add last point to each side (a beginning point of a nest side)
    points.forEach(function(e,i){
        var j = i === 3 ? 0 : i + 1;
        var k = 1;
        while(points[j].length === 0) {
            j = j === 3 ? 0 : j + 1;
            k++;
        }
        var x = points[j][0];
        e.push([x[0],x[1],x[2]+PI2*k]);
    });
    // starting indices
    var ids = [0,0,0,0];
    // state[0] is current aligned side
    // state[1] is current rotation
    var state = [0,-1];
    // find minimum angle and corresponding side index
    var nextstate = function(){
        return points.reduce(function(r,e,i){
                   return r[1] > e[ids[i]][2] ? [i,e[ids[i]][2]] : r;
               }, [0,points[0][ids[0]][2]]);
    };
    var dot = function(a,b1,b0){return a[0]*(b1[0] - b0[0]) + a[1]*(b1[1] - b0[1]);};
    var oangle;
    var nw = [1,0];
    var nh = [0,1];
    var w = Infinity, h = Infinity;
    var rez = [0,Infinity,[0,0],[Infinity,0],[0,Infinity]];
    // rotate bounding rectangle from 0 to pi/2
    while ( state[1] < PI2 ) {
        oangle = state[1];
        // find an angle that corresponds to next aligned edge
        do {
            state = nextstate();
            ids[state[0]]++;
        } while (state[1] === oangle);
        if (state[1] >= PI2) {break;}
        // normals of the bounding rectangle
        nw = [Math.cos(state[1]),Math.sin(state[1])];
        nh = [-nw[1],nw[0]];
        // calculating width
        w = dot(nw, points[3][ids[3]], points[1][ids[1]]);
        // calculating height
        h = dot(nh, points[0][ids[0]], points[2][ids[2]]);
        // if the area is reduced, recalculate the result
        if (w*h < rez[1] ) {
            d = dot(nw, points[2][ids[2]], points[1][ids[1]]);
            if (h >= w) {
                rez = [ state[1]
                      , w*h
                      , [ points[2][ids[2]][0] + (w*0.5 - d)*nw[0] + h*0.5*nh[0], points[2][ids[2]][1] + (w*0.5 - d)*nw[1] + h*0.5*nh[1] ]
                      , [nw[0]*w/2,nw[1]*w/2]
                      , [nh[0]*h/2,nh[1]*h/2] ];
            } else {
                rez = [ state[1] - PI2
                      , w*h
                      , [ points[2][ids[2]][0] + (w*0.5 - d)*nw[0] + h*0.5*nh[0], points[2][ids[2]][1] + (w*0.5 - d)*nw[1] + h*0.5*nh[1] ]
                      , [-nh[0]*h/2,-nh[1]*h/2]
                      , [nw[0]*w/2,nw[1]*w/2] ];
            }
        }
    }
    return rez;
}


/*
Numeric Javascript
Copyright (C) 2011 by Sébastien Loisel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
function gm$principalEigenvectors(mat,n) {
    'use strict';
    var A = new Array(n);
    for(var i = 0; i < n; i++) {
        A[i] = mat.slice(i*n,(i+1)*n);
    }
    var eigs = numeric['eig'](A);
    return eigs.lambda.x
        .map(function(e,i){return [e,eigs.E.x[i]];})
        .sort(function(a,b){return b[0]-a[0];})
        .map(function(e){return e[1];});
}



