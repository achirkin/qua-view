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
    var eigs = numeric.eig(A);
    return eigs.lambda.x
        .map(function(e,i){return [e,eigs.E.x[i]];})
        .sort(function(a,b){return b[0]-a[0];})
        .map(function(e){return e[1];});
}
