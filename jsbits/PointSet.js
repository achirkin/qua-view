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
    hull.forEach(function(e,i){var x = e[0]-start[0], y = e[1]-start[1]; hull[i]=[x,y,x*x + y*y].concat(e);});
    hull.sort(function(a,b){
        return (a[1]*b[0] - a[0]*b[1]) || (a[2] - b[2]);
    });
    start = [0,0,0].concat(start);
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
    // second pass! --------------------------------------------------------------------------------
    if(m > 3) {
        hull.splice(0,1);
        hull.splice(m,n);
        n = m;
        // get the point with the highest y-coordinate
        start = hull.splice(hull.reduce(function(r,e,i){return (hull[r][1] < e[1] || (hull[r][1] === e[1] && hull[r][0] < e[0] )) ? i : r; }, 0), 1)[0];
        // sort by angle to starting point
        hull.forEach(function(e,i){var x = e[0]-start[0], y = e[1]-start[1]; hull[i]=[x,y,x*x + y*y].concat(e.slice(3));});
        hull.sort(function(a,b){
            return (a[1]*b[0] - a[0]*b[1]) || (a[2] - b[2]);
        });
        start[0] = 0; start[1] = 0; start[2] = 0;
        // two beginning points are last and first ones
        end = hull[hull.length-1].slice();
        hull.splice(0,0,end,start);
        m = 1;
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
    }
    // end of second pass --------------------------------------------------------------------------
    return hull.slice(1,m+1).map(function(e){return e.slice(3);});
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
    hull.forEach(function(e,i){var x = e[0]-start[0], y = e[1]-start[1]; hull[i]=[x,y,x*x + y*y].concat(e);});
    hull.sort(function(a,b){
        return (a[1]*b[0] - a[0]*b[1]) || (a[2] - b[2]);
    });
    start = [0,0,0].concat(start);
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
    // second pass! --------------------------------------------------------------------------------
    if(m > 3) {
        hull.splice(0,1);
        hull.splice(m,n);
        n = m;
        // get the point with the highest y-coordinate
        start = hull.splice(hull.reduce(function(r,e,i){return (hull[r][1] < e[1] || (hull[r][1] === e[1] && hull[r][0] < e[0] )) ? i : r; }, 0), 1)[0];
        // sort by angle to starting point
        hull.forEach(function(e,i){var x = e[0]-start[0], y = e[1]-start[1]; hull[i]=[x,y,x*x + y*y].concat(e.slice(3));});
        hull.sort(function(a,b){
            return (a[1]*b[0] - a[0]*b[1]) || (a[2] - b[2]);
        });
        start[0] = 0; start[1] = 0; start[2] = 0;
        // two beginning points are last and first ones
        end = hull[hull.length-1].slice();
        hull.splice(0,0,end,start);
        m = 1;
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
    }
    // end of second pass --------------------------------------------------------------------------
    return hull.slice(1,m+1).map(function(e){return e[5];});
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

/**
 * OMABR
 * Oriented Minimum Area Bounding Rectangle
 * Input: A convex polygon with number of vertices > 2.
 * Output:
 *   Area -> Area of the final rectangle
 *   Center -> Center of the final rectangle
 *   WidthDirection -> Vector from center parallel to shorter side
 *                     that reaches the longer side.
 *   HeightDirection -> Vector from center parallel to longer side
 *                      that reaches the shorter side.
 * Polygon -> [Point], does not repeat its first point.
 * Point -> [Number, Number]
 */

/**
 * Get direction change from vector PQ to vector QR.
 * @param {Point} p
 * @param {Point} q
 * @param {Point} r
 * @returns {Number} angle -- Angle in radian from range [0, pi).
 */
function getTurn(p, q, r) {
  var angle = Math.atan2(r[1]-q[1], r[0]-q[0]) -
              Math.atan2(q[1]-p[1], q[0]-p[0]);
  if(angle < 0) angle += 2*Math.PI;
  return angle;
}

/**
 * Given an array of points, it will find all direction changes.
 * @param {[Point]} arr
 * @returns {[Number]} res
 */
function getDirectionChanges(arr) {
  var res = [];
  var n = arr.length;
  arr[n] = arr[0]; arr[n+1] = arr[1];
  for(var i = 0; i < n; ++i) {
    res[i] = getTurn(arr[i], arr[i+1], arr[i+2]);
  }
  return res;
}

/**
 * Get projection of point A to line PQ.
 * Inspired by: https://stackoverflow.com/questions/849211/
 *     shortest-distance-between-a-point-and-a-line-segment
 *     Answer by Joshua
 * Assumption: p and q are different.
 * @param {Point} p
 * @param {Point} q
 * @param {Point} a
 * @returns {Point} res -- the projection point.
 */
function getProjection(p, q, a) {
  var xap = a[0] - p[0];
  var yap = a[1] - p[1];
  var xqp = q[0] - p[0];
  var yqp = q[1] - p[1];
  var dot = xap * xqp + yap * yqp;
  var dist = xqp * xqp + yqp * yqp;
  var res = [];
  res[0] = p[0] + (xqp * dot) / dist;
  res[1] = p[1] + (yqp * dot) / dist;
  return res;
}

/**
 * Get midpoint of two points P and Q.
 * @param {Point} p
 * @param {Point} q
 * @returns {Point}
 */
function getMidpoint(p, q) {
  return [(p[0] + q[0]) / 2, (p[1] + q[1]) / 2];
}

/**
 * Calculate the properties of the bounding rectangle.
 * @param {Point} p1a -- one of the vertex of the base
 * @param {Point} p1b -- one of the vertex of the base
 * @param {Point} p2 -- vertex on the right side.
 * @param {Point} p3 -- vertex on the top side.
 * @param {Point} p4 -- vertex on the left side.
 * @returns {[area, center, widthDirection, heightDirection]}
 */
function getProperties(p1a, p1b, p2, p3, p4) {
  var q2 = getProjection(p1a, p1b, p2);
  var q3 = getProjection(p1a, p1b, p3);
  var q4 = getProjection(p1a, p1b, p4);
  var mid1 = getMidpoint(q2, q4);
  var mid2 = getMidpoint(p3, q3);
  var side1 = Math.hypot(q2[1] - q4[1], q2[0] - q4[0]);
  var side2 = Math.hypot(p3[1] - q3[1], p3[0] - q3[0]);
  var area = side1 * side2;
  var center = []
  center[0] = mid1[0] + mid2[0] - q3[0];
  center[1] = mid1[1] + mid2[1] - q3[1];
  var dir1 = [];
  dir1[0] = mid1[0] - q2[0];
  dir1[1] = mid1[1] - q2[1];
  var dir2 = [];
  dir2[0] = mid2[0] - p3[0];
  dir2[1] = mid2[1] - p3[1];
  var widthD, heightD;
  // ensure height is always not less than width (qua-kit)
  if(side1 < side2) {
    widthD = dir1;
    heightD = dir2;
  } else {
    widthD = dir2;
    heightD = dir1;
  }
  // ensure heightDirection is pi/2 CCW of widthDirection
  var checkAngle = (Math.atan2(heightD[1], heightD[0]) -
                    Math.atan2(widthD[1], widthD[0]))
  if(checkAngle < 0) checkAngle += 2*Math.PI;
  if(checkAngle > Math.PI) {
    heightD[0] = -heightD[0];
    heightD[1] = -heightD[1];
  }
  return [area, center, widthD, heightD];
}

/**
 * Calculate the minimum area bounding rectangle.
 * @param {Polygon} arr -- convex polygon with vertices > 2
 * @returns {[area, center, widthDirection, heightDirection]}
 */
function getMinAreaBoundRect(arr) {
  var n = arr.length;
  if(n === 0) {
    return [0, [0, 0], [0, 0], [0, 0]];
  }
  if(n == 1) {
    return [0, arr[0], [0, 0], [0, 0]];
  }
  if(n == 2) {
    // line segment
    p1 = arr[0];
    p2 = arr[1];
    var center = getMidpoint(p1, p2);
    var heightD = [p2[0] - center[0], p2[1] - center[1]];
    return [0, center, [0, 0], heightD];
  }
  // duplicate array
  arr = arr.concat(arr);
  var turns = getDirectionChanges(arr);
  // create prefix sum array for turns.
  var cum = [];
  cum[0] = 0;
  for(var i = 1; i < 2*n; ++i) {
    cum[i] = cum[i-1] + turns[i-1];
  }
  // prepare variable for finding the result
  var area = Number.MAX_VALUE;
  var info; // prop of the current result
  var prop;
  var r1, r2, r3, r4;
  // explore all possible bounding rectangle by rotation
  var p1 = 0, p2 = 0, p3 = 0, p4 = 0;
  for(; p1 < n; ++p1) {
    // move the pointers
    while(cum[p2] < cum[p1] + Math.PI/2) p2++;
    while(cum[p3] < cum[p1] + Math.PI) p3++;
    while(cum[p4] < cum[p1] + 3*Math.PI/2) p4++;
    prop = getProperties(arr[p1], arr[p1+1], 
                         arr[p2], arr[p3], arr[p4]);
    if(prop[0] < area) {
      area = prop[0];
      info = prop;
      r1 = p1;
      r2 = p2;
      r3 = p3;
      r4 = p4;
    }
  }
  return info;
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
    var eigs = gm$principalEigenvectorsTryEig(A, 100, 1.0e-8, 4);
    numeric['epsilon'] = 1.0e-8;
    return eigs.lambda.x
        .map(function(e,i){return [e,eigs.E.x[i]];})
        .sort(function(a,b){return b[0]-a[0];})
        .map(function(e){return e[1];});
}

function gm$principalEigenvectorsTryEig(A, maxiter, eps, i) {
    'use strict';
    numeric['epsilon'] = eps;
    if (i <= 0) {
      console.log("Warning: eigenvector computation failed!");
      var dim = numeric.dim(A);
      return numeric.eig(numeric.identity(dim[0]));
    } else {
      try {
        return numeric.eig(A, maxiter);
      } catch (err) {
        return gm$principalEigenvectorsTryEig(A, maxiter*2, eps*100, i-1);
      }
    }
}


