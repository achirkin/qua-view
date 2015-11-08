"use strict";
/*
var numeric = (typeof exports === "undefined")?(function numeric() {}):(exports);
if(typeof global !== "undefined") { global.numeric = numeric; } */
numeric = {};

numeric['version'] = "1.2.6";

// 1. Utility functions
numeric['bench'] = function bench (f,interval) {
    var t1,t2,n,i;
    if(typeof interval === "undefined") { interval = 15; }
    n = 0.5;
    t1 = new Date();
    while(1) {
        n*=2;
        for(i=n;i>3;i-=4) { f(); f(); f(); f(); }
        while(i>0) { f(); i--; }
        t2 = new Date();
        if(t2-t1 > interval) break;
    }
    for(i=n;i>3;i-=4) { f(); f(); f(); f(); }
    while(i>0) { f(); i--; }
    t2 = new Date();
    return 1000*(3*n-1)/(t2-t1);
}

numeric['_myIndexOf'] = (function _myIndexOf(w) {
    var n = this.length,k;
    for(k=0;k<n;++k) if(this[k]===w) return k;
    return -1;
});
numeric['myIndexOf'] = (Array.prototype.indexOf)?Array.prototype.indexOf:numeric['_myIndexOf'];

numeric['Function'] = Function;
numeric['precision'] = 4;
numeric['largeArray'] = 50;

// 2. Linear algebra with Arrays.
numeric['_dim'] = function _dim(x) {
    var ret = [];
    while(typeof x === "object") { ret.push(x.length); x = x[0]; }
    return ret;
}

numeric['dim'] = function dim(x) {
    var y,z;
    if(typeof x === "object") {
        y = x[0];
        if(typeof y === "object") {
            z = y[0];
            if(typeof z === "object") {
                return numeric['_dim'](x);
            }
            return [x.length,y.length];
        }
        return [x.length];
    }
    return [];
}

numeric['mapreduce'] = function mapreduce(body,init) {
    return Function('x','accum','_s','_k',
            'if(typeof accum === "undefined") accum = '+init+';\n'+
            'if(typeof x === "number") { var xi = x; '+body+'; return accum; }\n'+
            'if(typeof _s === "undefined") _s = numeric[\'dim\'](x);\n'+
            'if(typeof _k === "undefined") _k = 0;\n'+
            'var _n = _s[_k];\n'+
            'var i,xi;\n'+
            'if(_k < _s.length-1) {\n'+
            '    for(i=_n-1;i>=0;i--) {\n'+
            '        accum = arguments.callee(x[i],accum,_s,_k+1);\n'+
            '    }'+
            '    return accum;\n'+
            '}\n'+
            'for(i=_n-1;i>=1;i-=2) { \n'+
            '    xi = x[i];\n'+
            '    '+body+';\n'+
            '    xi = x[i-1];\n'+
            '    '+body+';\n'+
            '}\n'+
            'if(i === 0) {\n'+
            '    xi = x[i];\n'+
            '    '+body+'\n'+
            '}\n'+
            'return accum;'
            );
}
numeric['mapreduce2'] = function mapreduce2(body,setup) {
    return Function('x',
            'var n = x.length;\n'+
            'var i,xi;\n'+setup+';\n'+
            'for(i=n-1;i!==-1;--i) { \n'+
            '    xi = x[i];\n'+
            '    '+body+';\n'+
            '}\n'+
            'return accum;'
            );
}


numeric['same'] = function same(x,y) {
    var i,n;
    if(!(x instanceof Array) || !(y instanceof Array)) { return false; }
    n = x.length;
    if(n !== y.length) { return false; }
    for(i=0;i<n;i++) {
        if(x[i] === y[i]) { continue; }
        if(typeof x[i] === "object") { if(!same(x[i],y[i])) return false; }
        else { return false; }
    }
    return true;
}

numeric['rep'] = function rep(s,v,k) {
    if(typeof k === "undefined") { k=0; }
    var n = s[k], ret = Array(n), i;
    if(k === s.length-1) {
        for(i=n-2;i>=0;i-=2) { ret[i+1] = v; ret[i] = v; }
        if(i===-1) { ret[0] = v; }
        return ret;
    }
    for(i=n-1;i>=0;i--) { ret[i] = numeric['rep'](s,v,k+1); }
    return ret;
}


numeric['dotMMsmall'] = function dotMMsmall(x,y) {
    var i,j,k,p,q,r,ret,foo,bar,woo,i0,k0,p0,r0;
    p = x.length; q = y.length; r = y[0].length;
    ret = Array(p);
    for(i=p-1;i>=0;i--) {
        foo = Array(r);
        bar = x[i];
        for(k=r-1;k>=0;k--) {
            woo = bar[q-1]*y[q-1][k];
            for(j=q-2;j>=1;j-=2) {
                i0 = j-1;
                woo += bar[j]*y[j][k] + bar[i0]*y[i0][k];
            }
            if(j===0) { woo += bar[0]*y[0][k]; }
            foo[k] = woo;
        }
        ret[i] = foo;
    }
    return ret;
}
numeric['_getCol'] = function _getCol(A,j,x) {
    var n = A.length, i;
    for(i=n-1;i>0;--i) {
        x[i] = A[i][j];
        --i;
        x[i] = A[i][j];
    }
    if(i===0) x[0] = A[0][j];
}
numeric['dotMMbig'] = function dotMMbig(x,y){
    var gc = numeric['_getCol'], p = y.length, v = Array(p);
    var m = x.length, n = y[0].length, A = new Array(m), xj;
    var VV = numeric['dotVV'];
    var i,j,k,z;
    --p;
    --m;
    for(i=m;i!==-1;--i) A[i] = Array(n);
    --n;
    for(i=n;i!==-1;--i) {
        gc(y,i,v);
        for(j=m;j!==-1;--j) {
            z=0;
            xj = x[j];
            A[j][i] = VV(xj,v);
        }
    }
    return A;
}

numeric['dotMV'] = function dotMV(x,y) {
    var p = x.length, q = y.length,i;
    var ret = Array(p), dotVV = numeric['dotVV'];
    for(i=p-1;i>=0;i--) { ret[i] = dotVV(x[i],y); }
    return ret;
}

numeric['dotVM'] = function dotVM(x,y) {
    var i,j,k,p,q,r,ret,foo,bar,woo,i0,k0,p0,r0,s1,s2,s3,baz,accum;
    p = x.length; q = y[0].length;
    ret = Array(q);
    for(k=q-1;k>=0;k--) {
        woo = x[p-1]*y[p-1][k];
        for(j=p-2;j>=1;j-=2) {
            i0 = j-1;
            woo += x[j]*y[j][k] + x[i0]*y[i0][k];
        }
        if(j===0) { woo += x[0]*y[0][k]; }
        ret[k] = woo;
    }
    return ret;
}

numeric['dotVV'] = function dotVV(x,y) {
    var i,n=x.length,i1,ret = x[n-1]*y[n-1];
    for(i=n-2;i>=1;i-=2) {
        i1 = i-1;
        ret += x[i]*y[i] + x[i1]*y[i1];
    }
    if(i===0) { ret += x[0]*y[0]; }
    return ret;
}

numeric['dot'] = function dot(x,y) {
    var d = numeric['dim'];
    switch(d(x).length*1000+d(y).length) {
    case 2002:
        if(y.length < 10) return numeric['dotMMsmall'](x,y);
        else return numeric['dotMMbig'](x,y);
    case 2001: return numeric['dotMV'](x,y);
    case 1002: return numeric['dotVM'](x,y);
    case 1001: return numeric['dotVV'](x,y);
    case 1000: return numeric['mulVS'](x,y);
    case 1: return numeric['mulSV'](x,y);
    case 0: return x*y;
    default: throw new Error('numeric[\'dot\'] only works on vectors and matrices');
    }
}

numeric['diag'] = function diag(d) {
    var i,i1,j,n = d.length, A = Array(n), Ai;
    for(i=n-1;i>=0;i--) {
        Ai = Array(n);
        i1 = i+2;
        for(j=n-1;j>=i1;j-=2) {
            Ai[j] = 0;
            Ai[j-1] = 0;
        }
        if(j>i) { Ai[j] = 0; }
        Ai[i] = d[i];
        for(j=i-1;j>=1;j-=2) {
            Ai[j] = 0;
            Ai[j-1] = 0;
        }
        if(j===0) { Ai[0] = 0; }
        A[i] = Ai;
    }
    return A;
}
numeric['getDiag'] = function(A) {
    var n = Math.min(A.length,A[0].length),i,ret = Array(n);
    for(i=n-1;i>=1;--i) {
        ret[i] = A[i][i];
        --i;
        ret[i] = A[i][i];
    }
    if(i===0) {
        ret[0] = A[0][0];
    }
    return ret;
}

numeric['identity'] = function identity(n) { return numeric['diag'](numeric['rep']([n],1)); }
numeric['pointwise'] = function pointwise(params,body,setup) {
    if(typeof setup === "undefined") { setup = ""; }
    var fun = [];
    var k;
    var avec = /\[i\]$/,p,thevec = '';
    var haveret = false;
    for(k=0;k<params.length;k++) {
        if(avec.test(params[k])) {
            p = params[k].substring(0,params[k].length-3);
            thevec = p;
        } else { p = params[k]; }
        if(p==='ret') haveret = true;
        fun.push(p);
    }
    fun[params.length] = '_s';
    fun[params.length+1] = '_k';
    fun[params.length+2] = (
            'if(typeof _s === "undefined") _s = numeric[\'dim\']('+thevec+');\n'+
            'if(typeof _k === "undefined") _k = 0;\n'+
            'var _n = _s[_k];\n'+
            'var i'+(haveret?'':', ret = Array(_n)')+';\n'+
            'if(_k < _s.length-1) {\n'+
            '    for(i=_n-1;i>=0;i--) ret[i] = arguments.callee('+params.join(',')+',_s,_k+1);\n'+
            '    return ret;\n'+
            '}\n'+
            setup+'\n'+
            'for(i=_n-1;i!==-1;--i) {\n'+
            '    '+body+'\n'+
            '}\n'+
            'return ret;'
            );
    return Function.apply(null,fun);
}
numeric['pointwise2'] = function pointwise2(params,body,setup) {
    if(typeof setup === "undefined") { setup = ""; }
    var fun = [];
    var k;
    var avec = /\[i\]$/,p,thevec = '';
    var haveret = false;
    for(k=0;k<params.length;k++) {
        if(avec.test(params[k])) {
            p = params[k].substring(0,params[k].length-3);
            thevec = p;
        } else { p = params[k]; }
        if(p==='ret') haveret = true;
        fun.push(p);
    }
    fun[params.length] = (
            'var _n = '+thevec+'.length;\n'+
            'var i'+(haveret?'':', ret = Array(_n)')+';\n'+
            setup+'\n'+
            'for(i=_n-1;i!==-1;--i) {\n'+
            body+'\n'+
            '}\n'+
            'return ret;'
            );
    return Function.apply(null,fun);
}
numeric['_biforeach'] = function _biforeach(x,y,s,k,f) {
    if(k === s.length-1) { f(x,y); return; }
    var i,n=s[k];
    for(i=n-1;i>=0;i--) { _biforeach(typeof x==="object"?x[i]:x,typeof y==="object"?y[i]:y,s,k+1,f); }
}
numeric['_biforeach2'] = function _biforeach2(x,y,s,k,f) {
    if(k === s.length-1) { return f(x,y); }
    var i,n=s[k],ret = Array(n);
    for(i=n-1;i>=0;--i) { ret[i] = _biforeach2(typeof x==="object"?x[i]:x,typeof y==="object"?y[i]:y,s,k+1,f); }
    return ret;
}
numeric['_foreach'] = function _foreach(x,s,k,f) {
    if(k === s.length-1) { f(x); return; }
    var i,n=s[k];
    for(i=n-1;i>=0;i--) { _foreach(x[i],s,k+1,f); }
}
numeric['_foreach2'] = function _foreach2(x,s,k,f) {
    if(k === s.length-1) { return f(x); }
    var i,n=s[k], ret = Array(n);
    for(i=n-1;i>=0;i--) { ret[i] = _foreach2(x[i],s,k+1,f); }
    return ret;
}

/*numeric['anyV'] = numeric['mapreduce']('if(xi) return true;','false');
numeric['allV'] = numeric['mapreduce']('if(!xi) return false;','true');
numeric['any'] = function(x) { if(typeof x.length === "undefined") return x; return numeric['anyV'](x); }
numeric['all'] = function(x) { if(typeof x.length === "undefined") return x; return numeric['allV'](x); }*/

numeric['ops2'] = {
        'add': '+',
        'sub': '-',
        'mul': '*',
        'div': '/',
        'mod': '%',
        'and': '&&',
        'or':  '||',
        'eq':  '===',
        'neq': '!==',
        'lt':  '<',
        'gt':  '>',
        'leq': '<=',
        'geq': '>=',
        'band': '&',
        'bor': '|',
        'bxor': '^',
        'lshift': '<<',
        'rshift': '>>',
        'rrshift': '>>>'
};
numeric['opseq'] = {
        'addeq': '+=',
        'subeq': '-=',
        'muleq': '*=',
        'diveq': '/=',
        'modeq': '%=',
        'lshifteq': '<<=',
        'rshifteq': '>>=',
        'rrshifteq': '>>>=',
        'bandeq': '&=',
        'boreq': '|=',
        'bxoreq': '^='
};
numeric['mathfuns'] = ['abs','acos','asin','atan','ceil','cos',
                    'exp','floor','log','round','sin','sqrt','tan',
                    'isNaN','isFinite'];
numeric['mathfuns2'] = ['atan2','pow','max','min'];
numeric['ops1'] = {
        'neg': '-',
        'not': '!',
        'bnot': '~',
        'clone': ''
};
numeric['mapreducers'] = {
        'any': ['if(xi) return true;','var accum = false;'],
        'all': ['if(!xi) return false;','var accum = true;'],
        'sum': ['accum += xi;','var accum = 0;'],
        'prod': ['accum *= xi;','var accum = 1;'],
        'norm2Squared': ['accum += xi*xi;','var accum = 0;'],
        'norminf': ['accum = max(accum,abs(xi));','var accum = 0, max = Math.max, abs = Math.abs;'],
        'norm1': ['accum += abs(xi)','var accum = 0, abs = Math.abs;'],
        'sup': ['accum = max(accum,xi);','var accum = -Infinity, max = Math.max;'],
        'inf': ['accum = min(accum,xi);','var accum = Infinity, min = Math.min;']
};

(function () {
    var i,o;
    for(i=0;i<numeric['mathfuns2'].length;++i) {
        o = numeric['mathfuns2'][i];
        numeric['ops2'][o] = o;
    }
    for(i in numeric['ops2']) {
        if(numeric['ops2'].hasOwnProperty(i)) {
            o = numeric['ops2'][i];
            var code, codeeq, setup = '';
            if(numeric['myIndexOf'].call(numeric['mathfuns2'],i)!==-1) {
                setup = 'var '+o+' = Math.'+o+';\n';
                code = function(r,x,y) { return r+' = '+o+'('+x+','+y+')'; };
                codeeq = function(x,y) { return x+' = '+o+'('+x+','+y+')'; };
            } else {
                code = function(r,x,y) { return r+' = '+x+' '+o+' '+y; };
                if(numeric['opseq'].hasOwnProperty(i+'eq')) {
                    codeeq = function(x,y) { return x+' '+o+'= '+y; };
                } else {
                    codeeq = function(x,y) { return x+' = '+x+' '+o+' '+y; };
                }
            }
            numeric[i+'VV'] = numeric['pointwise2'](['x[i]','y[i]'],code('ret[i]','x[i]','y[i]'),setup);
            numeric[i+'SV'] = numeric['pointwise2'](['x','y[i]'],code('ret[i]','x','y[i]'),setup);
            numeric[i+'VS'] = numeric['pointwise2'](['x[i]','y'],code('ret[i]','x[i]','y'),setup);
            numeric[i] = Function(
                    'var n = arguments.length, i, x = arguments[0], y;\n'+
                    'var VV = numeric[\''+i+'VV\'], VS = numeric[\''+i+'VS\'], SV = numeric[\''+i+'SV\'];\n'+
                    'var dim = numeric[\'dim\'];\n'+
                    'for(i=1;i!==n;++i) { \n'+
                    '  y = arguments[i];\n'+
                    '  if(typeof x === "object") {\n'+
                    '      if(typeof y === "object") x = numeric[\'_biforeach2\'](x,y,dim(x),0,VV);\n'+
                    '      else x = numeric[\'_biforeach2\'](x,y,dim(x),0,VS);\n'+
                    '  } else if(typeof y === "object") x = numeric[\'_biforeach2\'](x,y,dim(y),0,SV);\n'+
                    '  else '+codeeq('x','y')+'\n'+
                    '}\nreturn x;\n');
            numeric[o] = numeric[i];
            numeric[i+'eqV'] = numeric['pointwise2'](['ret[i]','x[i]'], codeeq('ret[i]','x[i]'),setup);
            numeric[i+'eqS'] = numeric['pointwise2'](['ret[i]','x'], codeeq('ret[i]','x'),setup);
            numeric[i+'eq'] = Function(
                    'var n = arguments.length, i, x = arguments[0], y;\n'+
                    'var V = numeric[\''+i+'eqV\'], S = numeric[\''+i+'eqS\']\n'+
                    'var s = numeric[\'dim\'](x);\n'+
                    'for(i=1;i!==n;++i) { \n'+
                    '  y = arguments[i];\n'+
                    '  if(typeof y === "object") numeric[\'_biforeach\'](x,y,s,0,V);\n'+
                    '  else numeric[\'_biforeach\'](x,y,s,0,S);\n'+
                    '}\nreturn x;\n');
        }
    }
    for(i=0;i<numeric['mathfuns2'].length;++i) {
        o = numeric['mathfuns2'][i];
        delete numeric['ops2'][o];
    }
    for(i=0;i<numeric['mathfuns'].length;++i) {
        o = numeric['mathfuns'][i];
        numeric['ops1'][o] = o;
    }
    for(i in numeric['ops1']) {
        if(numeric['ops1'].hasOwnProperty(i)) {
            setup = '';
            o = numeric['ops1'][i];
            if(numeric['myIndexOf'].call(numeric['mathfuns'],i)!==-1) {
                if(Math.hasOwnProperty(o)) setup = 'var '+o+' = Math.'+o+';\n';
            }
            numeric[i+'eqV'] = numeric['pointwise2'](['ret[i]'],'ret[i] = '+o+'(ret[i]);',setup);
            numeric[i+'eq'] = Function('x',
                    'if(typeof x !== "object") return '+o+'x\n'+
                    'var i;\n'+
                    'var V = numeric[\''+i+'eqV\'];\n'+
                    'var s = numeric[\'dim\'](x);\n'+
                    'numeric[\'_foreach\'](x,s,0,V);\n'+
                    'return x;\n');
            numeric[i+'V'] = numeric['pointwise2'](['x[i]'],'ret[i] = '+o+'(x[i]);',setup);
            numeric[i] = Function('x',
                    'if(typeof x !== "object") return '+o+'(x)\n'+
                    'var i;\n'+
                    'var V = numeric[\''+i+'V\'];\n'+
                    'var s = numeric[\'dim\'](x);\n'+
                    'return numeric[\'_foreach2\'](x,s,0,V);\n');
        }
    }
    for(i=0;i<numeric['mathfuns'].length;++i) {
        o = numeric['mathfuns'][i];
        delete numeric['ops1'][o];
    }
    for(i in numeric['mapreducers']) {
        if(numeric['mapreducers'].hasOwnProperty(i)) {
            o = numeric['mapreducers'][i];
            numeric[i+'V'] = numeric['mapreduce2'](o[0],o[1]);
            numeric[i] = Function('x','s','k',
                    o[1]+
                    'if(typeof x !== "object") {'+
                    '    xi = x;\n'+
                    o[0]+';\n'+
                    '    return accum;\n'+
                    '}'+
                    'if(typeof s === "undefined") s = numeric[\'dim\'](x);\n'+
                    'if(typeof k === "undefined") k = 0;\n'+
                    'if(k === s.length-1) return numeric[\''+i+'V\'](x);\n'+
                    'var xi;\n'+
                    'var n = x.length, i;\n'+
                    'for(i=n-1;i!==-1;--i) {\n'+
                    '   xi = arguments.callee(x[i]);\n'+
                    o[0]+';\n'+
                    '}\n'+
                    'return accum;\n');
        }
    }
}());

numeric['truncVV'] = numeric['pointwise'](['x[i]','y[i]'],'ret[i] = round(x[i]/y[i])*y[i];','var round = Math.round;');
numeric['truncVS'] = numeric['pointwise'](['x[i]','y'],'ret[i] = round(x[i]/y)*y;','var round = Math.round;');
numeric['truncSV'] = numeric['pointwise'](['x','y[i]'],'ret[i] = round(x/y[i])*y[i];','var round = Math.round;');
numeric['trunc'] = function trunc(x,y) {
    if(typeof x === "object") {
        if(typeof y === "object") return numeric['truncVV'](x,y);
        return numeric['truncVS'](x,y);
    }
    if (typeof y === "object") return numeric['truncSV'](x,y);
    return Math.round(x/y)*y;
}

numeric['inv'] = function inv(x) {
    var s = numeric['dim'](x), abs = Math.abs, m = s[0], n = s[1];
    var A = numeric['clone'](x), Ai, Aj;
    var I = numeric['identity'](m), Ii, Ij;
    var i,j,k,x;
    for(j=0;j<n;++j) {
        var i0 = -1;
        var v0 = -1;
        for(i=j;i!==m;++i) { k = abs(A[i][j]); if(k>v0) { i0 = i; v0 = k; } }
        Aj = A[i0]; A[i0] = A[j]; A[j] = Aj;
        Ij = I[i0]; I[i0] = I[j]; I[j] = Ij;
        x = Aj[j];
        for(k=j;k!==n;++k)    Aj[k] /= x;
        for(k=n-1;k!==-1;--k) Ij[k] /= x;
        for(i=m-1;i!==-1;--i) {
            if(i!==j) {
                Ai = A[i];
                Ii = I[i];
                x = Ai[j];
                for(k=j+1;k!==n;++k)  Ai[k] -= Aj[k]*x;
                for(k=n-1;k>0;--k) { Ii[k] -= Ij[k]*x; --k; Ii[k] -= Ij[k]*x; }
                if(k===0) Ii[0] -= Ij[0]*x;
            }
        }
    }
    return I;
}

numeric['det'] = function det(x) {
    var s = numeric['dim'](x);
    if(s.length !== 2 || s[0] !== s[1]) { throw new Error('numeric: det() only works on square matrices'); }
    var n = s[0], ret = 1,i,j,k,A = numeric['clone'](x),Aj,Ai,alpha,temp,k1,k2,k3;
    for(j=0;j<n-1;j++) {
        k=j;
        for(i=j+1;i<n;i++) { if(Math.abs(A[i][j]) > Math.abs(A[k][j])) { k = i; } }
        if(k !== j) {
            temp = A[k]; A[k] = A[j]; A[j] = temp;
            ret *= -1;
        }
        Aj = A[j];
        for(i=j+1;i<n;i++) {
            Ai = A[i];
            alpha = Ai[j]/Aj[j];
            for(k=j+1;k<n-1;k+=2) {
                k1 = k+1;
                Ai[k] -= Aj[k]*alpha;
                Ai[k1] -= Aj[k1]*alpha;
            }
            if(k!==n) { Ai[k] -= Aj[k]*alpha; }
        }
        if(Aj[j] === 0) { return 0; }
        ret *= Aj[j];
    }
    return ret*A[j][j];
}

numeric['transpose'] = function transpose(x) {
    var i,j,m = x.length,n = x[0].length, ret=Array(n),A0,A1,Bj;
    for(j=0;j<n;j++) ret[j] = Array(m);
    for(i=m-1;i>=1;i-=2) {
        A1 = x[i];
        A0 = x[i-1];
        for(j=n-1;j>=1;--j) {
            Bj = ret[j]; Bj[i] = A1[j]; Bj[i-1] = A0[j];
            --j;
            Bj = ret[j]; Bj[i] = A1[j]; Bj[i-1] = A0[j];
        }
        if(j===0) {
            Bj = ret[0]; Bj[i] = A1[0]; Bj[i-1] = A0[0];
        }
    }
    if(i===0) {
        A0 = x[0];
        for(j=n-1;j>=1;--j) {
            ret[j][0] = A0[j];
            --j;
            ret[j][0] = A0[j];
        }
        if(j===0) { ret[0][0] = A0[0]; }
    }
    return ret;
}
numeric['negtranspose'] = function negtranspose(x) {
    var i,j,m = x.length,n = x[0].length, ret=Array(n),A0,A1,Bj;
    for(j=0;j<n;j++) ret[j] = Array(m);
    for(i=m-1;i>=1;i-=2) {
        A1 = x[i];
        A0 = x[i-1];
        for(j=n-1;j>=1;--j) {
            Bj = ret[j]; Bj[i] = -A1[j]; Bj[i-1] = -A0[j];
            --j;
            Bj = ret[j]; Bj[i] = -A1[j]; Bj[i-1] = -A0[j];
        }
        if(j===0) {
            Bj = ret[0]; Bj[i] = -A1[0]; Bj[i-1] = -A0[0];
        }
    }
    if(i===0) {
        A0 = x[0];
        for(j=n-1;j>=1;--j) {
            ret[j][0] = -A0[j];
            --j;
            ret[j][0] = -A0[j];
        }
        if(j===0) { ret[0][0] = -A0[0]; }
    }
    return ret;
}

numeric['_random'] = function _random(s,k) {
    var i,n=s[k],ret=Array(n), rnd;
    if(k === s.length-1) {
        rnd = Math.random;
        for(i=n-1;i>=1;i-=2) {
            ret[i] = rnd();
            ret[i-1] = rnd();
        }
        if(i===0) { ret[0] = rnd(); }
        return ret;
    }
    for(i=n-1;i>=0;i--) ret[i] = _random(s,k+1);
    return ret;
}
numeric['random'] = function random(s) { return numeric['_random'](s,0); }

numeric['norm2'] = function norm2(x) { return Math.sqrt(numeric['norm2Squared'](x)); }

numeric['linspace'] = function linspace(a,b,n) {
    if(typeof n === "undefined") n = Math.max(Math.round(b-a)+1,1);
    if(n<2) { return n===1?[a]:[]; }
    var i,ret = Array(n);
    n--;
    for(i=n;i>=0;i--) { ret[i] = (i*b+(n-i)*a)/n; }
    return ret;
}

numeric['getBlock'] = function getBlock(x,from,to) {
    var s = numeric['dim'](x);
    function foo(x,k) {
        var i,a = from[k], n = to[k]-a, ret = Array(n);
        if(k === s.length-1) {
            for(i=n;i>=0;i--) { ret[i] = x[i+a]; }
            return ret;
        }
        for(i=n;i>=0;i--) { ret[i] = foo(x[i+a],k+1); }
        return ret;
    }
    return foo(x,0);
}

numeric['setBlock'] = function setBlock(x,from,to,B) {
    var s = numeric['dim'](x);
    function foo(x,y,k) {
        var i,a = from[k], n = to[k]-a;
        if(k === s.length-1) { for(i=n;i>=0;i--) { x[i+a] = y[i]; } }
        for(i=n;i>=0;i--) { foo(x[i+a],y[i],k+1); }
    }
    foo(x,B,0);
    return x;
}

numeric['getRange'] = function getRange(A,I,J) {
    var m = I.length, n = J.length;
    var i,j;
    var B = Array(m), Bi, AI;
    for(i=m-1;i!==-1;--i) {
        B[i] = Array(n);
        Bi = B[i];
        AI = A[I[i]];
        for(j=n-1;j!==-1;--j) Bi[j] = AI[J[j]];
    }
    return B;
}

numeric['blockMatrix'] = function blockMatrix(X) {
    var s = numeric['dim'](X);
    if(s.length<4) return numeric['blockMatrix']([X]);
    var m=s[0],n=s[1],M,N,i,j,Xij;
    M = 0; N = 0;
    for(i=0;i<m;++i) M+=X[i][0].length;
    for(j=0;j<n;++j) N+=X[0][j][0].length;
    var Z = Array(M);
    for(i=0;i<M;++i) Z[i] = Array(N);
    var I=0,J,ZI,k,l,Xijk;
    for(i=0;i<m;++i) {
        J=N;
        for(j=n-1;j!==-1;--j) {
            Xij = X[i][j];
            J -= Xij[0].length;
            for(k=Xij.length-1;k!==-1;--k) {
                Xijk = Xij[k];
                ZI = Z[I+k];
                for(l = Xijk.length-1;l!==-1;--l) ZI[J+l] = Xijk[l];
            }
        }
        I += X[i][0].length;
    }
    return Z;
}

numeric['tensor'] = function tensor(x,y) {
    if(typeof x === "number" || typeof y === "number") return numeric['mul'](x,y);
    var s1 = numeric['dim'](x), s2 = numeric['dim'](y);
    if(s1.length !== 1 || s2.length !== 1) {
        throw new Error('numeric: tensor product is only defined for vectors');
    }
    var m = s1[0], n = s2[0], A = Array(m), Ai, i,j,xi;
    for(i=m-1;i>=0;i--) {
        Ai = Array(n);
        xi = x[i];
        for(j=n-1;j>=3;--j) {
            Ai[j] = xi * y[j];
            --j;
            Ai[j] = xi * y[j];
            --j;
            Ai[j] = xi * y[j];
            --j;
            Ai[j] = xi * y[j];
        }
        while(j>=0) { Ai[j] = xi * y[j]; --j; }
        A[i] = Ai;
    }
    return A;
}

// 3. The Tensor type T
numeric['T'] = function T(x,y) { this.x = x; this.y = y; }
numeric['t'] = function t(x,y) { return new numeric['T'](x,y); }

numeric['Tbinop'] = function Tbinop(rr,rc,cr,cc,setup) {
    var io = numeric['indexOf'];
    if(typeof setup !== "string") {
        var k;
        setup = '';
        for(k in numeric) {
            if(numeric['hasOwnProperty'](k) && (rr.indexOf(k)>=0 || rc.indexOf(k)>=0 || cr.indexOf(k)>=0 || cc.indexOf(k)>=0) && k.length>1) {
                setup += 'var '+k+' = numeric.'+k+';\n';
            }
        }
    }
    return Function(['y'],
            'var x = this;\n'+
            'if(!(y instanceof numeric[\'T\'])) { y = new numeric[\'T\'](y); }\n'+
            setup+'\n'+
            'if(x.y) {'+
            '  if(y.y) {'+
            '    return new numeric[\'T\']('+cc+');\n'+
            '  }\n'+
            '  return new numeric[\'T\']('+cr+');\n'+
            '}\n'+
            'if(y.y) {\n'+
            '  return new numeric[\'T\']('+rc+');\n'+
            '}\n'+
            'return new numeric[\'T\']('+rr+');\n'
    );
}

numeric['T'].prototype.add = numeric['Tbinop'](
        'add(x.x,y.x)',
        'add(x.x,y.x),y.y',
        'add(x.x,y.x),x.y',
        'add(x.x,y.x),add(x.y,y.y)');
numeric['T'].prototype.sub = numeric['Tbinop'](
        'sub(x.x,y.x)',
        'sub(x.x,y.x),neg(y.y)',
        'sub(x.x,y.x),x.y',
        'sub(x.x,y.x),sub(x.y,y.y)');
numeric['T'].prototype.mul = numeric['Tbinop'](
        'mul(x.x,y.x)',
        'mul(x.x,y.x),mul(x.x,y.y)',
        'mul(x.x,y.x),mul(x.y,y.x)',
        'sub(mul(x.x,y.x),mul(x.y,y.y)),add(mul(x.x,y.y),mul(x.y,y.x))');

numeric['T'].prototype.reciprocal = function reciprocal() {
    var mul = numeric['mul'], div = numeric['div'];
    if(this.y) {
        var d = numeric['add'](mul(this.x,this.x),mul(this.y,this.y));
        return new numeric['T'](div(this.x,d),div(numeric['neg'](this.y),d));
    }
    return new T(div(1,this.x));
}
numeric['T'].prototype.div = function div(y) {
    if(!(y instanceof numeric['T'])) y = new numeric['T'](y);
    if(y.y) { return this.mul(y.reciprocal()); }
    var div = numeric['div'];
    if(this.y) { return new numeric['T'](div(this.x,y.x),div(this.y,y.x)); }
    return new numeric['T'](div(this.x,y.x));
}
numeric['T'].prototype.dot = numeric['Tbinop'](
        'dot(x.x,y.x)',
        'dot(x.x,y.x),dot(x.x,y.y)',
        'dot(x.x,y.x),dot(x.y,y.x)',
        'sub(dot(x.x,y.x),dot(x.y,y.y)),add(dot(x.x,y.y),dot(x.y,y.x))'
        );
numeric['T'].prototype.transpose = function transpose() {
    var t = numeric['transpose'], x = this.x, y = this.y;
    if(y) { return new numeric['T'](t(x),t(y)); }
    return new numeric['T'](t(x));
}
numeric['T'].prototype.transjugate = function transjugate() {
    var t = numeric['transpose'], x = this.x, y = this.y;
    if(y) { return new numeric['T'](t(x),numeric['negtranspose'](y)); }
    return new numeric['T'](t(x));
}
numeric['Tunop'] = function Tunop(r,c,s) {
    if(typeof s !== "string") { s = ''; }
    return Function(
            'var x = this;\n'+
            s+'\n'+
            'if(x.y) {'+
            '  '+c+';\n'+
            '}\n'+
            r+';\n'
    );
}

numeric['T'].prototype.exp = numeric['Tunop'](
        'return new numeric[\'T\'](ex)',
        'return new numeric[\'T\'](mul(cos(x.y),ex),mul(sin(x.y),ex))',
        'var ex = numeric[\'exp\'](x.x), cos = numeric[\'cos\'], sin = numeric[\'sin\'], mul = numeric[\'mul\'];');
numeric['T'].prototype.conj = numeric['Tunop'](
        'return new numeric[\'T\'](x.x);',
        'return new numeric[\'T\'](x.x,numeric[\'neg\'](x.y));');
numeric['T'].prototype.neg = numeric['Tunop'](
        'return new numeric[\'T\'](neg(x.x));',
        'return new numeric[\'T\'](neg(x.x),neg(x.y));',
        'var neg = numeric[\'neg\'];');
numeric['T'].prototype.sin = numeric['Tunop'](
        'return new numeric[\'T\'](numeric[\'sin\'](x.x))',
        'return x.exp().sub(x.neg().exp()).div(new numeric[\'T\'](0,2));');
numeric['T'].prototype.cos = numeric['Tunop'](
        'return new numeric[\'T\'](numeric[\'cos\'](x.x))',
        'return x.exp().add(x.neg().exp()).div(2);');
numeric['T'].prototype.abs = numeric['Tunop'](
        'return new numeric[\'T\'](numeric[\'abs\'](x.x));',
        'return new numeric[\'T\'](numeric[\'sqrt\'](numeric[\'add\'](mul(x.x,x.x),mul(x.y,x.y))));',
        'var mul = numeric[\'mul\'];');
numeric['T'].prototype.log = numeric['Tunop'](
        'return new numeric[\'T\'](numeric[\'log\'](x.x));',
        'var theta = new numeric[\'T\'](numeric[\'atan2\'](x.y,x.x)), r = x.abs();\n'+
        'return new numeric[\'T\'](numeric[\'log\'](r.x),theta.x);');
numeric['T'].prototype.norm2 = numeric['Tunop'](
        'return numeric[\'norm2\'](x.x);',
        'var f = numeric[\'norm2Squared\'];\n'+
        'return Math.sqrt(f(x.x)+f(x.y));');
numeric['T'].prototype.inv = function inv() {
    var A = this;
    if(typeof A.y === "undefined") { return new numeric['T'](numeric['inv'](A.x)); }
    var n = A.x.length, i, j, k;
    var Rx = numeric['identity'](n),Ry = numeric['rep']([n,n],0);
    var Ax = numeric['clone'](A.x), Ay = numeric['clone'](A.y);
    var Aix, Aiy, Ajx, Ajy, Rix, Riy, Rjx, Rjy;
    var i,j,k,d,d1,ax,ay,bx,by,temp;
    for(i=0;i<n;i++) {
        ax = Ax[i][i]; ay = Ay[i][i];
        d = ax*ax+ay*ay;
        k = i;
        for(j=i+1;j<n;j++) {
            ax = Ax[j][i]; ay = Ay[j][i];
            d1 = ax*ax+ay*ay;
            if(d1 > d) { k=j; d = d1; }
        }
        if(k!==i) {
            temp = Ax[i]; Ax[i] = Ax[k]; Ax[k] = temp;
            temp = Ay[i]; Ay[i] = Ay[k]; Ay[k] = temp;
            temp = Rx[i]; Rx[i] = Rx[k]; Rx[k] = temp;
            temp = Ry[i]; Ry[i] = Ry[k]; Ry[k] = temp;
        }
        Aix = Ax[i]; Aiy = Ay[i];
        Rix = Rx[i]; Riy = Ry[i];
        ax = Aix[i]; ay = Aiy[i];
        for(j=i+1;j<n;j++) {
            bx = Aix[j]; by = Aiy[j];
            Aix[j] = (bx*ax+by*ay)/d;
            Aiy[j] = (by*ax-bx*ay)/d;
        }
        for(j=0;j<n;j++) {
            bx = Rix[j]; by = Riy[j];
            Rix[j] = (bx*ax+by*ay)/d;
            Riy[j] = (by*ax-bx*ay)/d;
        }
        for(j=i+1;j<n;j++) {
            Ajx = Ax[j]; Ajy = Ay[j];
            Rjx = Rx[j]; Rjy = Ry[j];
            ax = Ajx[i]; ay = Ajy[i];
            for(k=i+1;k<n;k++) {
                bx = Aix[k]; by = Aiy[k];
                Ajx[k] -= bx*ax-by*ay;
                Ajy[k] -= by*ax+bx*ay;
            }
            for(k=0;k<n;k++) {
                bx = Rix[k]; by = Riy[k];
                Rjx[k] -= bx*ax-by*ay;
                Rjy[k] -= by*ax+bx*ay;
            }
        }
    }
    for(i=n-1;i>0;i--) {
        Rix = Rx[i]; Riy = Ry[i];
        for(j=i-1;j>=0;j--) {
            Rjx = Rx[j]; Rjy = Ry[j];
            ax = Ax[j][i]; ay = Ay[j][i];
            for(k=n-1;k>=0;k--) {
                bx = Rix[k]; by = Riy[k];
                Rjx[k] -= ax*bx - ay*by;
                Rjy[k] -= ax*by + ay*bx;
            }
        }
    }
    return new numeric['T'](Rx,Ry);
}
numeric['T'].prototype.get = function get(i) {
    var x = this.x, y = this.y, k = 0, ik, n = i.length;
    if(y) {
        while(k<n) {
            ik = i[k];
            x = x[ik];
            y = y[ik];
            k++;
        }
        return new numeric['T'](x,y);
    }
    while(k<n) {
        ik = i[k];
        x = x[ik];
        k++;
    }
    return new numeric['T'](x);
}
numeric['T'].prototype.set = function set(i,v) {
    var x = this.x, y = this.y, k = 0, ik, n = i.length, vx = v.x, vy = v.y;
    if(n===0) {
        if(vy) { this.y = vy; }
        else if(y) { this.y = undefined; }
        this.x = x;
        return this;
    }
    if(vy) {
        if(y) { /* ok */ }
        else {
            y = numeric['rep'](numeric['dim'](x),0);
            this.y = y;
        }
        while(k<n-1) {
            ik = i[k];
            x = x[ik];
            y = y[ik];
            k++;
        }
        ik = i[k];
        x[ik] = vx;
        y[ik] = vy;
        return this;
    }
    if(y) {
        while(k<n-1) {
            ik = i[k];
            x = x[ik];
            y = y[ik];
            k++;
        }
        ik = i[k];
        x[ik] = vx;
        if(vx instanceof Array) y[ik] = numeric['rep'](numeric['dim'](vx),0);
        else y[ik] = 0;
        return this;
    }
    while(k<n-1) {
        ik = i[k];
        x = x[ik];
        k++;
    }
    ik = i[k];
    x[ik] = vx;
    return this;
}
numeric['T'].prototype.getRows = function getRows(i0,i1) {
    var n = i1-i0+1, j;
    var rx = Array(n), ry, x = this.x, y = this.y;
    for(j=i0;j<=i1;j++) { rx[j-i0] = x[j]; }
    if(y) {
        ry = Array(n);
        for(j=i0;j<=i1;j++) { ry[j-i0] = y[j]; }
        return new numeric['T'](rx,ry);
    }
    return new numeric['T'](rx);
}
numeric['T'].prototype.setRows = function setRows(i0,i1,A) {
    var j;
    var rx = this.x, ry = this.y, x = A.x, y = A.y;
    for(j=i0;j<=i1;j++) { rx[j] = x[j-i0]; }
    if(y) {
        if(!ry) { ry = numeric['rep'](numeric['dim'](rx),0); this.y = ry; }
        for(j=i0;j<=i1;j++) { ry[j] = y[j-i0]; }
    } else if(ry) {
        for(j=i0;j<=i1;j++) { ry[j] = numeric['rep']([x[j-i0].length],0); }
    }
    return this;
}
numeric['T'].prototype.getRow = function getRow(k) {
    var x = this.x, y = this.y;
    if(y) { return new numeric['T'](x[k],y[k]); }
    return new numeric['T'](x[k]);
}
numeric['T'].prototype.setRow = function setRow(i,v) {
    var rx = this.x, ry = this.y, x = v.x, y = v.y;
    rx[i] = x;
    if(y) {
        if(!ry) { ry = numeric['rep'](numeric['dim'](rx),0); this.y = ry; }
        ry[i] = y;
    } else if(ry) {
        ry = numeric['rep']([x.length],0);
    }
    return this;
}

numeric['T'].prototype.getBlock = function getBlock(from,to) {
    var x = this.x, y = this.y, b = numeric['getBlock'];
    if(y) { return new numeric['T'](b(x,from,to),b(y,from,to)); }
    return new numeric['T'](b(x,from,to));
}
numeric['T'].prototype.setBlock = function setBlock(from,to,A) {
    if(!(A instanceof numeric['T'])) A = new numeric['T'](A);
    var x = this.x, y = this.y, b = numeric['setBlock'], Ax = A.x, Ay = A.y;
    if(Ay) {
        if(!y) { this.y = numeric['rep'](numeric['dim'](this),0); y = this.y; }
        b(x,from,to,Ax);
        b(y,from,to,Ay);
        return this;
    }
    b(x,from,to,Ax);
    if(y) b(y,from,to,numeric['rep'](numeric['dim'](Ax),0));
}
numeric['T'].rep = function rep(s,v) {
    var T = numeric['T'];
    if(!(v instanceof T)) v = new T(v);
    var x = v.x, y = v.y, r = numeric['rep'];
    if(y) return new T(r(s,x),r(s,y));
    return new T(r(s,x));
}
numeric['T'].diag = function diag(d) {
    if(!(d instanceof numeric['T'])) d = new numeric['T'](d);
    var x = d.x, y = d.y, diag = numeric['diag'];
    if(y) return new numeric['T'](diag(x),diag(y));
    return new numeric['T'](diag(x));
}
numeric['T'].eig = function eig() {
    if(this.y) { throw new Error('eig: not implemented for complex matrices.'); }
    return numeric['eig'](this.x);
}
numeric['T'].identity = function identity(n) { return new numeric['T'](numeric['identity'](n)); }
numeric['T'].prototype.getDiag = function getDiag() {
    var n = numeric;
    var x = this.x, y = this.y;
    if(y) { return new n.T(n.getDiag(x),n.getDiag(y)); }
    return new n.T(n.getDiag(x));
}

// 4. Eigenvalues of real matrices

numeric['house'] = function house(x) {
    var v = numeric['clone'](x);
    var s = x[0] >= 0 ? 1 : -1;
    var alpha = s*numeric['norm2'](x);
    v[0] += alpha;
    var foo = numeric['norm2'](v);
    if(foo === 0) { /* this should not happen */ throw new Error('eig: internal error'); }
    return numeric['div'](v,foo);
}

numeric['toUpperHessenberg'] = function toUpperHessenberg(me) {
    var s = numeric['dim'](me);
    if(s.length !== 2 || s[0] !== s[1]) { throw new Error('numeric: toUpperHessenberg() only works on square matrices'); }
    var m = s[0], i,j,k,x,v,A = numeric['clone'](me),B,C,Ai,Ci,Q = numeric['identity'](m),Qi;
    for(j=0;j<m-2;j++) {
        x = Array(m-j-1);
        for(i=j+1;i<m;i++) { x[i-j-1] = A[i][j]; }
        if(numeric['norm2'](x)>0) {
            v = numeric['house'](x);
            B = numeric['getBlock'](A,[j+1,j],[m-1,m-1]);
            C = numeric['tensor'](v,numeric['dot'](v,B));
            for(i=j+1;i<m;i++) { Ai = A[i]; Ci = C[i-j-1]; for(k=j;k<m;k++) Ai[k] -= 2*Ci[k-j]; }
            B = numeric['getBlock'](A,[0,j+1],[m-1,m-1]);
            C = numeric['tensor'](numeric['dot'](B,v),v);
            for(i=0;i<m;i++) { Ai = A[i]; Ci = C[i]; for(k=j+1;k<m;k++) Ai[k] -= 2*Ci[k-j-1]; }
            B = Array(m-j-1);
            for(i=j+1;i<m;i++) B[i-j-1] = Q[i];
            C = numeric['tensor'](v,numeric['dot'](v,B));
            for(i=j+1;i<m;i++) { Qi = Q[i]; Ci = C[i-j-1]; for(k=0;k<m;k++) Qi[k] -= 2*Ci[k]; }
        }
    }
    return {H:A, Q:Q};
}

numeric['epsilon'] = 2.220446049250313e-16;

numeric['QRFrancis'] = function(H,maxiter) {
    if(typeof maxiter === "undefined") { maxiter = 10000; }
    H = numeric['clone'](H);
    var H0 = numeric['clone'](H);
    var s = numeric['dim'](H),m=s[0],x,v,a,b,c,d,det,tr, Hloc, Q = numeric['identity'](m), Qi, Hi, B, C, Ci,i,j,k,iter;
    if(m<3) { return {Q:Q, B:[ [0,m-1] ]}; }
    var epsilon = numeric['epsilon'];
    for(iter=0;iter<maxiter;iter++) {
        for(j=0;j<m-1;j++) {
            if(Math.abs(H[j+1][j]) < epsilon*(Math.abs(H[j][j])+Math.abs(H[j+1][j+1]))) {
                var QH1 = numeric['QRFrancis'](numeric['getBlock'](H,[0,0],[j,j]),maxiter);
                var QH2 = numeric['QRFrancis'](numeric['getBlock'](H,[j+1,j+1],[m-1,m-1]),maxiter);
                B = Array(j+1);
                for(i=0;i<=j;i++) { B[i] = Q[i]; }
                C = numeric['dot'](QH1.Q,B);
                for(i=0;i<=j;i++) { Q[i] = C[i]; }
                B = Array(m-j-1);
                for(i=j+1;i<m;i++) { B[i-j-1] = Q[i]; }
                C = numeric['dot'](QH2.Q,B);
                for(i=j+1;i<m;i++) { Q[i] = C[i-j-1]; }
                return {Q:Q,B:QH1.B.concat(numeric['add'](QH2.B,j+1))};
            }
        }
        a = H[m-2][m-2]; b = H[m-2][m-1];
        c = H[m-1][m-2]; d = H[m-1][m-1];
        tr = a+d;
        det = (a*d-b*c);
        Hloc = numeric['getBlock'](H, [0,0], [2,2]);
        if(tr*tr>=4*det) {
            var s1,s2;
            s1 = 0.5*(tr+Math.sqrt(tr*tr-4*det));
            s2 = 0.5*(tr-Math.sqrt(tr*tr-4*det));
            Hloc = numeric['add'](numeric['sub'](numeric['dot'](Hloc,Hloc),
                                           numeric['mul'](Hloc,s1+s2)),
                               numeric['diag'](numeric['rep']([3],s1*s2)));
        } else {
            Hloc = numeric['add'](numeric['sub'](numeric['dot'](Hloc,Hloc),
                                           numeric['mul'](Hloc,tr)),
                               numeric['diag'](numeric['rep']([3],det)));
        }
        x = [Hloc[0][0],Hloc[1][0],Hloc[2][0]];
        v = numeric['house'](x);
        B = [H[0],H[1],H[2]];
        C = numeric['tensor'](v,numeric['dot'](v,B));
        for(i=0;i<3;i++) { Hi = H[i]; Ci = C[i]; for(k=0;k<m;k++) Hi[k] -= 2*Ci[k]; }
        B = numeric['getBlock'](H, [0,0],[m-1,2]);
        C = numeric['tensor'](numeric['dot'](B,v),v);
        for(i=0;i<m;i++) { Hi = H[i]; Ci = C[i]; for(k=0;k<3;k++) Hi[k] -= 2*Ci[k]; }
        B = [Q[0],Q[1],Q[2]];
        C = numeric['tensor'](v,numeric['dot'](v,B));
        for(i=0;i<3;i++) { Qi = Q[i]; Ci = C[i]; for(k=0;k<m;k++) Qi[k] -= 2*Ci[k]; }
        var J;
        for(j=0;j<m-2;j++) {
            for(k=j;k<=j+1;k++) {
                if(Math.abs(H[k+1][k]) < epsilon*(Math.abs(H[k][k])+Math.abs(H[k+1][k+1]))) {
                    var QH1 = numeric['QRFrancis'](numeric['getBlock'](H,[0,0],[k,k]),maxiter);
                    var QH2 = numeric['QRFrancis'](numeric['getBlock'](H,[k+1,k+1],[m-1,m-1]),maxiter);
                    B = Array(k+1);
                    for(i=0;i<=k;i++) { B[i] = Q[i]; }
                    C = numeric['dot'](QH1.Q,B);
                    for(i=0;i<=k;i++) { Q[i] = C[i]; }
                    B = Array(m-k-1);
                    for(i=k+1;i<m;i++) { B[i-k-1] = Q[i]; }
                    C = numeric['dot'](QH2.Q,B);
                    for(i=k+1;i<m;i++) { Q[i] = C[i-k-1]; }
                    return {Q:Q,B:QH1.B.concat(numeric['add'](QH2.B,k+1))};
                }
            }
            J = Math.min(m-1,j+3);
            x = Array(J-j);
            for(i=j+1;i<=J;i++) { x[i-j-1] = H[i][j]; }
            v = numeric['house'](x);
            B = numeric['getBlock'](H, [j+1,j],[J,m-1]);
            C = numeric['tensor'](v,numeric['dot'](v,B));
            for(i=j+1;i<=J;i++) { Hi = H[i]; Ci = C[i-j-1]; for(k=j;k<m;k++) Hi[k] -= 2*Ci[k-j]; }
            B = numeric['getBlock'](H, [0,j+1],[m-1,J]);
            C = numeric['tensor'](numeric['dot'](B,v),v);
            for(i=0;i<m;i++) { Hi = H[i]; Ci = C[i]; for(k=j+1;k<=J;k++) Hi[k] -= 2*Ci[k-j-1]; }
            B = Array(J-j);
            for(i=j+1;i<=J;i++) B[i-j-1] = Q[i];
            C = numeric['tensor'](v,numeric['dot'](v,B));
            for(i=j+1;i<=J;i++) { Qi = Q[i]; Ci = C[i-j-1]; for(k=0;k<m;k++) Qi[k] -= 2*Ci[k]; }
        }
    }
    throw new Error('numeric: eigenvalue iteration does not converge -- increase maxiter?');
}

numeric['eig'] = function eig(A,maxiter) {
    var QH = numeric['toUpperHessenberg'](A);
    var QB = numeric['QRFrancis'](QH.H,maxiter);
    var T = numeric['T'];
    var n = A.length,i,k,flag = false,B = QB.B,H = numeric['dot'](QB.Q,numeric['dot'](QH.H,numeric['transpose'](QB.Q)));
    var Q = new T(numeric['dot'](QB.Q,QH.Q)),Q0;
    var m = B.length,j;
    var a,b,c,d,p1,p2,disc,x,y,p,q,n1,n2;
    var sqrt = Math.sqrt;
    for(k=0;k<m;k++) {
        i = B[k][0];
        if(i === B[k][1]) {
            // nothing
        } else {
            j = i+1;
            a = H[i][i];
            b = H[i][j];
            c = H[j][i];
            d = H[j][j];
            if(b === 0 && c === 0) continue;
            p1 = -a-d;
            p2 = a*d-b*c;
            disc = p1*p1-4*p2;
            if(disc>=0) {
                if(p1<0) x = -0.5*(p1-sqrt(disc));
                else     x = -0.5*(p1+sqrt(disc));
                n1 = (a-x)*(a-x)+b*b;
                n2 = c*c+(d-x)*(d-x);
                if(n1>n2) {
                    n1 = sqrt(n1);
                    p = (a-x)/n1;
                    q = b/n1;
                } else {
                    n2 = sqrt(n2);
                    p = c/n2;
                    q = (d-x)/n2;
                }
                Q0 = new T([[q,-p],[p,q]]);
                Q.setRows(i,j,Q0.dot(Q.getRows(i,j)));
            } else {
                x = -0.5*p1;
                y = 0.5*sqrt(-disc);
                n1 = (a-x)*(a-x)+b*b;
                n2 = c*c+(d-x)*(d-x);
                if(n1>n2) {
                    n1 = sqrt(n1+y*y);
                    p = (a-x)/n1;
                    q = b/n1;
                    x = 0;
                    y /= n1;
                } else {
                    n2 = sqrt(n2+y*y);
                    p = c/n2;
                    q = (d-x)/n2;
                    x = y/n2;
                    y = 0;
                }
                Q0 = new T([[q,-p],[p,q]],[[x,y],[y,-x]]);
                Q.setRows(i,j,Q0.dot(Q.getRows(i,j)));
            }
        }
    }
    var R = Q.dot(A).dot(Q.transjugate()), n = A.length, E = numeric['T'].identity(n);
    for(j=0;j<n;j++) {
        if(j>0) {
            for(k=j-1;k>=0;k--) {
                var Rk = R.get([k,k]), Rj = R.get([j,j]);
                if(numeric['neq'](Rk.x,Rj.x) || numeric['neq'](Rk.y,Rj.y)) {
                    x = R.getRow(k).getBlock([k],[j-1]);
                    y = E.getRow(j).getBlock([k],[j-1]);
                    E.set([j,k],(R.get([k,j]).neg().sub(x.dot(y))).div(Rk.sub(Rj)));
                } else {
                    E.setRow(j,E.getRow(k));
                    continue;
                }
            }
        }
    }
    for(j=0;j<n;j++) {
        x = E.getRow(j);
        E.setRow(j,x.div(x.norm2()));
    }
    E = E.transpose();
    E = Q.transjugate().dot(E);
    return { lambda:R.getDiag(), E:E };
};
