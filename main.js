"use strict";
// This object will hold all exports.
var Haste = {};
if(typeof window === 'undefined') window = global;

/* Constructor functions for small ADTs. */
function T0(t){this._=t;}
function T1(t,a){this._=t;this.a=a;}
function T2(t,a,b){this._=t;this.a=a;this.b=b;}
function T3(t,a,b,c){this._=t;this.a=a;this.b=b;this.c=c;}
function T4(t,a,b,c,d){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;}
function T5(t,a,b,c,d,e){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;}
function T6(t,a,b,c,d,e,f){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;this.f=f;}

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// "Zero" object; used to avoid creating a whole bunch of new objects
// in the extremely common case of a nil-like data constructor.
var __Z = new T0(0);

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

// Indicates that a closure-creating tail loop isn't done.
var __continue = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof Function) {
            if(args.length === f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else if(f instanceof PAP) {
            if(args.length === f.arity) {
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                return new PAP(f.f, f.args.concat(args));
            } else {
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

function A1(f, x) {
    f = E(f);
    if(f instanceof Function) {
        return f.length === 1 ? f(x) : new PAP(f, [x]);
    } else if(f instanceof PAP) {
        return f.arity === 1 ? f.f.apply(null, f.args.concat([x]))
                             : new PAP(f.f, f.args.concat([x]));
    } else {
        return f;
    }
}

function A2(f, x, y) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 2:  return f(x, y);
        case 1:  return A1(B(f(x)), y);
        default: return new PAP(f, [x,y]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 2:  return f.f.apply(null, f.args.concat([x,y]));
        case 1:  return A1(B(f.f.apply(null, f.args.concat([x]))), y);
        default: return new PAP(f.f, f.args.concat([x,y]));
        }
    } else {
        return f;
    }
}

function A3(f, x, y, z) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 3:  return f(x, y, z);
        case 2:  return A1(B(f(x, y)), z);
        case 1:  return A2(B(f(x)), y, z);
        default: return new PAP(f, [x,y,z]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 3:  return f.f.apply(null, f.args.concat([x,y,z]));
        case 2:  return A1(B(f.f.apply(null, f.args.concat([x,y]))), z);
        case 1:  return A2(B(f.f.apply(null, f.args.concat([x]))), y, z);
        default: return new PAP(f.f, f.args.concat([x,y,z]));
        }
    } else {
        return f;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            if(t.x === __updatable) {
                var f = t.f;
                t.f = __blackhole;
                t.x = f();
            } else {
                return t.f();
            }
        }
        if(t.x === __updatable) {
            throw 'Infinite loop!';
        } else {
            return t.x;
        }
    } else {
        return t;
    }
}

/* Tail call chain counter. */
var C = 0, Cs = [];

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    Cs.push(C);
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        C = 0;
        f = fun();
    }
    C = Cs.pop();
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return {_:0, a:(a-a%b)/b, b:a%b};
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return {_:0, a:x & 0xffffffff, b:x > 0x7fffffff};
}

function subC(a, b) {
    var x = a-b;
    return {_:0, a:x & 0xffffffff, b:x < -2147483648};
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, __Z);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return {_:1,a:str.charCodeAt(i),b:new T(function() {
            return unAppCStr(str,chrs,i+1);
        })};
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str._ == 1; str = E(str.b)) {
        s += String.fromCharCode(E(str.a));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x.a;
    return x.b;
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

/* Convert a JS exception into a Haskell JSException */
function __hsException(e) {
  e = e.toString();
  var x = new Long(2904464383, 3929545892, true);
  var y = new Long(3027541338, 3270546716, true);
  var t = new T5(0, x, y
                  , new T5(0, x, y
                            , unCStr("haste-prim")
                            , unCStr("Haste.Prim.Foreign")
                            , unCStr("JSException")), __Z, __Z);
  var show = function(x) {return unCStr(E(x).a);}
  var dispEx = function(x) {return unCStr("JavaScript exception: " + E(x).a);}
  var showList = function(_, s) {return unAppCStr(e, s);}
  var showsPrec = function(_, _p, s) {return unAppCStr(e, s);}
  var showDict = new T3(0, showsPrec, show, showList);
  var self;
  var fromEx = function(_) {return new T1(1, self);}
  var dict = new T5(0, t, showDict, null /* toException */, fromEx, dispEx);
  self = new T2(0, dict, new T1(0, e));
  return self;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        if(typeof e._ === 'undefined') {
            e = __hsException(e);
        }
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Object) {
        return x._;
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round, rintDouble = jsRound, rintFloat = jsRound;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt64(i) {
    return popCnt(i.low) + popCnt(i.high);
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function __clz(bits, x) {
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    } else {
        return bits - (1 + Math.floor(Math.log(x)/Math.LN2));
    }
}

// TODO: can probably be done much faster with arithmetic tricks like __clz
function __ctz(bits, x) {
    var y = 1;
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    }
    for(var i = 0; i < bits; ++i) {
        if(y & x) {
            return i;
        } else {
            y <<= 1;
        }
    }
    return 0;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    if(x === 0) {
        return __decodedZeroF;
    }
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return {_:0, a:sign*man, b:exp};
}

var __decodedZero = {_:0,a:1,b:0,c:0,d:0};
var __decodedZeroF = {_:0,a:1,b:0};

function decodeDouble(x) {
    if(x === 0) {
        // GHC 7.10+ *really* doesn't like 0 to be represented as anything
        // but zeroes all the way.
        return __decodedZero;
    }
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return {_:0, a:sign, b:manHigh, c:manLow, d:exp};
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs._) {
        strs = E(strs);
        arr.push(E(strs.a));
        strs = E(strs.b);
    }
    return arr.join(sep);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return __Z;
    }
    return {_:1,a:hs};
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return {_:0, a:jsRead(obj)};
    case 'string':
        return {_:1, a:obj};
    case 'boolean':
        return {_:2, a:obj}; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return {_:3, a:arr2lst_json(obj, 0)};
        } else if (obj == null) {
            return {_:5};
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = {_:1, a:{_:0, a:ks[i], b:toHS(obj[ks[i]])}, b:xs};
            }
            return {_:4, a:xs};
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1, a:toHS(arr[elem]), b:new T(function() {return arr2lst_json(arr,elem+1);}),c:true}
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

/* bn.js by Fedor Indutny, see doc/LICENSE.bn for license */
var __bn = {};
(function (module, exports) {
'use strict';

function BN(number, base, endian) {
  // May be `new BN(bn)` ?
  if (number !== null &&
      typeof number === 'object' &&
      Array.isArray(number.words)) {
    return number;
  }

  this.negative = 0;
  this.words = null;
  this.length = 0;

  if (base === 'le' || base === 'be') {
    endian = base;
    base = 10;
  }

  if (number !== null)
    this._init(number || 0, base || 10, endian || 'be');
}
if (typeof module === 'object')
  module.exports = BN;
else
  exports.BN = BN;

BN.BN = BN;
BN.wordSize = 26;

BN.max = function max(left, right) {
  if (left.cmp(right) > 0)
    return left;
  else
    return right;
};

BN.min = function min(left, right) {
  if (left.cmp(right) < 0)
    return left;
  else
    return right;
};

BN.prototype._init = function init(number, base, endian) {
  if (typeof number === 'number') {
    return this._initNumber(number, base, endian);
  } else if (typeof number === 'object') {
    return this._initArray(number, base, endian);
  }
  if (base === 'hex')
    base = 16;

  number = number.toString().replace(/\s+/g, '');
  var start = 0;
  if (number[0] === '-')
    start++;

  if (base === 16)
    this._parseHex(number, start);
  else
    this._parseBase(number, base, start);

  if (number[0] === '-')
    this.negative = 1;

  this.strip();

  if (endian !== 'le')
    return;

  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initNumber = function _initNumber(number, base, endian) {
  if (number < 0) {
    this.negative = 1;
    number = -number;
  }
  if (number < 0x4000000) {
    this.words = [ number & 0x3ffffff ];
    this.length = 1;
  } else if (number < 0x10000000000000) {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff
    ];
    this.length = 2;
  } else {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff,
      1
    ];
    this.length = 3;
  }

  if (endian !== 'le')
    return;

  // Reverse the bytes
  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initArray = function _initArray(number, base, endian) {
  if (number.length <= 0) {
    this.words = [ 0 ];
    this.length = 1;
    return this;
  }

  this.length = Math.ceil(number.length / 3);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  var off = 0;
  if (endian === 'be') {
    for (var i = number.length - 1, j = 0; i >= 0; i -= 3) {
      var w = number[i] | (number[i - 1] << 8) | (number[i - 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  } else if (endian === 'le') {
    for (var i = 0, j = 0; i < number.length; i += 3) {
      var w = number[i] | (number[i + 1] << 8) | (number[i + 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  }
  return this.strip();
};

function parseHex(str, start, end) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r <<= 4;

    // 'a' - 'f'
    if (c >= 49 && c <= 54)
      r |= c - 49 + 0xa;

    // 'A' - 'F'
    else if (c >= 17 && c <= 22)
      r |= c - 17 + 0xa;

    // '0' - '9'
    else
      r |= c & 0xf;
  }
  return r;
}

BN.prototype._parseHex = function _parseHex(number, start) {
  // Create possibly bigger array to ensure that it fits the number
  this.length = Math.ceil((number.length - start) / 6);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  // Scan 24-bit chunks and add them to the number
  var off = 0;
  for (var i = number.length - 6, j = 0; i >= start; i -= 6) {
    var w = parseHex(number, i, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
    off += 24;
    if (off >= 26) {
      off -= 26;
      j++;
    }
  }
  if (i + 6 !== start) {
    var w = parseHex(number, start, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
  }
  this.strip();
};

function parseBase(str, start, end, mul) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r *= mul;

    // 'a'
    if (c >= 49)
      r += c - 49 + 0xa;

    // 'A'
    else if (c >= 17)
      r += c - 17 + 0xa;

    // '0' - '9'
    else
      r += c;
  }
  return r;
}

BN.prototype._parseBase = function _parseBase(number, base, start) {
  // Initialize as zero
  this.words = [ 0 ];
  this.length = 1;

  // Find length of limb in base
  for (var limbLen = 0, limbPow = 1; limbPow <= 0x3ffffff; limbPow *= base)
    limbLen++;
  limbLen--;
  limbPow = (limbPow / base) | 0;

  var total = number.length - start;
  var mod = total % limbLen;
  var end = Math.min(total, total - mod) + start;

  var word = 0;
  for (var i = start; i < end; i += limbLen) {
    word = parseBase(number, i, i + limbLen, base);

    this.imuln(limbPow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }

  if (mod !== 0) {
    var pow = 1;
    var word = parseBase(number, i, number.length, base);

    for (var i = 0; i < mod; i++)
      pow *= base;
    this.imuln(pow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }
};

BN.prototype.copy = function copy(dest) {
  dest.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    dest.words[i] = this.words[i];
  dest.length = this.length;
  dest.negative = this.negative;
};

BN.prototype.clone = function clone() {
  var r = new BN(null);
  this.copy(r);
  return r;
};

// Remove leading `0` from `this`
BN.prototype.strip = function strip() {
  while (this.length > 1 && this.words[this.length - 1] === 0)
    this.length--;
  return this._normSign();
};

BN.prototype._normSign = function _normSign() {
  // -0 = 0
  if (this.length === 1 && this.words[0] === 0)
    this.negative = 0;
  return this;
};

var zeros = [
  '',
  '0',
  '00',
  '000',
  '0000',
  '00000',
  '000000',
  '0000000',
  '00000000',
  '000000000',
  '0000000000',
  '00000000000',
  '000000000000',
  '0000000000000',
  '00000000000000',
  '000000000000000',
  '0000000000000000',
  '00000000000000000',
  '000000000000000000',
  '0000000000000000000',
  '00000000000000000000',
  '000000000000000000000',
  '0000000000000000000000',
  '00000000000000000000000',
  '000000000000000000000000',
  '0000000000000000000000000'
];

var groupSizes = [
  0, 0,
  25, 16, 12, 11, 10, 9, 8,
  8, 7, 7, 7, 7, 6, 6,
  6, 6, 6, 6, 6, 5, 5,
  5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5
];

var groupBases = [
  0, 0,
  33554432, 43046721, 16777216, 48828125, 60466176, 40353607, 16777216,
  43046721, 10000000, 19487171, 35831808, 62748517, 7529536, 11390625,
  16777216, 24137569, 34012224, 47045881, 64000000, 4084101, 5153632,
  6436343, 7962624, 9765625, 11881376, 14348907, 17210368, 20511149,
  24300000, 28629151, 33554432, 39135393, 45435424, 52521875, 60466176
];

BN.prototype.toString = function toString(base, padding) {
  base = base || 10;
  var padding = padding | 0 || 1;
  if (base === 16 || base === 'hex') {
    var out = '';
    var off = 0;
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var w = this.words[i];
      var word = (((w << off) | carry) & 0xffffff).toString(16);
      carry = (w >>> (24 - off)) & 0xffffff;
      if (carry !== 0 || i !== this.length - 1)
        out = zeros[6 - word.length] + word + out;
      else
        out = word + out;
      off += 2;
      if (off >= 26) {
        off -= 26;
        i--;
      }
    }
    if (carry !== 0)
      out = carry.toString(16) + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else if (base === (base | 0) && base >= 2 && base <= 36) {
    var groupSize = groupSizes[base];
    var groupBase = groupBases[base];
    var out = '';
    var c = this.clone();
    c.negative = 0;
    while (c.cmpn(0) !== 0) {
      var r = c.modn(groupBase).toString(base);
      c = c.idivn(groupBase);

      if (c.cmpn(0) !== 0)
        out = zeros[groupSize - r.length] + r + out;
      else
        out = r + out;
    }
    if (this.cmpn(0) === 0)
      out = '0' + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else {
    throw 'Base should be between 2 and 36';
  }
};

BN.prototype.toJSON = function toJSON() {
  return this.toString(16);
};

BN.prototype.toArray = function toArray(endian, length) {
  this.strip();
  var littleEndian = endian === 'le';
  var res = new Array(this.byteLength());
  res[0] = 0;

  var q = this.clone();
  if (!littleEndian) {
    // Assume big-endian
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[res.length - i - 1] = b;
    }
  } else {
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[i] = b;
    }
  }

  if (length) {
    while (res.length < length) {
      if (littleEndian)
        res.push(0);
      else
        res.unshift(0);
    }
  }

  return res;
};

if (Math.clz32) {
  BN.prototype._countBits = function _countBits(w) {
    return 32 - Math.clz32(w);
  };
} else {
  BN.prototype._countBits = function _countBits(w) {
    var t = w;
    var r = 0;
    if (t >= 0x1000) {
      r += 13;
      t >>>= 13;
    }
    if (t >= 0x40) {
      r += 7;
      t >>>= 7;
    }
    if (t >= 0x8) {
      r += 4;
      t >>>= 4;
    }
    if (t >= 0x02) {
      r += 2;
      t >>>= 2;
    }
    return r + t;
  };
}

// Return number of used bits in a BN
BN.prototype.bitLength = function bitLength() {
  var hi = 0;
  var w = this.words[this.length - 1];
  var hi = this._countBits(w);
  return (this.length - 1) * 26 + hi;
};

BN.prototype.byteLength = function byteLength() {
  return Math.ceil(this.bitLength() / 8);
};

// Return negative clone of `this`
BN.prototype.neg = function neg() {
  if (this.cmpn(0) === 0)
    return this.clone();

  var r = this.clone();
  r.negative = this.negative ^ 1;
  return r;
};

BN.prototype.ineg = function ineg() {
  this.negative ^= 1;
  return this;
};

// Or `num` with `this` in-place
BN.prototype.iuor = function iuor(num) {
  while (this.length < num.length)
    this.words[this.length++] = 0;

  for (var i = 0; i < num.length; i++)
    this.words[i] = this.words[i] | num.words[i];

  return this.strip();
};

BN.prototype.ior = function ior(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuor(num);
};


// Or `num` with `this`
BN.prototype.or = function or(num) {
  if (this.length > num.length)
    return this.clone().ior(num);
  else
    return num.clone().ior(this);
};

BN.prototype.uor = function uor(num) {
  if (this.length > num.length)
    return this.clone().iuor(num);
  else
    return num.clone().iuor(this);
};


// And `num` with `this` in-place
BN.prototype.iuand = function iuand(num) {
  // b = min-length(num, this)
  var b;
  if (this.length > num.length)
    b = num;
  else
    b = this;

  for (var i = 0; i < b.length; i++)
    this.words[i] = this.words[i] & num.words[i];

  this.length = b.length;

  return this.strip();
};

BN.prototype.iand = function iand(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuand(num);
};


// And `num` with `this`
BN.prototype.and = function and(num) {
  if (this.length > num.length)
    return this.clone().iand(num);
  else
    return num.clone().iand(this);
};

BN.prototype.uand = function uand(num) {
  if (this.length > num.length)
    return this.clone().iuand(num);
  else
    return num.clone().iuand(this);
};


// Xor `num` with `this` in-place
BN.prototype.iuxor = function iuxor(num) {
  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  for (var i = 0; i < b.length; i++)
    this.words[i] = a.words[i] ^ b.words[i];

  if (this !== a)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];

  this.length = a.length;

  return this.strip();
};

BN.prototype.ixor = function ixor(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuxor(num);
};


// Xor `num` with `this`
BN.prototype.xor = function xor(num) {
  if (this.length > num.length)
    return this.clone().ixor(num);
  else
    return num.clone().ixor(this);
};

BN.prototype.uxor = function uxor(num) {
  if (this.length > num.length)
    return this.clone().iuxor(num);
  else
    return num.clone().iuxor(this);
};


// Add `num` to `this` in-place
BN.prototype.iadd = function iadd(num) {
  // negative + positive
  if (this.negative !== 0 && num.negative === 0) {
    this.negative = 0;
    var r = this.isub(num);
    this.negative ^= 1;
    return this._normSign();

  // positive + negative
  } else if (this.negative === 0 && num.negative !== 0) {
    num.negative = 0;
    var r = this.isub(num);
    num.negative = 1;
    return r._normSign();
  }

  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) + (b.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }

  this.length = a.length;
  if (carry !== 0) {
    this.words[this.length] = carry;
    this.length++;
  // Copy the rest of the words
  } else if (a !== this) {
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  }

  return this;
};

// Add `num` to `this`
BN.prototype.add = function add(num) {
  if (num.negative !== 0 && this.negative === 0) {
    num.negative = 0;
    var res = this.sub(num);
    num.negative ^= 1;
    return res;
  } else if (num.negative === 0 && this.negative !== 0) {
    this.negative = 0;
    var res = num.sub(this);
    this.negative = 1;
    return res;
  }

  if (this.length > num.length)
    return this.clone().iadd(num);
  else
    return num.clone().iadd(this);
};

// Subtract `num` from `this` in-place
BN.prototype.isub = function isub(num) {
  // this - (-num) = this + num
  if (num.negative !== 0) {
    num.negative = 0;
    var r = this.iadd(num);
    num.negative = 1;
    return r._normSign();

  // -this - num = -(this + num)
  } else if (this.negative !== 0) {
    this.negative = 0;
    this.iadd(num);
    this.negative = 1;
    return this._normSign();
  }

  // At this point both numbers are positive
  var cmp = this.cmp(num);

  // Optimization - zeroify
  if (cmp === 0) {
    this.negative = 0;
    this.length = 1;
    this.words[0] = 0;
    return this;
  }

  // a > b
  var a;
  var b;
  if (cmp > 0) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) - (b.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }

  // Copy rest of the words
  if (carry === 0 && i < a.length && a !== this)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  this.length = Math.max(this.length, i);

  if (a !== this)
    this.negative = 1;

  return this.strip();
};

// Subtract `num` from `this`
BN.prototype.sub = function sub(num) {
  return this.clone().isub(num);
};

function smallMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  var len = (self.length + num.length) | 0;
  out.length = len;
  len = (len - 1) | 0;

  // Peel one iteration (compiler can't do it, because of code complexity)
  var a = self.words[0] | 0;
  var b = num.words[0] | 0;
  var r = a * b;

  var lo = r & 0x3ffffff;
  var carry = (r / 0x4000000) | 0;
  out.words[0] = lo;

  for (var k = 1; k < len; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = carry >>> 26;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = (k - j) | 0;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;
    }
    out.words[k] = rword | 0;
    carry = ncarry | 0;
  }
  if (carry !== 0) {
    out.words[k] = carry | 0;
  } else {
    out.length--;
  }

  return out.strip();
}

function bigMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  out.length = self.length + num.length;

  var carry = 0;
  var hncarry = 0;
  for (var k = 0; k < out.length - 1; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = hncarry;
    hncarry = 0;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;

      hncarry += ncarry >>> 26;
      ncarry &= 0x3ffffff;
    }
    out.words[k] = rword;
    carry = ncarry;
    ncarry = hncarry;
  }
  if (carry !== 0) {
    out.words[k] = carry;
  } else {
    out.length--;
  }

  return out.strip();
}

BN.prototype.mulTo = function mulTo(num, out) {
  var res;
  if (this.length + num.length < 63)
    res = smallMulTo(this, num, out);
  else
    res = bigMulTo(this, num, out);
  return res;
};

// Multiply `this` by `num`
BN.prototype.mul = function mul(num) {
  var out = new BN(null);
  out.words = new Array(this.length + num.length);
  return this.mulTo(num, out);
};

// In-place Multiplication
BN.prototype.imul = function imul(num) {
  if (this.cmpn(0) === 0 || num.cmpn(0) === 0) {
    this.words[0] = 0;
    this.length = 1;
    return this;
  }

  var tlen = this.length;
  var nlen = num.length;

  this.negative = num.negative ^ this.negative;
  this.length = this.length + num.length;
  this.words[this.length - 1] = 0;

  for (var k = this.length - 2; k >= 0; k--) {
    // Sum all words with the same `i + j = k` and accumulate `carry`,
    // note that carry could be >= 0x3ffffff
    var carry = 0;
    var rword = 0;
    var maxJ = Math.min(k, nlen - 1);
    for (var j = Math.max(0, k - tlen + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = this.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      carry += (r / 0x4000000) | 0;
      lo += rword;
      rword = lo & 0x3ffffff;
      carry += lo >>> 26;
    }
    this.words[k] = rword;
    this.words[k + 1] += carry;
    carry = 0;
  }

  // Propagate overflows
  var carry = 0;
  for (var i = 1; i < this.length; i++) {
    var w = (this.words[i] | 0) + carry;
    this.words[i] = w & 0x3ffffff;
    carry = w >>> 26;
  }

  return this.strip();
};

BN.prototype.imuln = function imuln(num) {
  // Carry
  var carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = (this.words[i] | 0) * num;
    var lo = (w & 0x3ffffff) + (carry & 0x3ffffff);
    carry >>= 26;
    carry += (w / 0x4000000) | 0;
    // NOTE: lo is 27bit maximum
    carry += lo >>> 26;
    this.words[i] = lo & 0x3ffffff;
  }

  if (carry !== 0) {
    this.words[i] = carry;
    this.length++;
  }

  return this;
};

BN.prototype.muln = function muln(num) {
  return this.clone().imuln(num);
};

// `this` * `this`
BN.prototype.sqr = function sqr() {
  return this.mul(this);
};

// `this` * `this` in-place
BN.prototype.isqr = function isqr() {
  return this.mul(this);
};

// Shift-left in-place
BN.prototype.iushln = function iushln(bits) {
  var r = bits % 26;
  var s = (bits - r) / 26;
  var carryMask = (0x3ffffff >>> (26 - r)) << (26 - r);

  if (r !== 0) {
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var newCarry = this.words[i] & carryMask;
      var c = ((this.words[i] | 0) - newCarry) << r;
      this.words[i] = c | carry;
      carry = newCarry >>> (26 - r);
    }
    if (carry) {
      this.words[i] = carry;
      this.length++;
    }
  }

  if (s !== 0) {
    for (var i = this.length - 1; i >= 0; i--)
      this.words[i + s] = this.words[i];
    for (var i = 0; i < s; i++)
      this.words[i] = 0;
    this.length += s;
  }

  return this.strip();
};

BN.prototype.ishln = function ishln(bits) {
  return this.iushln(bits);
};

// Shift-right in-place
BN.prototype.iushrn = function iushrn(bits, hint, extended) {
  var h;
  if (hint)
    h = (hint - (hint % 26)) / 26;
  else
    h = 0;

  var r = bits % 26;
  var s = Math.min((bits - r) / 26, this.length);
  var mask = 0x3ffffff ^ ((0x3ffffff >>> r) << r);
  var maskedWords = extended;

  h -= s;
  h = Math.max(0, h);

  // Extended mode, copy masked part
  if (maskedWords) {
    for (var i = 0; i < s; i++)
      maskedWords.words[i] = this.words[i];
    maskedWords.length = s;
  }

  if (s === 0) {
    // No-op, we should not move anything at all
  } else if (this.length > s) {
    this.length -= s;
    for (var i = 0; i < this.length; i++)
      this.words[i] = this.words[i + s];
  } else {
    this.words[0] = 0;
    this.length = 1;
  }

  var carry = 0;
  for (var i = this.length - 1; i >= 0 && (carry !== 0 || i >= h); i--) {
    var word = this.words[i] | 0;
    this.words[i] = (carry << (26 - r)) | (word >>> r);
    carry = word & mask;
  }

  // Push carried bits as a mask
  if (maskedWords && carry !== 0)
    maskedWords.words[maskedWords.length++] = carry;

  if (this.length === 0) {
    this.words[0] = 0;
    this.length = 1;
  }

  this.strip();

  return this;
};

BN.prototype.ishrn = function ishrn(bits, hint, extended) {
  return this.iushrn(bits, hint, extended);
};

// Shift-left
BN.prototype.shln = function shln(bits) {
  var x = this.clone();
  var neg = x.negative;
  x.negative = false;
  x.ishln(bits);
  x.negative = neg;
  return x;
};

BN.prototype.ushln = function ushln(bits) {
  return this.clone().iushln(bits);
};

// Shift-right
BN.prototype.shrn = function shrn(bits) {
  var x = this.clone();
  if(x.negative) {
      x.negative = false;
      x.ishrn(bits);
      x.negative = true;
      return x.isubn(1);
  } else {
      return x.ishrn(bits);
  }
};

BN.prototype.ushrn = function ushrn(bits) {
  return this.clone().iushrn(bits);
};

// Test if n bit is set
BN.prototype.testn = function testn(bit) {
  var r = bit % 26;
  var s = (bit - r) / 26;
  var q = 1 << r;

  // Fast case: bit is much higher than all existing words
  if (this.length <= s) {
    return false;
  }

  // Check bit and return
  var w = this.words[s];

  return !!(w & q);
};

// Add plain number `num` to `this`
BN.prototype.iaddn = function iaddn(num) {
  if (num < 0)
    return this.isubn(-num);

  // Possible sign change
  if (this.negative !== 0) {
    if (this.length === 1 && (this.words[0] | 0) < num) {
      this.words[0] = num - (this.words[0] | 0);
      this.negative = 0;
      return this;
    }

    this.negative = 0;
    this.isubn(num);
    this.negative = 1;
    return this;
  }

  // Add without checks
  return this._iaddn(num);
};

BN.prototype._iaddn = function _iaddn(num) {
  this.words[0] += num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] >= 0x4000000; i++) {
    this.words[i] -= 0x4000000;
    if (i === this.length - 1)
      this.words[i + 1] = 1;
    else
      this.words[i + 1]++;
  }
  this.length = Math.max(this.length, i + 1);

  return this;
};

// Subtract plain number `num` from `this`
BN.prototype.isubn = function isubn(num) {
  if (num < 0)
    return this.iaddn(-num);

  if (this.negative !== 0) {
    this.negative = 0;
    this.iaddn(num);
    this.negative = 1;
    return this;
  }

  this.words[0] -= num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] < 0; i++) {
    this.words[i] += 0x4000000;
    this.words[i + 1] -= 1;
  }

  return this.strip();
};

BN.prototype.addn = function addn(num) {
  return this.clone().iaddn(num);
};

BN.prototype.subn = function subn(num) {
  return this.clone().isubn(num);
};

BN.prototype.iabs = function iabs() {
  this.negative = 0;

  return this;
};

BN.prototype.abs = function abs() {
  return this.clone().iabs();
};

BN.prototype._ishlnsubmul = function _ishlnsubmul(num, mul, shift) {
  // Bigger storage is needed
  var len = num.length + shift;
  var i;
  if (this.words.length < len) {
    var t = new Array(len);
    for (var i = 0; i < this.length; i++)
      t[i] = this.words[i];
    this.words = t;
  } else {
    i = this.length;
  }

  // Zeroify rest
  this.length = Math.max(this.length, len);
  for (; i < this.length; i++)
    this.words[i] = 0;

  var carry = 0;
  for (var i = 0; i < num.length; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    var right = (num.words[i] | 0) * mul;
    w -= right & 0x3ffffff;
    carry = (w >> 26) - ((right / 0x4000000) | 0);
    this.words[i + shift] = w & 0x3ffffff;
  }
  for (; i < this.length - shift; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    carry = w >> 26;
    this.words[i + shift] = w & 0x3ffffff;
  }

  if (carry === 0)
    return this.strip();

  carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = -(this.words[i] | 0) + carry;
    carry = w >> 26;
    this.words[i] = w & 0x3ffffff;
  }
  this.negative = 1;

  return this.strip();
};

BN.prototype._wordDiv = function _wordDiv(num, mode) {
  var shift = this.length - num.length;

  var a = this.clone();
  var b = num;

  // Normalize
  var bhi = b.words[b.length - 1] | 0;
  var bhiBits = this._countBits(bhi);
  shift = 26 - bhiBits;
  if (shift !== 0) {
    b = b.ushln(shift);
    a.iushln(shift);
    bhi = b.words[b.length - 1] | 0;
  }

  // Initialize quotient
  var m = a.length - b.length;
  var q;

  if (mode !== 'mod') {
    q = new BN(null);
    q.length = m + 1;
    q.words = new Array(q.length);
    for (var i = 0; i < q.length; i++)
      q.words[i] = 0;
  }

  var diff = a.clone()._ishlnsubmul(b, 1, m);
  if (diff.negative === 0) {
    a = diff;
    if (q)
      q.words[m] = 1;
  }

  for (var j = m - 1; j >= 0; j--) {
    var qj = (a.words[b.length + j] | 0) * 0x4000000 +
             (a.words[b.length + j - 1] | 0);

    // NOTE: (qj / bhi) is (0x3ffffff * 0x4000000 + 0x3ffffff) / 0x2000000 max
    // (0x7ffffff)
    qj = Math.min((qj / bhi) | 0, 0x3ffffff);

    a._ishlnsubmul(b, qj, j);
    while (a.negative !== 0) {
      qj--;
      a.negative = 0;
      a._ishlnsubmul(b, 1, j);
      if (a.cmpn(0) !== 0)
        a.negative ^= 1;
    }
    if (q)
      q.words[j] = qj;
  }
  if (q)
    q.strip();
  a.strip();

  // Denormalize
  if (mode !== 'div' && shift !== 0)
    a.iushrn(shift);
  return { div: q ? q : null, mod: a };
};

BN.prototype.divmod = function divmod(num, mode, positive) {
  if (this.negative !== 0 && num.negative === 0) {
    var res = this.neg().divmod(num, mode);
    var div;
    var mod;
    if (mode !== 'mod')
      div = res.div.neg();
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.add(num);
    }
    return {
      div: div,
      mod: mod
    };
  } else if (this.negative === 0 && num.negative !== 0) {
    var res = this.divmod(num.neg(), mode);
    var div;
    if (mode !== 'mod')
      div = res.div.neg();
    return { div: div, mod: res.mod };
  } else if ((this.negative & num.negative) !== 0) {
    var res = this.neg().divmod(num.neg(), mode);
    var mod;
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.isub(num);
    }
    return {
      div: res.div,
      mod: mod
    };
  }

  // Both numbers are positive at this point

  // Strip both numbers to approximate shift value
  if (num.length > this.length || this.cmp(num) < 0)
    return { div: new BN(0), mod: this };

  // Very short reduction
  if (num.length === 1) {
    if (mode === 'div')
      return { div: this.divn(num.words[0]), mod: null };
    else if (mode === 'mod')
      return { div: null, mod: new BN(this.modn(num.words[0])) };
    return {
      div: this.divn(num.words[0]),
      mod: new BN(this.modn(num.words[0]))
    };
  }

  return this._wordDiv(num, mode);
};

// Find `this` / `num`
BN.prototype.div = function div(num) {
  return this.divmod(num, 'div', false).div;
};

// Find `this` % `num`
BN.prototype.mod = function mod(num) {
  return this.divmod(num, 'mod', false).mod;
};

BN.prototype.umod = function umod(num) {
  return this.divmod(num, 'mod', true).mod;
};

// Find Round(`this` / `num`)
BN.prototype.divRound = function divRound(num) {
  var dm = this.divmod(num);

  // Fast case - exact division
  if (dm.mod.cmpn(0) === 0)
    return dm.div;

  var mod = dm.div.negative !== 0 ? dm.mod.isub(num) : dm.mod;

  var half = num.ushrn(1);
  var r2 = num.andln(1);
  var cmp = mod.cmp(half);

  // Round down
  if (cmp < 0 || r2 === 1 && cmp === 0)
    return dm.div;

  // Round up
  return dm.div.negative !== 0 ? dm.div.isubn(1) : dm.div.iaddn(1);
};

BN.prototype.modn = function modn(num) {
  var p = (1 << 26) % num;

  var acc = 0;
  for (var i = this.length - 1; i >= 0; i--)
    acc = (p * acc + (this.words[i] | 0)) % num;

  return acc;
};

// In-place division by number
BN.prototype.idivn = function idivn(num) {
  var carry = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var w = (this.words[i] | 0) + carry * 0x4000000;
    this.words[i] = (w / num) | 0;
    carry = w % num;
  }

  return this.strip();
};

BN.prototype.divn = function divn(num) {
  return this.clone().idivn(num);
};

BN.prototype.isEven = function isEven() {
  return (this.words[0] & 1) === 0;
};

BN.prototype.isOdd = function isOdd() {
  return (this.words[0] & 1) === 1;
};

// And first word and num
BN.prototype.andln = function andln(num) {
  return this.words[0] & num;
};

BN.prototype.cmpn = function cmpn(num) {
  var negative = num < 0;
  if (negative)
    num = -num;

  if (this.negative !== 0 && !negative)
    return -1;
  else if (this.negative === 0 && negative)
    return 1;

  num &= 0x3ffffff;
  this.strip();

  var res;
  if (this.length > 1) {
    res = 1;
  } else {
    var w = this.words[0] | 0;
    res = w === num ? 0 : w < num ? -1 : 1;
  }
  if (this.negative !== 0)
    res = -res;
  return res;
};

// Compare two numbers and return:
// 1 - if `this` > `num`
// 0 - if `this` == `num`
// -1 - if `this` < `num`
BN.prototype.cmp = function cmp(num) {
  if (this.negative !== 0 && num.negative === 0)
    return -1;
  else if (this.negative === 0 && num.negative !== 0)
    return 1;

  var res = this.ucmp(num);
  if (this.negative !== 0)
    return -res;
  else
    return res;
};

// Unsigned comparison
BN.prototype.ucmp = function ucmp(num) {
  // At this point both numbers have the same sign
  if (this.length > num.length)
    return 1;
  else if (this.length < num.length)
    return -1;

  var res = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var a = this.words[i] | 0;
    var b = num.words[i] | 0;

    if (a === b)
      continue;
    if (a < b)
      res = -1;
    else if (a > b)
      res = 1;
    break;
  }
  return res;
};
})(undefined, __bn);

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return {_:0, a:0, b:undefined};
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return {_:0, a:1, b:val};
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

// TODO: inefficient compared to real fromInt?
__bn.Z = new __bn.BN(0);
__bn.ONE = new __bn.BN(1);
__bn.MOD32 = new __bn.BN(0x100000000); // 2^32
var I_fromNumber = function(x) {return new __bn.BN(x);}
var I_fromInt = I_fromNumber;
var I_fromBits = function(lo,hi) {
    var x = new __bn.BN(lo >>> 0);
    var y = new __bn.BN(hi >>> 0);
    y.ishln(32);
    x.iadd(y);
    return x;
}
var I_fromString = function(s) {return new __bn.BN(s);}
var I_toInt = function(x) {return I_toNumber(x.mod(__bn.MOD32));}
var I_toWord = function(x) {return I_toInt(x) >>> 0;};
// TODO: inefficient!
var I_toNumber = function(x) {return Number(x.toString());}
var I_equals = function(a,b) {return a.cmp(b) === 0;}
var I_compare = function(a,b) {return a.cmp(b);}
var I_compareInt = function(x,i) {return x.cmp(new __bn.BN(i));}
var I_negate = function(x) {return x.neg();}
var I_add = function(a,b) {return a.add(b);}
var I_sub = function(a,b) {return a.sub(b);}
var I_mul = function(a,b) {return a.mul(b);}
var I_mod = function(a,b) {return I_rem(I_add(b, I_rem(a, b)), b);}
var I_quotRem = function(a,b) {
    var qr = a.divmod(b);
    return {_:0, a:qr.div, b:qr.mod};
}
var I_div = function(a,b) {
    if((a.cmp(__bn.Z)>=0) != (a.cmp(__bn.Z)>=0)) {
        if(a.cmp(a.rem(b), __bn.Z) !== 0) {
            return a.div(b).sub(__bn.ONE);
        }
    }
    return a.div(b);
}
var I_divMod = function(a,b) {
    return {_:0, a:I_div(a,b), b:a.mod(b)};
}
var I_quot = function(a,b) {return a.div(b);}
var I_rem = function(a,b) {return a.mod(b);}
var I_and = function(a,b) {return a.and(b);}
var I_or = function(a,b) {return a.or(b);}
var I_xor = function(a,b) {return a.xor(b);}
var I_shiftLeft = function(a,b) {return a.shln(b);}
var I_shiftRight = function(a,b) {return a.shrn(b);}
var I_signum = function(x) {return x.cmp(new __bn.BN(0));}
var I_abs = function(x) {return x.abs();}
var I_decodeDouble = function(x) {
    var dec = decodeDouble(x);
    var mantissa = I_fromBits(dec.c, dec.b);
    if(dec.a < 0) {
        mantissa = I_negate(mantissa);
    }
    return {_:0, a:dec.d, b:mantissa};
}
var I_toString = function(x) {return x.toString();}
var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    if(x.isNegative()) {
        return I_negate(I_fromInt64(x.negate()));
    } else {
        return I_fromBits(x.low, x.high);
    }
}

function I_toInt64(x) {
    if(x.negative) {
        return I_toInt64(I_negate(x)).negate();
    } else {
        return new Long(I_toInt(x), I_toInt(I_shiftRight(x,32)));
    }
}

function I_fromWord64(x) {
    return I_fromBits(x.toInt(), x.shru(32).toInt());
}

function I_toWord64(x) {
    var w = I_toInt64(x);
    w.unsigned = true;
    return w;
}

/**
 * @license long.js (c) 2013 Daniel Wirtz <dcode@dcode.io>
 * Released under the Apache License, Version 2.0
 * see: https://github.com/dcodeIO/long.js for details
 */
function Long(low, high, unsigned) {
    this.low = low | 0;
    this.high = high | 0;
    this.unsigned = !!unsigned;
}

var INT_CACHE = {};
var UINT_CACHE = {};
function cacheable(x, u) {
    return u ? 0 <= (x >>>= 0) && x < 256 : -128 <= (x |= 0) && x < 128;
}

function __fromInt(value, unsigned) {
    var obj, cachedObj, cache;
    if (unsigned) {
        if (cache = cacheable(value >>>= 0, true)) {
            cachedObj = UINT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache)
            UINT_CACHE[value] = obj;
        return obj;
    } else {
        if (cache = cacheable(value |= 0, false)) {
            cachedObj = INT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, value < 0 ? -1 : 0, false);
        if (cache)
            INT_CACHE[value] = obj;
        return obj;
    }
}

function __fromNumber(value, unsigned) {
    if (isNaN(value) || !isFinite(value))
        return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return __fromNumber(-value, unsigned).neg();
    return new Long((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}
var pow_dbl = Math.pow;
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = __fromInt(TWO_PWR_24_DBL);
var ZERO = __fromInt(0);
Long.ZERO = ZERO;
var UZERO = __fromInt(0, true);
Long.UZERO = UZERO;
var ONE = __fromInt(1);
Long.ONE = ONE;
var UONE = __fromInt(1, true);
Long.UONE = UONE;
var NEG_ONE = __fromInt(-1);
Long.NEG_ONE = NEG_ONE;
var MAX_VALUE = new Long(0xFFFFFFFF|0, 0x7FFFFFFF|0, false);
Long.MAX_VALUE = MAX_VALUE;
var MAX_UNSIGNED_VALUE = new Long(0xFFFFFFFF|0, 0xFFFFFFFF|0, true);
Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
var MIN_VALUE = new Long(0, 0x80000000|0, false);
Long.MIN_VALUE = MIN_VALUE;
var __lp = Long.prototype;
__lp.toInt = function() {return this.unsigned ? this.low >>> 0 : this.low;};
__lp.toNumber = function() {
    if (this.unsigned)
        return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
    return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
};
__lp.isZero = function() {return this.high === 0 && this.low === 0;};
__lp.isNegative = function() {return !this.unsigned && this.high < 0;};
__lp.isOdd = function() {return (this.low & 1) === 1;};
__lp.eq = function(other) {
    if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
        return false;
    return this.high === other.high && this.low === other.low;
};
__lp.neq = function(other) {return !this.eq(other);};
__lp.lt = function(other) {return this.comp(other) < 0;};
__lp.lte = function(other) {return this.comp(other) <= 0;};
__lp.gt = function(other) {return this.comp(other) > 0;};
__lp.gte = function(other) {return this.comp(other) >= 0;};
__lp.compare = function(other) {
    if (this.eq(other))
        return 0;
    var thisNeg = this.isNegative(),
        otherNeg = other.isNegative();
    if (thisNeg && !otherNeg)
        return -1;
    if (!thisNeg && otherNeg)
        return 1;
    if (!this.unsigned)
        return this.sub(other).isNegative() ? -1 : 1;
    return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
};
__lp.comp = __lp.compare;
__lp.negate = function() {
    if (!this.unsigned && this.eq(MIN_VALUE))
        return MIN_VALUE;
    return this.not().add(ONE);
};
__lp.neg = __lp.negate;
__lp.add = function(addend) {
    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = addend.high >>> 16;
    var b32 = addend.high & 0xFFFF;
    var b16 = addend.low >>> 16;
    var b00 = addend.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 + b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 + b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 + b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 + b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.subtract = function(subtrahend) {return this.add(subtrahend.neg());};
__lp.sub = __lp.subtract;
__lp.multiply = function(multiplier) {
    if (this.isZero())
        return ZERO;
    if (multiplier.isZero())
        return ZERO;
    if (this.eq(MIN_VALUE))
        return multiplier.isOdd() ? MIN_VALUE : ZERO;
    if (multiplier.eq(MIN_VALUE))
        return this.isOdd() ? MIN_VALUE : ZERO;

    if (this.isNegative()) {
        if (multiplier.isNegative())
            return this.neg().mul(multiplier.neg());
        else
            return this.neg().mul(multiplier).neg();
    } else if (multiplier.isNegative())
        return this.mul(multiplier.neg()).neg();

    if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
        return __fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = multiplier.high >>> 16;
    var b32 = multiplier.high & 0xFFFF;
    var b16 = multiplier.low >>> 16;
    var b00 = multiplier.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * b00;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c16 += a00 * b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * b00;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a16 * b16;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a00 * b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.mul = __lp.multiply;
__lp.divide = function(divisor) {
    if (divisor.isZero())
        throw Error('division by zero');
    if (this.isZero())
        return this.unsigned ? UZERO : ZERO;
    var approx, rem, res;
    if (this.eq(MIN_VALUE)) {
        if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
            return MIN_VALUE;
        else if (divisor.eq(MIN_VALUE))
            return ONE;
        else {
            var halfThis = this.shr(1);
            approx = halfThis.div(divisor).shl(1);
            if (approx.eq(ZERO)) {
                return divisor.isNegative() ? ONE : NEG_ONE;
            } else {
                rem = this.sub(divisor.mul(approx));
                res = approx.add(rem.div(divisor));
                return res;
            }
        }
    } else if (divisor.eq(MIN_VALUE))
        return this.unsigned ? UZERO : ZERO;
    if (this.isNegative()) {
        if (divisor.isNegative())
            return this.neg().div(divisor.neg());
        return this.neg().div(divisor).neg();
    } else if (divisor.isNegative())
        return this.div(divisor.neg()).neg();

    res = ZERO;
    rem = this;
    while (rem.gte(divisor)) {
        approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
        var log2 = Math.ceil(Math.log(approx) / Math.LN2),
            delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),
            approxRes = __fromNumber(approx),
            approxRem = approxRes.mul(divisor);
        while (approxRem.isNegative() || approxRem.gt(rem)) {
            approx -= delta;
            approxRes = __fromNumber(approx, this.unsigned);
            approxRem = approxRes.mul(divisor);
        }
        if (approxRes.isZero())
            approxRes = ONE;

        res = res.add(approxRes);
        rem = rem.sub(approxRem);
    }
    return res;
};
__lp.div = __lp.divide;
__lp.modulo = function(divisor) {return this.sub(this.div(divisor).mul(divisor));};
__lp.mod = __lp.modulo;
__lp.not = function not() {return new Long(~this.low, ~this.high, this.unsigned);};
__lp.and = function(other) {return new Long(this.low & other.low, this.high & other.high, this.unsigned);};
__lp.or = function(other) {return new Long(this.low | other.low, this.high | other.high, this.unsigned);};
__lp.xor = function(other) {return new Long(this.low ^ other.low, this.high ^ other.high, this.unsigned);};

__lp.shl = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
    else
        return new Long(0, this.low << (numBits - 32), this.unsigned);
};

__lp.shr = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
    else
        return new Long(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
};

__lp.shru = function(numBits) {
    numBits &= 63;
    if (numBits === 0)
        return this;
    else {
        var high = this.high;
        if (numBits < 32) {
            var low = this.low;
            return new Long((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
        } else if (numBits === 32)
            return new Long(high, 0, this.unsigned);
        else
            return new Long(high >>> (numBits - 32), 0, this.unsigned);
    }
};

__lp.toSigned = function() {return this.unsigned ? new Long(this.low, this.high, false) : this;};
__lp.toUnsigned = function() {return this.unsigned ? this : new Long(this.low, this.high, true);};

// Int64
function hs_eqInt64(x, y) {return x.eq(y);}
function hs_neInt64(x, y) {return x.neq(y);}
function hs_ltInt64(x, y) {return x.lt(y);}
function hs_leInt64(x, y) {return x.lte(y);}
function hs_gtInt64(x, y) {return x.gt(y);}
function hs_geInt64(x, y) {return x.gte(y);}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shl(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shr(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shru(bits);}
function hs_int64ToInt(x) {return x.toInt();}
var hs_intToInt64 = __fromInt;

// Word64
function hs_wordToWord64(x) {return __fromInt(x, true);}
function hs_word64ToWord(x) {return x.toInt(x);}
function hs_mkWord64(low, high) {return new Long(low,high,true);}
function hs_and64(a,b) {return a.and(b);};
function hs_or64(a,b) {return a.or(b);};
function hs_xor64(a,b) {return a.xor(b);};
function hs_not64(x) {return x.not();}
var hs_eqWord64 = hs_eqInt64;
var hs_neWord64 = hs_neInt64;
var hs_ltWord64 = hs_ltInt64;
var hs_leWord64 = hs_leInt64;
var hs_gtWord64 = hs_gtInt64;
var hs_geWord64 = hs_geInt64;
var hs_quotWord64 = hs_quotInt64;
var hs_remWord64 = hs_remInt64;
var hs_uncheckedShiftL64 = hs_uncheckedIShiftL64;
var hs_uncheckedShiftRL64 = hs_uncheckedIShiftRL64;
function hs_int64ToWord64(x) {return x.toUnsigned();}
function hs_word64ToInt64(x) {return x.toSigned();}

// Joseph Myers' MD5 implementation, ported to work on typed arrays.
// Used under the BSD license.
function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s, n) {
    var a = s['v']['w8'];
    var orig_n = n,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=n; i+=64) {
        md5cycle(state, md5blk(a.subarray(i-64, i)));
    }
    a = a.subarray(i-64);
    n = n < (i-64) ? 0 : n-(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<n; i++)
        tail[i>>2] |= a[i] << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = orig_n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s[i]
            + (s[i+1] << 8)
            + (s[i+2] << 16)
            + (s[i+3] << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s, n) {
    return hex(md51(s, n));
}

window['md5'] = md5;

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

function __hsbase_MD5Init(ctx) {}
// Note that this is a one time "update", since that's all that's used by
// GHC.Fingerprint.
function __hsbase_MD5Update(ctx, s, n) {
    ctx.md5 = md51(s, n);
}
function __hsbase_MD5Final(out, ctx) {
    var a = out['v']['i32'];
    a[0] = ctx.md5[0];
    a[1] = ctx.md5[1];
    a[2] = ctx.md5[2];
    a[3] = ctx.md5[3];
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = new Array(n);
    for(var i = 0; i < n; ++i) {
        arr[i] = x;
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    return new ByteArray(new ArrayBuffer(n));
}

// Wrap a JS ArrayBuffer into a ByteArray. Truncates the array length to the
// closest multiple of 8 bytes.
function wrapByteArr(buffer) {
    var diff = buffer.byteLength % 8;
    if(diff != 0) {
        var buffer = buffer.slice(0, buffer.byteLength-diff);
    }
    return new ByteArray(buffer);
}

function ByteArray(buffer) {
    var views =
        { 'i8' : new Int8Array(buffer)
        , 'i16': new Int16Array(buffer)
        , 'i32': new Int32Array(buffer)
        , 'w8' : new Uint8Array(buffer)
        , 'w16': new Uint16Array(buffer)
        , 'w32': new Uint32Array(buffer)
        , 'f32': new Float32Array(buffer)
        , 'f64': new Float64Array(buffer)
        };
    this['b'] = buffer;
    this['v'] = views;
    this['off'] = 0;
}
window['newArr'] = newArr;
window['newByteArr'] = newByteArr;
window['wrapByteArr'] = wrapByteArr;
window['ByteArray'] = ByteArray;

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function writeOffAddr64(addr, off, x) {
    addr['v']['w32'][addr.off/8 + off*2] = x.low;
    addr['v']['w32'][addr.off/8 + off*2 + 1] = x.high;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

function readOffAddr64(signed, addr, off) {
    var w64 = hs_mkWord64( addr['v']['w32'][addr.off/8 + off*2]
                         , addr['v']['w32'][addr.off/8 + off*2 + 1]);
    return signed ? hs_word64ToInt64(w64) : w64;
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return {_:0, a:1, b:E(w).val};
}

function finalizeWeak(w) {
    return {_:0, a:B(A1(E(w).fin, __Z))};
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as._ === 1; as = as.b) {
        arr.push(as.a);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1,
            a:arr[elem],
            b:new T(function(){return __arr2lst(elem+1,arr);})};
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs._ === 1; xs = E(xs.b)) {
        arr.push(E(xs.a));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0=new T(function(){return eval("(function(e){return e.getContext(\'2d\');})");}),_1=new T(function(){return eval("(function(e){return !!e.getContext;})");}),_2=function(_3,_4){var _5=E(_3);return (_5._==0)?E(_4):new T2(1,_5.a,new T(function(){return B(_2(_5.b,_4));}));},_6=__Z,_7=0,_8=function(_9,_){while(1){var _a=E(_9);if(!_a._){return _7;}else{var _b=_a.b,_c=E(_a.a);switch(_c._){case 0:var _d=B(A1(_c.a,_));_9=B(_2(_b,new T2(1,_d,_6)));continue;case 1:_9=B(_2(_b,_c.a));continue;default:_9=_b;continue;}}}},_e=function(_f,_g,_){var _h=E(_f);switch(_h._){case 0:var _i=B(A1(_h.a,_));return new F(function(){return _8(B(_2(_g,new T2(1,_i,_6))),_);});break;case 1:return new F(function(){return _8(B(_2(_g,_h.a)),_);});break;default:return new F(function(){return _8(_g,_);});}},_j=function(_k,_l,_){var _m=B(A1(_k,_)),_n=B(A1(_l,_));return _m;},_o=function(_p,_q,_){var _r=B(A1(_p,_)),_s=B(A1(_q,_));return new T(function(){return B(A1(_r,_s));});},_t=function(_u,_v,_){var _w=B(A1(_v,_));return _u;},_x=function(_y,_z,_){var _A=B(A1(_z,_));return new T(function(){return B(A1(_y,_A));});},_B=new T2(0,_x,_t),_C=function(_D,_){return _D;},_E=function(_F,_G,_){var _H=B(A1(_F,_));return new F(function(){return A1(_G,_);});},_I=new T5(0,_B,_C,_o,_E,_j),_J=new T(function(){return B(unCStr("base"));}),_K=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_L=new T(function(){return B(unCStr("IOException"));}),_M=new T5(0,new Long(4053623282,1685460941,true),new Long(3693590983,2507416641,true),_J,_K,_L),_N=new T5(0,new Long(4053623282,1685460941,true),new Long(3693590983,2507416641,true),_M,_6,_6),_O=function(_P){return E(_N);},_Q=function(_R){return E(E(_R).a);},_S=function(_T,_U,_V){var _W=B(A1(_T,_)),_X=B(A1(_U,_)),_Y=hs_eqWord64(_W.a,_X.a);if(!_Y){return __Z;}else{var _Z=hs_eqWord64(_W.b,_X.b);return (!_Z)?__Z:new T1(1,_V);}},_10=function(_11){var _12=E(_11);return new F(function(){return _S(B(_Q(_12.a)),_O,_12.b);});},_13=new T(function(){return B(unCStr(": "));}),_14=new T(function(){return B(unCStr(")"));}),_15=new T(function(){return B(unCStr(" ("));}),_16=new T(function(){return B(unCStr("interrupted"));}),_17=new T(function(){return B(unCStr("system error"));}),_18=new T(function(){return B(unCStr("unsatisified constraints"));}),_19=new T(function(){return B(unCStr("user error"));}),_1a=new T(function(){return B(unCStr("permission denied"));}),_1b=new T(function(){return B(unCStr("illegal operation"));}),_1c=new T(function(){return B(unCStr("end of file"));}),_1d=new T(function(){return B(unCStr("resource exhausted"));}),_1e=new T(function(){return B(unCStr("resource busy"));}),_1f=new T(function(){return B(unCStr("does not exist"));}),_1g=new T(function(){return B(unCStr("already exists"));}),_1h=new T(function(){return B(unCStr("resource vanished"));}),_1i=new T(function(){return B(unCStr("timeout"));}),_1j=new T(function(){return B(unCStr("unsupported operation"));}),_1k=new T(function(){return B(unCStr("hardware fault"));}),_1l=new T(function(){return B(unCStr("inappropriate type"));}),_1m=new T(function(){return B(unCStr("invalid argument"));}),_1n=new T(function(){return B(unCStr("failed"));}),_1o=new T(function(){return B(unCStr("protocol error"));}),_1p=function(_1q,_1r){switch(E(_1q)){case 0:return new F(function(){return _2(_1g,_1r);});break;case 1:return new F(function(){return _2(_1f,_1r);});break;case 2:return new F(function(){return _2(_1e,_1r);});break;case 3:return new F(function(){return _2(_1d,_1r);});break;case 4:return new F(function(){return _2(_1c,_1r);});break;case 5:return new F(function(){return _2(_1b,_1r);});break;case 6:return new F(function(){return _2(_1a,_1r);});break;case 7:return new F(function(){return _2(_19,_1r);});break;case 8:return new F(function(){return _2(_18,_1r);});break;case 9:return new F(function(){return _2(_17,_1r);});break;case 10:return new F(function(){return _2(_1o,_1r);});break;case 11:return new F(function(){return _2(_1n,_1r);});break;case 12:return new F(function(){return _2(_1m,_1r);});break;case 13:return new F(function(){return _2(_1l,_1r);});break;case 14:return new F(function(){return _2(_1k,_1r);});break;case 15:return new F(function(){return _2(_1j,_1r);});break;case 16:return new F(function(){return _2(_1i,_1r);});break;case 17:return new F(function(){return _2(_1h,_1r);});break;default:return new F(function(){return _2(_16,_1r);});}},_1s=new T(function(){return B(unCStr("}"));}),_1t=new T(function(){return B(unCStr("{handle: "));}),_1u=function(_1v,_1w,_1x,_1y,_1z,_1A){var _1B=new T(function(){var _1C=new T(function(){var _1D=new T(function(){var _1E=E(_1y);if(!_1E._){return E(_1A);}else{var _1F=new T(function(){return B(_2(_1E,new T(function(){return B(_2(_14,_1A));},1)));},1);return B(_2(_15,_1F));}},1);return B(_1p(_1w,_1D));}),_1G=E(_1x);if(!_1G._){return E(_1C);}else{return B(_2(_1G,new T(function(){return B(_2(_13,_1C));},1)));}}),_1H=E(_1z);if(!_1H._){var _1I=E(_1v);if(!_1I._){return E(_1B);}else{var _1J=E(_1I.a);if(!_1J._){var _1K=new T(function(){var _1L=new T(function(){return B(_2(_1s,new T(function(){return B(_2(_13,_1B));},1)));},1);return B(_2(_1J.a,_1L));},1);return new F(function(){return _2(_1t,_1K);});}else{var _1M=new T(function(){var _1N=new T(function(){return B(_2(_1s,new T(function(){return B(_2(_13,_1B));},1)));},1);return B(_2(_1J.a,_1N));},1);return new F(function(){return _2(_1t,_1M);});}}}else{return new F(function(){return _2(_1H.a,new T(function(){return B(_2(_13,_1B));},1));});}},_1O=function(_1P){var _1Q=E(_1P);return new F(function(){return _1u(_1Q.a,_1Q.b,_1Q.c,_1Q.d,_1Q.f,_6);});},_1R=function(_1S){return new T2(0,_1T,_1S);},_1U=function(_1V,_1W,_1X){var _1Y=E(_1W);return new F(function(){return _1u(_1Y.a,_1Y.b,_1Y.c,_1Y.d,_1Y.f,_1X);});},_1Z=function(_20,_21){var _22=E(_20);return new F(function(){return _1u(_22.a,_22.b,_22.c,_22.d,_22.f,_21);});},_23=44,_24=93,_25=91,_26=function(_27,_28,_29){var _2a=E(_28);if(!_2a._){return new F(function(){return unAppCStr("[]",_29);});}else{var _2b=new T(function(){var _2c=new T(function(){var _2d=function(_2e){var _2f=E(_2e);if(!_2f._){return E(new T2(1,_24,_29));}else{var _2g=new T(function(){return B(A2(_27,_2f.a,new T(function(){return B(_2d(_2f.b));})));});return new T2(1,_23,_2g);}};return B(_2d(_2a.b));});return B(A2(_27,_2a.a,_2c));});return new T2(1,_25,_2b);}},_2h=function(_2i,_2j){return new F(function(){return _26(_1Z,_2i,_2j);});},_2k=new T3(0,_1U,_1O,_2h),_1T=new T(function(){return new T5(0,_O,_2k,_1R,_10,_1O);}),_2l=new T(function(){return E(_1T);}),_2m=function(_2n){return E(E(_2n).c);},_2o=__Z,_2p=7,_2q=function(_2r){return new T6(0,_2o,_2p,_6,_2r,_2o,_2o);},_2s=function(_2t,_){var _2u=new T(function(){return B(A2(_2m,_2l,new T(function(){return B(A1(_2q,_2t));})));});return new F(function(){return die(_2u);});},_2v=function(_2w,_){return new F(function(){return _2s(_2w,_);});},_2x=function(_2y){return new F(function(){return A1(_2v,_2y);});},_2z=function(_2A,_2B,_){var _2C=B(A1(_2A,_));return new F(function(){return A2(_2B,_2C,_);});},_2D=new T5(0,_I,_2z,_E,_C,_2x),_2E=function(_2F){return E(_2F);},_2G=new T2(0,_2D,_2E),_2H=function(_){return new F(function(){return __jsNull();});},_2I=function(_2J){var _2K=B(A1(_2J,_));return E(_2K);},_2L=new T(function(){return B(_2I(_2H));}),_2M=new T(function(){return E(_2L);}),_2N=new T(function(){return eval("window.requestAnimationFrame");}),_2O=function(_2P,_2Q,_2R){return function(_){var _2S=E(_2P),_2T=rMV(_2S),_2U=E(_2T);if(!_2U._){var _2V=B(A2(_2Q,_2U.a,_)),_2W=function(_2X,_){var _2Y=function(_){var _2Z=rMV(_2S),_30=function(_,_31){var _32=function(_){var _33=__createJSFunc(2,function(_34,_){var _35=B(_36(_,_));return _2M;}),_37=__app1(E(_2N),_33);return _7;},_38=E(_31);if(!_38._){return new F(function(){return _32(_);});}else{var _39=B(A2(_2Q,_38.a,_));return new F(function(){return _32(_);});}},_3a=E(_2Z);if(!_3a._){return new F(function(){return _30(_,new T1(1,_3a.a));});}else{return new F(function(){return _30(_,_2o);});}},_36=function(_3b,_){return new F(function(){return _2Y(_);});},_3c=B(_36(_,_));return _2M;},_3d=__createJSFunc(2,E(_2W)),_3e=__app1(E(_2N),_3d);return new T(function(){return B(A1(_2R,_7));});}else{var _3f=function(_3g,_){var _3h=function(_){var _3i=rMV(_2S),_3j=function(_,_3k){var _3l=function(_){var _3m=__createJSFunc(2,function(_3n,_){var _3o=B(_3p(_,_));return _2M;}),_3q=__app1(E(_2N),_3m);return _7;},_3r=E(_3k);if(!_3r._){return new F(function(){return _3l(_);});}else{var _3s=B(A2(_2Q,_3r.a,_));return new F(function(){return _3l(_);});}},_3t=E(_3i);if(!_3t._){return new F(function(){return _3j(_,new T1(1,_3t.a));});}else{return new F(function(){return _3j(_,_2o);});}},_3p=function(_3u,_){return new F(function(){return _3h(_);});},_3v=B(_3p(_,_));return _2M;},_3w=__createJSFunc(2,E(_3f)),_3x=__app1(E(_2N),_3w);return new T(function(){return B(A1(_2R,_7));});}};},_3y=new T0(2),_3z=function(_3A){return new T0(2);},_3B=function(_3C,_3D,_3E){return function(_){var _3F=E(_3C),_3G=rMV(_3F),_3H=E(_3G);if(!_3H._){var _3I=new T(function(){var _3J=new T(function(){return B(A1(_3E,_7));});return B(_2(_3H.b,new T2(1,new T2(0,_3D,function(_3K){return E(_3J);}),_6)));}),_=wMV(_3F,new T2(0,_3H.a,_3I));return _3y;}else{var _3L=E(_3H.a);if(!_3L._){var _=wMV(_3F,new T2(0,_3D,_6));return new T(function(){return B(A1(_3E,_7));});}else{var _=wMV(_3F,new T1(1,_3L.b));return new T1(1,new T2(1,new T(function(){return B(A1(_3E,_7));}),new T2(1,new T(function(){return B(A2(_3L.a,_3D,_3z));}),_6)));}}};},_3M=function(_3N){return E(E(_3N).b);},_3O=function(_3P,_3Q,_3R){var _3S=new T(function(){return new T1(0,B(_3B(_3Q,_3R,_3z)));}),_3T=function(_3U){return new T1(1,new T2(1,new T(function(){return B(A1(_3U,_7));}),new T2(1,_3S,_6)));};return new F(function(){return A2(_3M,_3P,_3T);});},_3V=function(_3W,_3X,_3Y){var _3Z=function(_40){var _41=new T(function(){return B(A1(_3Y,_40));});return new F(function(){return A1(_3X,function(_42){return E(_41);});});};return new F(function(){return A1(_3W,_3Z);});},_43=function(_44,_45,_46){var _47=new T(function(){return B(A1(_45,function(_48){return new F(function(){return A1(_46,_48);});}));});return new F(function(){return A1(_44,function(_49){return E(_47);});});},_4a=function(_4b,_4c,_4d){var _4e=function(_4f){var _4g=function(_4h){return new F(function(){return A1(_4d,new T(function(){return B(A1(_4f,_4h));}));});};return new F(function(){return A1(_4c,_4g);});};return new F(function(){return A1(_4b,_4e);});},_4i=function(_4j,_4k){return new F(function(){return A1(_4k,_4j);});},_4l=function(_4m,_4n,_4o){var _4p=new T(function(){return B(A1(_4o,_4m));});return new F(function(){return A1(_4n,function(_4q){return E(_4p);});});},_4r=function(_4s,_4t,_4u){var _4v=function(_4w){return new F(function(){return A1(_4u,new T(function(){return B(A1(_4s,_4w));}));});};return new F(function(){return A1(_4t,_4v);});},_4x=new T2(0,_4r,_4l),_4y=new T5(0,_4x,_4i,_4a,_43,_3V),_4z=function(_4A,_4B,_4C){return new F(function(){return A1(_4A,function(_4D){return new F(function(){return A2(_4B,_4D,_4C);});});});},_4E=function(_4F){return E(E(_4F).b);},_4G=function(_4H,_4I){return new F(function(){return A3(_4E,_4J,_4H,function(_4K){return E(_4I);});});},_4L=function(_4M){return new F(function(){return err(_4M);});},_4J=new T(function(){return new T5(0,_4y,_4z,_4G,_4i,_4L);}),_4N=function(_4O){var _4P=new T(function(){return B(A1(_4O,_3z));}),_4Q=function(_4R){return new T1(1,new T2(1,new T(function(){return B(A1(_4R,_7));}),new T2(1,_4P,_6)));};return E(_4Q);},_4S=new T3(0,_4J,_2E,_4N),_4T=new T2(0,_2G,_C),_4U=new T1(1,_6),_4V=function(_4W,_4X){return function(_){var _4Y=E(_4W),_4Z=rMV(_4Y),_50=E(_4Z);if(!_50._){var _51=_50.a,_52=E(_50.b);if(!_52._){var _=wMV(_4Y,_4U);return new T(function(){return B(A1(_4X,_51));});}else{var _53=E(_52.a),_=wMV(_4Y,new T2(0,_53.a,_52.b));return new T1(1,new T2(1,new T(function(){return B(A1(_4X,_51));}),new T2(1,new T(function(){return B(A1(_53.b,_3z));}),_6)));}}else{var _54=new T(function(){var _55=function(_56){var _57=new T(function(){return B(A1(_4X,_56));});return function(_58){return E(_57);};};return B(_2(_50.a,new T2(1,_55,_6)));}),_=wMV(_4Y,new T1(1,_54));return _3y;}};},_59=function(_5a){return E(E(_5a).a);},_5b=function(_5c){return E(E(_5c).a);},_5d=new T(function(){return eval("(function(t,f){window.setInterval(f,t);})");}),_5e=new T(function(){return eval("(function(t,f){window.setTimeout(f,t);})");}),_5f=function(_5g){return E(E(_5g).b);},_5h=function(_5i){return E(E(_5i).b);},_5j=function(_5k,_5l,_5m){var _5n=B(_59(_5k)),_5o=new T(function(){return B(_5f(_5n));}),_5p=function(_5q){var _5r=function(_){var _5s=E(_5l);if(!_5s._){var _5t=B(A1(_5q,_7)),_5u=__createJSFunc(0,function(_){var _5v=B(A1(_5t,_));return _2M;}),_5w=__app2(E(_5e),_5s.a,_5u);return new T(function(){var _5x=Number(_5w),_5y=jsTrunc(_5x);return new T2(0,_5y,E(_5s));});}else{var _5z=B(A1(_5q,_7)),_5A=__createJSFunc(0,function(_){var _5B=B(A1(_5z,_));return _2M;}),_5C=__app2(E(_5d),_5s.a,_5A);return new T(function(){var _5D=Number(_5C),_5E=jsTrunc(_5D);return new T2(0,_5E,E(_5s));});}};return new F(function(){return A1(_5o,_5r);});},_5F=new T(function(){return B(A2(_5h,_5k,function(_5G){return E(_5m);}));});return new F(function(){return A3(_4E,B(_5b(_5n)),_5F,_5p);});},_5H=new T1(1,_6),_5I=function(_5J,_5K){return function(_){var _5L=nMV(_5H),_5M=_5L,_5N=function(_){var _5O=function(_){return new F(function(){return _e(new T(function(){return new T1(0,B(_3B(_5M,_7,_3z)));}),_6,_);});},_5P=B(A(_5j,[_4T,new T(function(){return new T1(0,E(_5J));}),_5O,_]));return new T(function(){return new T1(0,B(_4V(_5M,_5K)));});};return new T1(0,_5N);};},_5Q=function(_5R,_5S,_5T){var _5U=new T(function(){var _5V=new T(function(){return B(_5W(_));}),_5X=new T(function(){var _5Y=function(_5Z){return E(_5V);};return B(A(_3O,[_4S,_5T,_5S,function(_60){return new T1(0,B(_5I(_5R,_5Y)));}]));}),_5W=function(_61){return E(_5X);};return B(_5W(_));});return function(_62){return E(_5U);};},_63=function(_){return _7;},_64=new T(function(){return eval("(function(ctx){ctx.beginPath();})");}),_65=new T(function(){return eval("(function(ctx){ctx.fill();})");}),_66=function(_67,_68,_){var _69=__app1(E(_64),_68),_6a=B(A2(_67,_68,_)),_6b=__app1(E(_65),_68);return new F(function(){return _63(_);});},_6c=new T(function(){return eval("(function(ctx){ctx.stroke();})");}),_6d=function(_6e,_6f,_){var _6g=__app1(E(_64),_6f),_6h=B(A2(_6e,_6f,_)),_6i=__app1(E(_6c),_6f);return new F(function(){return _63(_);});},_6j=new T(function(){return eval("(function(ctx){ctx.restore();})");}),_6k=new T(function(){return eval("(function(ctx){ctx.save();})");}),_6l=new T(function(){return eval("(function(ctx,x,y){ctx.translate(x,y);})");}),_6m=function(_6n,_6o,_6p,_6q,_){var _6r=__app1(E(_6k),_6q),_6s=__app3(E(_6l),_6q,E(_6n),E(_6o)),_6t=B(A2(_6p,_6q,_)),_6u=__app1(E(_6j),_6q);return new F(function(){return _63(_);});},_6v=function(_6w){var _6x=I_decodeDouble(_6w);return new T2(0,new T1(1,_6x.b),_6x.a);},_6y=function(_6z,_6A){var _6B=E(_6z);if(!_6B._){var _6C=_6B.a,_6D=E(_6A);return (_6D._==0)?_6C==_6D.a:(I_compareInt(_6D.a,_6C)==0)?true:false;}else{var _6E=_6B.a,_6F=E(_6A);return (_6F._==0)?(I_compareInt(_6E,_6F.a)==0)?true:false:(I_compare(_6E,_6F.a)==0)?true:false;}},_6G=new T1(0,0),_6H=function(_6I){var _6J=B(_6v(_6I));return (!B(_6y(_6J.a,_6G)))?_6J.b+53|0:0;},_6K=function(_6L,_6M){var _6N=E(_6L);return (_6N._==0)?_6N.a*Math.pow(2,_6M):I_toNumber(_6N.a)*Math.pow(2,_6M);},_6O=function(_6P,_6Q){var _6R=E(_6P);if(!_6R){return E(_6Q);}else{if(_6Q!=0){var _6S=isDoubleFinite(_6Q);if(!E(_6S)){return E(_6Q);}else{var _6T=B(_6v(_6Q)),_6U=_6T.a,_6V=_6T.b;if(2257>_6R){if(-2257>_6R){return new F(function(){return _6K(_6U,_6V+(-2257)|0);});}else{return new F(function(){return _6K(_6U,_6V+_6R|0);});}}else{return new F(function(){return _6K(_6U,_6V+2257|0);});}}}else{return E(_6Q);}}},_6W=function(_6X,_6Y){var _6Z=B(_6H(_6X)),_70=B(_6H(_6Y)),_71=function(_72){var _73= -_72,_74=B(_6O(_73,_6X)),_75=B(_6O(_73,_6Y));return new F(function(){return _6O(_72,Math.sqrt(_74*_74+_75*_75));});};if(_6Z>_70){return new F(function(){return _71(_6Z);});}else{return new F(function(){return _71(_70);});}},_76=function(_77,_78){if(_78<=0){var _79=function(_7a){var _7b=function(_7c){var _7d=function(_7e){var _7f=function(_7g){var _7h=isDoubleNegativeZero(_78),_7i=_7h,_7j=function(_7k){var _7l=E(_77);return (_7l!=0)?_78+_7l:(_78>=0)?(E(_7i)==0)?(_78!=0)?_78+_7l:E(_7l):3.141592653589793:3.141592653589793;};if(!E(_7i)){return new F(function(){return _7j(_);});}else{var _7m=E(_77),_7n=isDoubleNegativeZero(_7m);if(!E(_7n)){return new F(function(){return _7j(_);});}else{return  -B(_76( -_7m,_78));}}};if(_78>=0){return new F(function(){return _7f(_);});}else{var _7o=E(_77),_7p=isDoubleNegativeZero(_7o);if(!E(_7p)){return new F(function(){return _7f(_);});}else{return  -B(_76( -_7o,_78));}}};if(_78>0){return new F(function(){return _7d(_);});}else{var _7q=E(_77);if(_7q>=0){return new F(function(){return _7d(_);});}else{return  -B(_76( -_7q,_78));}}};if(_78>=0){return new F(function(){return _7b(_);});}else{var _7r=E(_77);if(_7r<=0){return new F(function(){return _7b(_);});}else{return 3.141592653589793+Math.atan(_7r/_78);}}};if(_78!=0){return new F(function(){return _79(_);});}else{if(E(_77)<=0){return new F(function(){return _79(_);});}else{return 1.5707963267948966;}}}else{return new F(function(){return Math.atan(E(_77)/_78);});}},_7s=function(_7t,_7u){if(_7t!=0){return new F(function(){return _76(_7u,_7t);});}else{if(_7u!=0){return new F(function(){return _76(_7u,_7t);});}else{return 0;}}},_7v=new T(function(){return  -0;}),_7w=new T2(0,E(_7v),E(_7v)),_7x=0,_7y=function(_7z,_7A,_7B,_7C,_7D){var _7E=_7A+_7C,_7F=_7E+_7B,_7G=_7F-_7C,_7H=_7F-_7G,_7I=_7F-_7E,_7J=function(_7K){var _7L= -_7C,_7M=Math.sin(B(_7s( -_7L, -_7B))),_7N=function(_7O){var _7P=function(_7Q,_7R){var _7S=_7K/_7O,_7T= -_7B* -(_7Q*0+_7R*_7S)+_7L*(_7Q*_7S-_7R*0),_7U=function(_7V){var _7W=new T(function(){if(_7B!=0){var _7X=B(_6W(_7B,_7C));return new T2(0, -(_7B/_7X), -(_7C/_7X));}else{if(_7C!=0){var _7Y=B(_6W(_7B,_7C));return new T2(0, -(_7B/_7Y), -(_7C/_7Y));}else{return E(_7w);}}}),_7Z=new T(function(){var _80=Math.sin(B(_7s( -_7B,_7L)));if(_80!=0){if(_80<=0){return  -_80;}else{return _80;}}else{return E(_7x);}}),_81=_7z+_7B,_82=_81+_7L;if(_7D>_7V){var _83=B(_6W(_7B+(_7Q*_7S-_7R*0),_7C+_7Q*0+_7R*_7S)),_84=function(_85){if(_7D>_85){var _86=_7F-_7A,_87=new T(function(){var _88=(_86-_7I)/_7O,_89=_7B* -(_7Q*0+_7R*_88)+_7C*(_7Q*_88-_7R*0);if(_89!=0){if(_89<=0){return _86-Math.sqrt( -_89/2-(_7D-_85));}else{return _86-Math.sqrt(_89/2-(_7D-_85));}}else{return _86-Math.sqrt( -(_7D-_85));}});return new T2(1,new T2(0,E(_82),E(_7F)),new T2(1,new T2(0,E(_81),E(_7E)),new T2(1,new T(function(){var _8a=E(_7W),_8b=E(_8a.a),_8c=E(_8a.b),_8d=(_86-E(_87))/E(_7Z);return new T2(0,_7z-(_8b*_8d-_8c*0),_7A-(_8b*0+_8c*_8d));}),new T2(1,new T(function(){var _8e=(_86-E(_87))/_7O;return new T2(0,_7z-(_7Q*_8e-_7R*0),_7A-(_7Q*0+_7R*_8e));}),new T2(1,new T2(0,_82-_7B,E(_7G)),_6)))));}else{var _8f=new T(function(){return (_7D-_7V)/_83+_7K;});return new T2(1,new T2(0,E(_82),E(_7F)),new T2(1,new T2(0,_82-_7B,E(_7G)),new T2(1,new T(function(){var _8g=(E(_8f)-_7H)/_7O;return new T2(0,_82-_7B+(_7Q*_8g-_7R*0),_7G+_7Q*0+_7R*_8g);}),new T2(1,new T(function(){var _8h=E(_8f)/_7O;return new T2(0,_82+(_7Q*_8h-_7R*0),_7F+_7Q*0+_7R*_8h);}),_6))));}};if(_7I>_7H){return new F(function(){return _84(_7V+_83*(_7I-_7K));});}else{return new F(function(){return _84(_7V+_83*(_7H-_7K));});}}else{var _8i=new T(function(){return _7K*Math.sqrt(_7D/_7V);});return new T2(1,new T2(0,E(_82),E(_7F)),new T2(1,new T(function(){var _8j=E(_8i)/_7O;return new T2(0,_82+(_7Q*_8j-_7R*0),_7F+_7Q*0+_7R*_8j);}),new T2(1,new T(function(){var _8k=E(_7W),_8l=E(_8k.a),_8m=E(_8k.b),_8n=E(_8i)/E(_7Z);return new T2(0,_82+(_8l*_8n-_8m*0),_7F+_8l*0+_8m*_8n);}),_6)));}};if(_7T!=0){if(_7T<=0){return new F(function(){return _7U( -_7T/2);});}else{return new F(function(){return _7U(_7T/2);});}}else{return new F(function(){return _7U(0);});}};if(_7L!=0){var _8o=B(_6W(_7L,_7B));return new F(function(){return _7P( -(_7L/_8o), -(_7B/_8o));});}else{if(_7B!=0){var _8p=B(_6W(_7L,_7B));return new F(function(){return _7P( -(_7L/_8p), -(_7B/_8p));});}else{return new F(function(){return _7P( -0, -0);});}}};if(_7M!=0){if(_7M<=0){return new F(function(){return _7N( -_7M);});}else{return new F(function(){return _7N(_7M);});}}else{return new F(function(){return _7N(0);});}};if(_7I>_7H){return new F(function(){return _7J(_7H);});}else{return new F(function(){return _7J(_7I);});}},_8q=new T(function(){return eval("(function(e){e.width = e.width;})");}),_8r=",",_8s="rgba(",_8t=new T(function(){return toJSStr(_6);}),_8u="rgb(",_8v=")",_8w=new T2(1,_8v,_6),_8x=function(_8y){var _8z=E(_8y);if(!_8z._){var _8A=jsCat(new T2(1,_8u,new T2(1,new T(function(){return String(_8z.a);}),new T2(1,_8r,new T2(1,new T(function(){return String(_8z.b);}),new T2(1,_8r,new T2(1,new T(function(){return String(_8z.c);}),_8w)))))),E(_8t));return E(_8A);}else{var _8B=jsCat(new T2(1,_8s,new T2(1,new T(function(){return String(_8z.a);}),new T2(1,_8r,new T2(1,new T(function(){return String(_8z.b);}),new T2(1,_8r,new T2(1,new T(function(){return String(_8z.c);}),new T2(1,_8r,new T2(1,new T(function(){return String(_8z.d);}),_8w)))))))),E(_8t));return E(_8B);}},_8C="strokeStyle",_8D="fillStyle",_8E=new T(function(){return eval("(function(e,p){var x = e[p];return typeof x === \'undefined\' ? \'\' : x.toString();})");}),_8F=new T(function(){return eval("(function(e,p,v){e[p] = v;})");}),_8G=function(_8H,_8I){var _8J=new T(function(){return B(_8x(_8H));});return function(_8K,_){var _8L=E(_8K),_8M=E(_8D),_8N=E(_8E),_8O=__app2(_8N,_8L,_8M),_8P=E(_8C),_8Q=__app2(_8N,_8L,_8P),_8R=E(_8J),_8S=E(_8F),_8T=__app3(_8S,_8L,_8M,_8R),_8U=__app3(_8S,_8L,_8P,_8R),_8V=B(A2(_8I,_8L,_)),_8W=String(_8O),_8X=__app3(_8S,_8L,_8M,_8W),_8Y=String(_8Q),_8Z=__app3(_8S,_8L,_8P,_8Y);return new F(function(){return _63(_);});};},_90=new T3(0,40,40,40),_91=0,_92=new T(function(){return eval("(function(ctx,x,y){ctx.moveTo(x,y);})");}),_93=new T(function(){return eval("(function(ctx,x,y){ctx.lineTo(x,y);})");}),_94=function(_95,_96,_){var _97=E(_95);if(!_97._){return _7;}else{var _98=E(_97.a),_99=E(_96),_9a=__app3(E(_92),_99,E(_98.a),E(_98.b)),_9b=E(_97.b);if(!_9b._){return _7;}else{var _9c=E(_9b.a),_9d=E(_93),_9e=__app3(_9d,_99,E(_9c.a),E(_9c.b)),_9f=function(_9g,_){while(1){var _9h=E(_9g);if(!_9h._){return _7;}else{var _9i=E(_9h.a),_9j=__app3(_9d,_99,E(_9i.a),E(_9i.b));_9g=_9h.b;continue;}}};return new F(function(){return _9f(_9b.b,_);});}}},_9k="lineWidth",_9l=function(_9m,_9n){var _9o=new T(function(){return String(E(_9m));});return function(_9p,_){var _9q=E(_9p),_9r=E(_9k),_9s=__app2(E(_8E),_9q,_9r),_9t=E(_8F),_9u=__app3(_9t,_9q,_9r,E(_9o)),_9v=B(A2(_9n,_9q,_)),_9w=String(_9s),_9x=__app3(_9t,_9q,_9r,_9w);return new F(function(){return _63(_);});};},_9y=2,_9z=new T3(0,255,255,255),_9A=new T(function(){return Math.acos(0.6);}),_9B=new T(function(){var _9C=1.5707963267948966-E(_9A);return new T2(0,150*Math.cos(_9C),150*Math.sin(_9C));}),_9D=new T(function(){var _9E=3.141592653589793-E(_9A);return new T2(0,200*Math.cos(_9E),200*Math.sin(_9E));}),_9F=new T(function(){var _9G=1.5707963267948966-E(_9A);return new T2(0,150*Math.cos(_9G),150*Math.sin(_9G));}),_9H=new T(function(){var _9I=1.5707963267948966-E(_9A);return new T2(0,200*Math.cos(_9I),200*Math.sin(_9I));}),_9J=function(_9K,_9L){var _9M=E(_9L);return (_9M._==0)?__Z:new T2(1,new T(function(){return B(A1(_9K,_9M.a));}),new T(function(){return B(_9J(_9K,_9M.b));}));},_9N=new T3(0,0,128,255),_9O=new T(function(){return eval("(function(){return Util.height;})");}),_9P=new T(function(){return eval("(function(){return Util.width;})");}),_9Q=function(_){var _9R=E(_2M),_9S=__app1(E(_9P),_9R),_9T=__app1(E(_9O),_9R);return new T2(0,_9S,_9T);},_9U=function(_9V,_9W,_){var _9X=B(_9Q(_)),_9Y=E(_9X),_9Z=E(_9V),_a0=__app1(E(_8q),_9Z.b),_a1=new T(function(){return E(E(E(_9W).a).c);}),_a2=new T(function(){return E(E(E(_9W).b).a);}),_a3=new T(function(){return 25-E(_a2);}),_a4=new T(function(){return E(E(E(_9W).a).b);}),_a5=new T(function(){return E(E(E(_9W).b).b);}),_a6=new T(function(){return E(E(_a5).a);}),_a7=new T(function(){return E(E(_a5).b);}),_a8=new T2(1,_a7,_6),_a9=new T2(1,_a6,new T2(1,new T(function(){var _aa=E(_a6);return new T2(0,E(_aa.a),E(_aa.b)+(-250));}),new T2(1,new T(function(){var _ab=E(_a7);return new T2(0,E(_ab.a),E(_ab.b)+(-250));}),_a8))),_ac=new T(function(){return E(E(_a5).c);}),_ad=new T2(1,_a7,new T2(1,new T(function(){var _ae=E(_a7),_af=E(_9B);return new T2(0,E(_ae.a)+E(_af.a),E(_ae.b)+E(_af.b));}),new T2(1,new T(function(){var _ag=E(_ac),_ah=E(_9B);return new T2(0,E(_ag.a)+E(_ah.a),E(_ag.b)+E(_ah.b));}),new T2(1,_ac,_6)))),_ai=new T2(1,_ac,new T2(1,new T(function(){var _aj=E(_ac),_ak=E(_9D);return new T2(0,E(_aj.a)+E(_ak.a),E(_aj.b)+E(_ak.b));}),new T2(1,new T(function(){var _al=E(_a6),_am=E(_9D);return new T2(0,E(_al.a)+E(_am.a),E(_al.b)+E(_am.b));}),new T2(1,_a6,_6)))),_an=function(_ao,_){var _ap=function(_){var _aq=new T(function(){if(!E(_a4)){return E(_9N);}else{return E(_9z);}}),_ar=new T(function(){var _as=new T(function(){return new T2(0,E(_91), -(50*(E(_a2)/5)));}),_at=new T(function(){var _au=E(_a1);return new T2(0,E(Math.cos(_au)),E(Math.sin(_au)));});return B(_9J(function(_av){var _aw=E(_av),_ax=E(_aw.a),_ay=E(_aw.b),_az=E(_at),_aA=E(_az.a),_aB=E(_az.b);return new T2(0,_ax*_aA-_ay*_aB,_ax*_aB+_ay*_aA);},new T2(1,_a6,new T2(1,new T(function(){var _aC=E(_a6),_aD=E(_as);return new T2(0,E(_aC.a)+E(_aD.a),E(_aC.b)+E(_aD.b));}),new T2(1,new T(function(){var _aE=E(_a7),_aF=E(_as);return new T2(0,E(_aE.a)+E(_aF.a),E(_aE.b)+E(_aF.b));}),_a8)))));}),_aG=function(_aH,_){return new F(function(){return _94(_ar,_aH,_);});},_aI=B(A(_8G,[_aq,function(_aJ,_){return new F(function(){return _66(_aG,E(_aJ),_);});},_ao,_])),_aK=new T(function(){var _aL=E(_a7),_aM=E(_9F),_aN=new T(function(){var _aO=E(_a1);return new T2(0,E(Math.cos(_aO)),E(Math.sin(_aO)));});return B(_9J(function(_aP){var _aQ=E(_aP),_aR=E(_aQ.a),_aS=E(_aQ.b),_aT=E(_aN),_aU=E(_aT.a),_aV=E(_aT.b);return new T2(0,_aR*_aU-_aS*_aV,_aR*_aV+_aS*_aU);},B(_7y(E(_aL.a),E(_aL.b),E(_aM.a),E(_aM.b),2500*E(_a3)*Math.pow(0.6,2)))));}),_aW=function(_aH,_){return new F(function(){return _94(_aK,_aH,_);});},_aX=B(A(_8G,[_aq,function(_aY,_){return new F(function(){return _66(_aW,E(_aY),_);});},_ao,_])),_aZ=new T(function(){var _b0=E(_a6),_b1=E(_9H),_b2=new T(function(){var _b3=E(_a1);return new T2(0,E(Math.cos(_b3)),E(Math.sin(_b3)));});return B(_9J(function(_b4){var _b5=E(_b4),_b6=E(_b5.a),_b7=E(_b5.b),_b8=E(_b2),_b9=E(_b8.a),_ba=E(_b8.b);return new T2(0,_b6*_b9-_b7*_ba,_b6*_ba+_b7*_b9);},B(_7y(E(_b0.a),E(_b0.b),E(_b1.a),E(_b1.b),2500*E(_a3)*Math.pow(0.8,2)))));}),_bb=function(_aH,_){return new F(function(){return _94(_aZ,_aH,_);});},_bc=B(A(_8G,[_aq,function(_bd,_){return new F(function(){return _66(_bb,E(_bd),_);});},_ao,_])),_be=new T(function(){var _bf=new T(function(){var _bg=new T(function(){var _bh=E(_a1);return new T2(0,E(Math.cos(_bh)),E(Math.sin(_bh)));}),_bi=new T(function(){var _bj=E(_a6),_bk=E(_bj.a),_bl=E(_bj.b),_bm=E(_bg),_bn=E(_bm.a),_bo=E(_bm.b);return new T2(0,_bk*_bn-_bl*_bo,_bk*_bo+_bl*_bn);});return B(_2(new T2(1,_bi,new T2(1,new T(function(){var _bp=E(_a7),_bq=E(_bp.a),_br=E(_bp.b),_bs=E(_bg),_bt=E(_bs.a),_bu=E(_bs.b);return new T2(0,_bq*_bt-_br*_bu,_bq*_bu+_br*_bt);}),new T2(1,new T(function(){var _bv=E(_ac),_bw=E(_bv.a),_bx=E(_bv.b),_by=E(_bg),_bz=E(_by.a),_bA=E(_by.b);return new T2(0,_bw*_bz-_bx*_bA,_bw*_bA+_bx*_bz);}),_6))),new T2(1,_bi,_6)));}),_bB=function(_aH,_){return new F(function(){return _94(_bf,_aH,_);});};return B(_9l(_9y,function(_bC,_){return new F(function(){return _6d(_bB,E(_bC),_);});}));}),_bD=B(A(_8G,[_90,_be,_ao,_])),_bE=new T(function(){var _bF=new T(function(){var _bG=new T(function(){var _bH=E(_a1);return new T2(0,E(Math.cos(_bH)),E(Math.sin(_bH)));});return B(_9J(function(_bI){var _bJ=E(_bI),_bK=E(_bJ.a),_bL=E(_bJ.b),_bM=E(_bG),_bN=E(_bM.a),_bO=E(_bM.b);return new T2(0,_bK*_bN-_bL*_bO,_bK*_bO+_bL*_bN);},_a9));}),_bP=function(_aH,_){return new F(function(){return _94(_bF,_aH,_);});};return B(_9l(_9y,function(_bQ,_){return new F(function(){return _6d(_bP,E(_bQ),_);});}));}),_bR=B(A(_8G,[_90,_bE,_ao,_])),_bS=new T(function(){var _bT=new T(function(){var _bU=new T(function(){var _bV=E(_a1);return new T2(0,E(Math.cos(_bV)),E(Math.sin(_bV)));});return B(_9J(function(_bW){var _bX=E(_bW),_bY=E(_bX.a),_bZ=E(_bX.b),_c0=E(_bU),_c1=E(_c0.a),_c2=E(_c0.b);return new T2(0,_bY*_c1-_bZ*_c2,_bY*_c2+_bZ*_c1);},_ad));}),_c3=function(_aH,_){return new F(function(){return _94(_bT,_aH,_);});};return B(_9l(_9y,function(_c4,_){return new F(function(){return _6d(_c3,E(_c4),_);});}));}),_c5=B(A(_8G,[_90,_bS,_ao,_])),_c6=new T(function(){var _c7=new T(function(){var _c8=new T(function(){var _c9=E(_a1);return new T2(0,E(Math.cos(_c9)),E(Math.sin(_c9)));});return B(_9J(function(_ca){var _cb=E(_ca),_cc=E(_cb.a),_cd=E(_cb.b),_ce=E(_c8),_cf=E(_ce.a),_cg=E(_ce.b);return new T2(0,_cc*_cf-_cd*_cg,_cc*_cg+_cd*_cf);},_ai));}),_ch=function(_aH,_){return new F(function(){return _94(_c7,_aH,_);});};return B(_9l(_9y,function(_ci,_){return new F(function(){return _6d(_ch,E(_ci),_);});}));});return new F(function(){return A(_8G,[_90,_c6,_ao,_]);});};if(!E(_a4)){return new F(function(){return _ap(_);});}else{var _cj=new T(function(){var _ck=new T(function(){var _cl=E(_a1);return new T2(0,E(Math.cos(_cl)),E(Math.sin(_cl)));});return B(_9J(function(_cm){var _cn=E(_cm),_co=E(_cn.a),_cp=E(_cn.b),_cq=E(_ck),_cr=E(_cq.a),_cs=E(_cq.b);return new T2(0,_co*_cr-_cp*_cs,_co*_cs+_cp*_cr);},_a9));}),_ct=function(_aH,_){return new F(function(){return _94(_cj,_aH,_);});},_cu=B(A(_8G,[_9N,function(_cv,_){return new F(function(){return _66(_ct,E(_cv),_);});},_ao,_])),_cw=new T(function(){var _cx=new T(function(){var _cy=E(_a1);return new T2(0,E(Math.cos(_cy)),E(Math.sin(_cy)));});return B(_9J(function(_cz){var _cA=E(_cz),_cB=E(_cA.a),_cC=E(_cA.b),_cD=E(_cx),_cE=E(_cD.a),_cF=E(_cD.b);return new T2(0,_cB*_cE-_cC*_cF,_cB*_cF+_cC*_cE);},_ad));}),_cG=function(_aH,_){return new F(function(){return _94(_cw,_aH,_);});},_cH=B(A(_8G,[_9N,function(_cI,_){return new F(function(){return _66(_cG,E(_cI),_);});},_ao,_])),_cJ=new T(function(){var _cK=new T(function(){var _cL=E(_a1);return new T2(0,E(Math.cos(_cL)),E(Math.sin(_cL)));});return B(_9J(function(_cM){var _cN=E(_cM),_cO=E(_cN.a),_cP=E(_cN.b),_cQ=E(_cK),_cR=E(_cQ.a),_cS=E(_cQ.b);return new T2(0,_cO*_cR-_cP*_cS,_cO*_cS+_cP*_cR);},_ai));}),_cT=function(_aH,_){return new F(function(){return _94(_cJ,_aH,_);});},_cU=B(A(_8G,[_9N,function(_cV,_){return new F(function(){return _66(_cT,E(_cV),_);});},_ao,_]));return new F(function(){return _ap(_);});}},_cW=B(_6m(new T(function(){return E(_9Y.a)/2;},1),new T(function(){return E(_9Y.b)/2;},1),_an,_9Z.a,_));return _7;},_cX=16,_cY=new T1(1,_6),_cZ=new T1(1,_6),_d0=false,_d1=0,_d2=new T4(0,_d1,_d0,_91,_91),_d3=function(_d4,_d5,_d6,_d7){var _d8=B(_6H(_d6)),_d9=B(_6H(_d7));if(_d8>_d9){var _da= -_d8,_db=B(_6O(_da,_d6)),_dc=B(_6O(_da,_d7)),_dd=_d6*_db+_d7*_dc;return new T2(0,(_d4*_db+_d5*_dc)/_dd,(_d5*_db-_d4*_dc)/_dd);}else{var _de= -_d9,_df=B(_6O(_de,_d6)),_dg=B(_6O(_de,_d7)),_dh=_d6*_df+_d7*_dg;return new T2(0,(_d4*_df+_d5*_dg)/_dh,(_d5*_df-_d4*_dg)/_dh);}},_di=new T(function(){var _dj=B(_d3(-70,-120,2,0)),_dk=E(_dj.a);return new T2(0,E(_dk),E(_dj.b)+60);}),_dl=new T(function(){var _dm=E(_di);return new T2(0, -E(_dm.a),60-E(_dm.b));}),_dn=new T(function(){var _do=E(_di);return new T2(0,90-E(_do.a),-60-E(_do.b));}),_dp=new T(function(){var _dq=E(_di);return new T2(0,-160-E(_dq.a),-60-E(_dq.b));}),_dr=new T3(0,_dp,_dn,_dl),_ds=25,_dt=new T2(0,_ds,_dr),_du=new T2(0,_d2,_dt),_dv=new T2(0,_du,_6),_dw=function(_dx,_dy,_dz,_dA,_dB,_dC,_dD){var _dE=_dA-_dz;if(_dE!=0){if(_dE<=0){if( -_dE>=1.0e-2){if(_dB>0){return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),_dy,_dz+_dE/10,_dA),new T2(0,_dB,_dC))));});}else{return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),new T(function(){if(!E(_dy)){return true;}else{return false;}}),_dz+_dE/10,_dA+3.141592653589793),new T2(0,_ds,_dC))));});}}else{var _dF=_dB-0.1;if(_dF>0){return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),_dy,_dz+_dE/10,_dA),new T2(0,_dF,_dC))));});}else{return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),new T(function(){if(!E(_dy)){return true;}else{return false;}}),_dz+_dE/10,_dA+3.141592653589793),new T2(0,_ds,_dC))));});}}}else{if(_dE>=1.0e-2){if(_dB>0){return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),_dy,_dz+_dE/10,_dA),new T2(0,_dB,_dC))));});}else{return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),new T(function(){if(!E(_dy)){return true;}else{return false;}}),_dz+_dE/10,_dA+3.141592653589793),new T2(0,_ds,_dC))));});}}else{var _dG=_dB-0.1;if(_dG>0){return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),_dy,_dz+_dE/10,_dA),new T2(0,_dG,_dC))));});}else{return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),new T(function(){if(!E(_dy)){return true;}else{return false;}}),_dz+_dE/10,_dA+3.141592653589793),new T2(0,_ds,_dC))));});}}}}else{var _dH=_dB-0.1;if(_dH>0){return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),_dy,_dz+_dE/10,_dA),new T2(0,_dH,_dC))));});}else{return new F(function(){return A1(_dD,new T2(0,_7,new T2(0,new T4(0,new T(function(){return E(_dx)+1|0;}),new T(function(){if(!E(_dy)){return true;}else{return false;}}),_dz+_dE/10,_dA+3.141592653589793),new T2(0,_ds,_dC))));});}}},_dI=function(_dJ,_dK){var _dL=E(_dJ),_dM=E(_dL.a),_dN=E(_dL.b);return new F(function(){return _dw(_dM.a,_dM.b,E(_dM.c),E(_dM.d),E(_dN.a),_dN.b,_dK);});},_dO=function(_dP){return E(E(_dP).a);},_dQ=function(_dR){return E(E(_dR).c);},_dS=function(_dT,_dU,_dV){return new T1(0,B(_3B(_dT,_dU,_dV)));},_dW=function(_dX){return E(E(_dX).d);},_dY=function(_dZ,_e0){return new T1(0,B(_4V(_dZ,_e0)));},_e1=function(_e2,_e3,_e4){var _e5=new T(function(){return B(_3M(_e2));}),_e6=B(_dO(_e2)),_e7=function(_e8,_e9){var _ea=new T(function(){return B(A1(_e5,function(_eb){return new F(function(){return _dS(_e3,_e9,_eb);});}));});return new F(function(){return A3(_dQ,_e6,_ea,new T(function(){return B(A2(_dW,_e6,_e8));}));});},_ec=function(_ed){var _ee=E(_ed);return new F(function(){return _e7(_ee.a,_ee.b);});},_ef=function(_eg){return new F(function(){return A3(_4E,_e6,new T(function(){return B(A1(_e4,_eg));}),_ec);});},_eh=new T(function(){return B(A2(_3M,_e2,function(_eb){return new F(function(){return _dY(_e3,_eb);});}));});return new F(function(){return A3(_4E,_e6,_eh,_ef);});},_ei=function(_ej,_ek){var _el=new T(function(){var _em=new T(function(){return B(_en(_));}),_eo=new T(function(){var _ep=new T(function(){return B(A(_e1,[_4S,_ej,_dI,function(_eq){return E(_em);}]));});return new T1(0,B(_4V(_ek,function(_er){return E(_ep);})));}),_en=function(_es){return E(_eo);};return B(_en(_));});return function(_et){return E(_el);};},_eu=function(_ev,_ew){return function(_){var _ex=nMV(_dv),_ey=_ex;return new T(function(){var _ez=function(_){var _eA=nMV(_cZ),_eB=_eA,_eC=function(_){var _eD=nMV(_cY);return new T1(1,new T2(1,new T(function(){return B(A1(_ew,_7));}),new T2(1,new T(function(){return B(A(_5Q,[_cX,_7,_eB,_3z]));}),_6)));};return new T1(1,new T2(1,new T1(0,_eC),new T2(1,new T(function(){return B(A3(_ei,_ey,_eB,_3z));}),_6)));};return new T1(0,B(_2O(_ey,function(_aH,_){return new F(function(){return _9U(_ev,_aH,_);});},function(_eE){return E(new T1(0,_ez));})));});};},_eF=new T(function(){return eval("(function(id){return document.getElementById(id);})");}),_eG=function(_eH,_eI){var _eJ=function(_){var _eK=__app1(E(_eF),E(_eI)),_eL=__eq(_eK,E(_2M));return (E(_eL)==0)?new T1(1,_eK):_2o;};return new F(function(){return A2(_5f,_eH,_eJ);});},_eM=new T(function(){return B(unCStr("Canvas not found!"));}),_eN=new T(function(){return B(err(_eM));}),_eO="canvas",_eP=new T(function(){return eval("(Util.onload)");}),_eQ=function(_){var _eR=__app1(E(_eP),E(_2M)),_eS=B(A3(_eG,_2G,_eO,_)),_eT=E(_eS);if(!_eT._){return E(_eN);}else{var _eU=E(_eT.a),_eV=__app1(E(_1),_eU);if(!_eV){return E(_eN);}else{var _eW=__app1(E(_0),_eU);return new F(function(){return _e(new T(function(){return new T1(0,B(_eu(new T2(0,_eW,_eU),_3z)));}),_6,_);});}}},_eX=function(_){return new F(function(){return _eQ(_);});};
var hasteMain = function() {B(A(_eX, [0]));};window.onload = hasteMain;