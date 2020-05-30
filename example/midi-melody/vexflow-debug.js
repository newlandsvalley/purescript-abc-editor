(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory();
	else if(typeof define === 'function' && define.amd)
		define([], factory);
	else if(typeof exports === 'object')
		exports["Vex"] = factory();
	else
		root["Vex"] = factory();
})(typeof self !== 'undefined' ? self : this, function() {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 64);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

// [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This file implements utility methods used by the rest of the VexFlow
// codebase.
//

var Vex = function Vex() {};

// Default log function sends all arguments to console.
Vex.L = function (block, args) {
  if (!args) return;
  var line = Array.prototype.slice.call(args).join(' ');
  window.console.log(block + ': ' + line);
};

Vex.MakeException = function (name) {
  var exception = function (_Error) {
    _inherits(exception, _Error);

    function exception(message, data) {
      _classCallCheck(this, exception);

      var _this = _possibleConstructorReturn(this, (exception.__proto__ || Object.getPrototypeOf(exception)).call(this, message));

      _this.name = name;
      _this.message = message;
      _this.data = data;
      return _this;
    }

    return exception;
  }(Error);

  return exception;
};

// Default runtime exception.

var RuntimeError = function () {
  function RuntimeError(code, message) {
    _classCallCheck(this, RuntimeError);

    this.code = code;
    this.message = message;
  }

  _createClass(RuntimeError, [{
    key: 'toString',
    value: function toString() {
      return '[RuntimeError] ' + this.code + ':' + this.message;
    }
  }]);

  return RuntimeError;
}();

// Shortcut method for `RuntimeError`.


Vex.RuntimeError = RuntimeError;
Vex.RERR = Vex.RuntimeError;

// Merge `destination` hash with `source` hash, overwriting like keys
// in `source` if necessary.
Vex.Merge = function (destination, source) {
  for (var property in source) {
    // eslint-disable-line guard-for-in
    destination[property] = source[property];
  }
  return destination;
};

// DEPRECATED. Use `Math.*`.
Vex.Min = Math.min;
Vex.Max = Math.max;
Vex.forEach = function (a, fn) {
  for (var i = 0; i < a.length; i++) {
    fn(a[i], i);
  }
};

// Round number to nearest fractional value (`.5`, `.25`, etc.)
Vex.RoundN = function (x, n) {
  return x % n >= n / 2 ? parseInt(x / n, 10) * n + n : parseInt(x / n, 10) * n;
};

// Locate the mid point between stave lines. Returns a fractional line if a space.
Vex.MidLine = function (a, b) {
  var mid_line = b + (a - b) / 2;
  if (mid_line % 2 > 0) {
    mid_line = Vex.RoundN(mid_line * 10, 5) / 10;
  }
  return mid_line;
};

// Take `arr` and return a new list consisting of the sorted, unique,
// contents of arr. Does not modify `arr`.
Vex.SortAndUnique = function (arr, cmp, eq) {
  if (arr.length > 1) {
    var newArr = [];
    var last = void 0;
    arr.sort(cmp);

    for (var i = 0; i < arr.length; ++i) {
      if (i === 0 || !eq(arr[i], last)) {
        newArr.push(arr[i]);
      }
      last = arr[i];
    }

    return newArr;
  } else {
    return arr;
  }
};

// Check if array `a` contains `obj`.
Vex.Contains = function (a, obj) {
  var i = a.length;
  while (i--) {
    if (a[i] === obj) {
      return true;
    }
  }
  return false;
};

// Get the 2D Canvas context from DOM element `canvas_sel`.
Vex.getCanvasContext = function (canvas_sel) {
  if (!canvas_sel) {
    throw new Vex.RERR('BadArgument', 'Invalid canvas selector: ' + canvas_sel);
  }

  var canvas = document.getElementById(canvas_sel);
  if (!(canvas && canvas.getContext)) {
    throw new Vex.RERR('UnsupportedBrowserError', 'This browser does not support HTML5 Canvas');
  }

  return canvas.getContext('2d');
};

// Draw a tiny dot marker on the specified canvas. A great debugging aid.
//
// `ctx`: Canvas context.
// `x`, `y`: Dot coordinates.
Vex.drawDot = function (ctx, x, y) {
  var color = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : '#55';

  ctx.save();
  ctx.setFillStyle(color);

  // draw a circle
  ctx.beginPath();
  ctx.arc(x, y, 3, 0, Math.PI * 2, true);
  ctx.closePath();
  ctx.fill();
  ctx.restore();
};

// Benchmark. Run function `f` once and report time elapsed shifted by `s` milliseconds.
Vex.BM = function (s, f) {
  var start_time = new Date().getTime();
  f();
  var elapsed = new Date().getTime() - start_time;
  Vex.L(s + elapsed + 'ms');
};

// Get stack trace.
Vex.StackTrace = function () {
  var err = new Error();
  return err.stack;
};

// Dump warning to console.
Vex.W = function () {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  var line = args.join(' ');
  window.console.log('Warning: ', line, Vex.StackTrace());
};

// Used by various classes (e.g., SVGContext) to provide a
// unique prefix to element names (or other keys in shared namespaces).
Vex.Prefix = function (text) {
  return Vex.Prefix.prefix + text;
};
Vex.Prefix.prefix = 'vf-';

exports.Vex = Vex;

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Flow = undefined;

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

/* eslint-disable key-spacing */

var _vex = __webpack_require__(0);

var _fraction = __webpack_require__(8);

var _glyph = __webpack_require__(2);

var Flow = {
  STEM_WIDTH: 1.5,
  STEM_HEIGHT: 35,
  STAVE_LINE_THICKNESS: 1,
  RESOLUTION: 16384,
  DEFAULT_NOTATION_FONT_SCALE: 39,
  DEFAULT_TABLATURE_FONT_SCALE: 39,
  SLASH_NOTEHEAD_WIDTH: 15,

  // HACK:
  // Since text origins are positioned at the baseline, we must
  // compensate for the ascender of the text. Of course, 1 staff space is
  // a very poor approximation.
  //
  // This will be deprecated in the future. This is a temporary solution until
  // we have more robust text metrics.
  TEXT_HEIGHT_OFFSET_HACK: 1,

  /* Kerning (DEPRECATED) */
  IsKerned: true
};

Flow.clefProperties = function (clef) {
  if (!clef) throw new _vex.Vex.RERR('BadArgument', 'Invalid clef: ' + clef);

  var props = Flow.clefProperties.values[clef];
  if (!props) throw new _vex.Vex.RERR('BadArgument', 'Invalid clef: ' + clef);

  return props;
};

Flow.clefProperties.values = {
  'treble': { line_shift: 0 },
  'bass': { line_shift: 6 },
  'tenor': { line_shift: 4 },
  'alto': { line_shift: 3 },
  'soprano': { line_shift: 1 },
  'percussion': { line_shift: 0 },
  'mezzo-soprano': { line_shift: 2 },
  'baritone-c': { line_shift: 5 },
  'baritone-f': { line_shift: 5 },
  'subbass': { line_shift: 7 },
  'french': { line_shift: -1 }
};

/*
  Take a note in the format "Key/Octave" (e.g., "C/5") and return properties.

  The last argument, params, is a struct the currently can contain one option,
  octave_shift for clef ottavation (0 = default; 1 = 8va; -1 = 8vb, etc.).
*/
Flow.keyProperties = function (key, clef, params) {
  if (clef === undefined) {
    clef = 'treble';
  }

  var options = { octave_shift: 0 };

  if ((typeof params === 'undefined' ? 'undefined' : _typeof(params)) === 'object') {
    _vex.Vex.Merge(options, params);
  }

  var pieces = key.split('/');

  if (pieces.length < 2) {
    throw new _vex.Vex.RERR('BadArguments', 'Key must have note + octave and an optional glyph: ' + key);
  }

  var k = pieces[0].toUpperCase();
  var value = Flow.keyProperties.note_values[k];
  if (!value) throw new _vex.Vex.RERR('BadArguments', 'Invalid key name: ' + k);
  if (value.octave) pieces[1] = value.octave;

  var octave = parseInt(pieces[1], 10);

  // Octave_shift is the shift to compensate for clef 8va/8vb.
  octave += -1 * options.octave_shift;

  var base_index = octave * 7 - 4 * 7;
  var line = (base_index + value.index) / 2;
  line += Flow.clefProperties(clef).line_shift;

  var stroke = 0;

  if (line <= 0 && line * 2 % 2 === 0) stroke = 1; // stroke up
  if (line >= 6 && line * 2 % 2 === 0) stroke = -1; // stroke down

  // Integer value for note arithmetic.
  var int_value = typeof value.int_val !== 'undefined' ? octave * 12 + value.int_val : null;

  /* Check if the user specified a glyph. */
  var code = value.code;
  var shift_right = value.shift_right;
  if (pieces.length > 2 && pieces[2]) {
    var glyph_name = pieces[2].toUpperCase();
    var note_glyph = Flow.keyProperties.note_glyph[glyph_name];
    if (note_glyph) {
      code = note_glyph.code;
      shift_right = note_glyph.shift_right;
    }
  }

  return {
    key: k,
    octave: octave,
    line: line,
    int_value: int_value,
    accidental: value.accidental,
    code: code,
    stroke: stroke,
    shift_right: shift_right,
    displaced: false
  };
};

Flow.keyProperties.note_values = {
  'C': { index: 0, int_val: 0, accidental: null },
  'CN': { index: 0, int_val: 0, accidental: 'n' },
  'C#': { index: 0, int_val: 1, accidental: '#' },
  'C##': { index: 0, int_val: 2, accidental: '##' },
  'CB': { index: 0, int_val: -1, accidental: 'b' },
  'CBB': { index: 0, int_val: -2, accidental: 'bb' },
  'D': { index: 1, int_val: 2, accidental: null },
  'DN': { index: 1, int_val: 2, accidental: 'n' },
  'D#': { index: 1, int_val: 3, accidental: '#' },
  'D##': { index: 1, int_val: 4, accidental: '##' },
  'DB': { index: 1, int_val: 1, accidental: 'b' },
  'DBB': { index: 1, int_val: 0, accidental: 'bb' },
  'E': { index: 2, int_val: 4, accidental: null },
  'EN': { index: 2, int_val: 4, accidental: 'n' },
  'E#': { index: 2, int_val: 5, accidental: '#' },
  'E##': { index: 2, int_val: 6, accidental: '##' },
  'EB': { index: 2, int_val: 3, accidental: 'b' },
  'EBB': { index: 2, int_val: 2, accidental: 'bb' },
  'F': { index: 3, int_val: 5, accidental: null },
  'FN': { index: 3, int_val: 5, accidental: 'n' },
  'F#': { index: 3, int_val: 6, accidental: '#' },
  'F##': { index: 3, int_val: 7, accidental: '##' },
  'FB': { index: 3, int_val: 4, accidental: 'b' },
  'FBB': { index: 3, int_val: 3, accidental: 'bb' },
  'G': { index: 4, int_val: 7, accidental: null },
  'GN': { index: 4, int_val: 7, accidental: 'n' },
  'G#': { index: 4, int_val: 8, accidental: '#' },
  'G##': { index: 4, int_val: 9, accidental: '##' },
  'GB': { index: 4, int_val: 6, accidental: 'b' },
  'GBB': { index: 4, int_val: 5, accidental: 'bb' },
  'A': { index: 5, int_val: 9, accidental: null },
  'AN': { index: 5, int_val: 9, accidental: 'n' },
  'A#': { index: 5, int_val: 10, accidental: '#' },
  'A##': { index: 5, int_val: 11, accidental: '##' },
  'AB': { index: 5, int_val: 8, accidental: 'b' },
  'ABB': { index: 5, int_val: 7, accidental: 'bb' },
  'B': { index: 6, int_val: 11, accidental: null },
  'BN': { index: 6, int_val: 11, accidental: 'n' },
  'B#': { index: 6, int_val: 12, accidental: '#' },
  'B##': { index: 6, int_val: 13, accidental: '##' },
  'BB': { index: 6, int_val: 10, accidental: 'b' },
  'BBB': { index: 6, int_val: 9, accidental: 'bb' },
  'R': { index: 6, int_val: 9, rest: true }, // Rest
  'X': {
    index: 6,
    accidental: '',
    octave: 4,
    code: 'v3e',
    shift_right: 5.5
  }
};

Flow.keyProperties.note_glyph = {
  /* Diamond */
  'D0': { code: 'v27', shift_right: -0.5 },
  'D1': { code: 'v2d', shift_right: -0.5 },
  'D2': { code: 'v22', shift_right: -0.5 },
  'D3': { code: 'v70', shift_right: -0.5 },

  /* Triangle */
  'T0': { code: 'v49', shift_right: -2 },
  'T1': { code: 'v93', shift_right: 0.5 },
  'T2': { code: 'v40', shift_right: 0.5 },
  'T3': { code: 'v7d', shift_right: 0.5 },

  /* Cross */
  'X0': { code: 'v92', shift_right: -2 },
  'X1': { code: 'v95', shift_right: -0.5 },
  'X2': { code: 'v7f', shift_right: 0.5 },
  'X3': { code: 'v3b', shift_right: -2 }
};

Flow.integerToNote = function (integer) {
  if (typeof integer === 'undefined') {
    throw new _vex.Vex.RERR('BadArguments', 'Undefined integer for integerToNote');
  }

  if (integer < -2) {
    throw new _vex.Vex.RERR('BadArguments', 'integerToNote requires integer > -2: ' + integer);
  }

  var noteValue = Flow.integerToNote.table[integer];
  if (!noteValue) {
    throw new _vex.Vex.RERR('BadArguments', 'Unknown note value for integer: ' + integer);
  }

  return noteValue;
};

Flow.integerToNote.table = {
  0: 'C',
  1: 'C#',
  2: 'D',
  3: 'D#',
  4: 'E',
  5: 'F',
  6: 'F#',
  7: 'G',
  8: 'G#',
  9: 'A',
  10: 'A#',
  11: 'B'
};

Flow.tabToGlyph = function (fret) {
  var scale = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 1.0;

  var glyph = null;
  var width = 0;
  var shift_y = 0;

  if (fret.toString().toUpperCase() === 'X') {
    var glyphMetrics = new _glyph.Glyph('v7f', Flow.DEFAULT_TABLATURE_FONT_SCALE).getMetrics();
    glyph = 'v7f';
    width = glyphMetrics.width;
    shift_y = -glyphMetrics.height / 2;
  } else {
    width = Flow.textWidth(fret.toString());
  }

  return {
    text: fret,
    code: glyph,
    getWidth: function getWidth() {
      return width * scale;
    },
    shift_y: shift_y
  };
};

Flow.textWidth = function (text) {
  return 7 * text.toString().length;
};

Flow.articulationCodes = function (artic) {
  return Flow.articulationCodes.articulations[artic];
};

Flow.articulationCodes.articulations = {
  'a.': { code: 'v23', between_lines: true }, // Staccato
  'av': { code: 'v28', between_lines: true }, // Staccatissimo
  'a>': { code: 'v42', between_lines: true }, // Accent
  'a-': { code: 'v25', between_lines: true }, // Tenuto
  'a^': { code: 'va', between_lines: false }, // Marcato
  'a+': { code: 'v8b', between_lines: false }, // Left hand pizzicato
  'ao': { code: 'v94', between_lines: false }, // Snap pizzicato
  'ah': { code: 'vb9', between_lines: false }, // Natural harmonic or open note
  'a@a': { code: 'v43', between_lines: false }, // Fermata above staff
  'a@u': { code: 'v5b', between_lines: false }, // Fermata below staff
  'a|': { code: 'v75', between_lines: false }, // Bow up - up stroke
  'am': { code: 'v97', between_lines: false }, // Bow down - down stroke
  'a,': { code: 'vb3', between_lines: false } // Choked
};

Flow.accidentalCodes = function (acc) {
  return Flow.accidentalCodes.accidentals[acc];
};

Flow.accidentalCodes.accidentals = {
  '#': { code: 'v18', parenRightPaddingAdjustment: -1 },
  '##': { code: 'v7f', parenRightPaddingAdjustment: -1 },
  'b': { code: 'v44', parenRightPaddingAdjustment: -2 },
  'bb': { code: 'v26', parenRightPaddingAdjustment: -2 },
  'n': { code: 'v4e', parenRightPaddingAdjustment: -1 },
  '{': { code: 'v9c', parenRightPaddingAdjustment: -1 },
  '}': { code: 'v84', parenRightPaddingAdjustment: -1 },
  'db': { code: 'v9e', parenRightPaddingAdjustment: -1 },
  'd': { code: 'vab', parenRightPaddingAdjustment: 0 },
  'bbs': { code: 'v90', parenRightPaddingAdjustment: -1 },
  '++': { code: 'v51', parenRightPaddingAdjustment: -1 },
  '+': { code: 'v78', parenRightPaddingAdjustment: -1 },
  '+-': { code: 'v8d', parenRightPaddingAdjustment: -1 },
  '++-': { code: 'v7a', parenRightPaddingAdjustment: -1 },
  'bs': { code: 'vb7', parenRightPaddingAdjustment: -1 },
  'bss': { code: 'v39', parenRightPaddingAdjustment: -1 },
  'o': { code: 'vd0', parenRightPaddingAdjustment: -1 },
  'k': { code: 'vd1', parenRightPaddingAdjustment: -1 }
};

Flow.accidentalColumnsTable = {
  1: {
    a: [1],
    b: [1]
  },
  2: {
    a: [1, 2]
  },
  3: {
    a: [1, 3, 2],
    b: [1, 2, 1],
    second_on_bottom: [1, 2, 3]
  },
  4: {
    a: [1, 3, 4, 2],
    b: [1, 2, 3, 1],
    spaced_out_tetrachord: [1, 2, 1, 2]
  },
  5: {
    a: [1, 3, 5, 4, 2],
    b: [1, 2, 4, 3, 1],
    spaced_out_pentachord: [1, 2, 3, 2, 1],
    very_spaced_out_pentachord: [1, 2, 1, 2, 1] },
  6: {
    a: [1, 3, 5, 6, 4, 2],
    b: [1, 2, 4, 5, 3, 1],
    spaced_out_hexachord: [1, 3, 2, 1, 3, 2],
    very_spaced_out_hexachord: [1, 2, 1, 2, 1, 2]
  }
};

Flow.ornamentCodes = function (acc) {
  return Flow.ornamentCodes.ornaments[acc];
};

Flow.ornamentCodes.ornaments = {
  'mordent': { code: 'v1e' },
  'mordent_inverted': { code: 'v45' },
  'turn': { code: 'v72' },
  'turn_inverted': { code: 'v33' },
  'tr': { code: 'v1f' },
  'upprall': { code: 'v60' },
  'downprall': { code: 'vb4' },
  'prallup': { code: 'v6d' },
  'pralldown': { code: 'v2c' },
  'upmordent': { code: 'v29' },
  'downmordent': { code: 'v68' },
  'lineprall': { code: 'v20' },
  'prallprall': { code: 'v86' }
};

Flow.keySignature = function (spec) {
  var keySpec = Flow.keySignature.keySpecs[spec];

  if (!keySpec) {
    throw new _vex.Vex.RERR('BadKeySignature', 'Bad key signature spec: \'' + spec + '\'');
  }

  if (!keySpec.acc) {
    return [];
  }

  var notes = Flow.keySignature.accidentalList(keySpec.acc);

  var acc_list = [];
  for (var i = 0; i < keySpec.num; ++i) {
    var line = notes[i];
    acc_list.push({ type: keySpec.acc, line: line });
  }

  return acc_list;
};

Flow.keySignature.keySpecs = {
  'C': { acc: null, num: 0 },
  'Am': { acc: null, num: 0 },
  'F': { acc: 'b', num: 1 },
  'Dm': { acc: 'b', num: 1 },
  'Bb': { acc: 'b', num: 2 },
  'Gm': { acc: 'b', num: 2 },
  'Eb': { acc: 'b', num: 3 },
  'Cm': { acc: 'b', num: 3 },
  'Ab': { acc: 'b', num: 4 },
  'Fm': { acc: 'b', num: 4 },
  'Db': { acc: 'b', num: 5 },
  'Bbm': { acc: 'b', num: 5 },
  'Gb': { acc: 'b', num: 6 },
  'Ebm': { acc: 'b', num: 6 },
  'Cb': { acc: 'b', num: 7 },
  'Abm': { acc: 'b', num: 7 },
  'G': { acc: '#', num: 1 },
  'Em': { acc: '#', num: 1 },
  'D': { acc: '#', num: 2 },
  'Bm': { acc: '#', num: 2 },
  'A': { acc: '#', num: 3 },
  'F#m': { acc: '#', num: 3 },
  'E': { acc: '#', num: 4 },
  'C#m': { acc: '#', num: 4 },
  'B': { acc: '#', num: 5 },
  'G#m': { acc: '#', num: 5 },
  'F#': { acc: '#', num: 6 },
  'D#m': { acc: '#', num: 6 },
  'C#': { acc: '#', num: 7 },
  'A#m': { acc: '#', num: 7 }
};

Flow.unicode = {
  // Unicode accidentals
  'sharp': String.fromCharCode(parseInt('266F', 16)),
  'flat': String.fromCharCode(parseInt('266D', 16)),
  'natural': String.fromCharCode(parseInt('266E', 16)),
  // Major Chord
  'triangle': String.fromCharCode(parseInt('25B3', 16)),
  // half-diminished
  'o-with-slash': String.fromCharCode(parseInt('00F8', 16)),
  // Diminished
  'degrees': String.fromCharCode(parseInt('00B0', 16)),
  'circle': String.fromCharCode(parseInt('25CB', 16))
};

Flow.keySignature.accidentalList = function (acc) {
  var patterns = {
    'b': [2, 0.5, 2.5, 1, 3, 1.5, 3.5],
    '#': [0, 1.5, -0.5, 1, 2.5, 0.5, 2]
  };

  return patterns[acc];
};

Flow.parseNoteDurationString = function (durationString) {
  if (typeof durationString !== 'string') {
    return null;
  }

  var regexp = /(\d*\/?\d+|[a-z])(d*)([nrhms]|$)/;

  var result = regexp.exec(durationString);
  if (!result) {
    return null;
  }

  var duration = result[1];
  var dots = result[2].length;
  var type = result[3];

  if (type.length === 0) {
    type = 'n';
  }

  return {
    duration: duration,
    dots: dots,
    type: type
  };
};

Flow.parseNoteData = function (noteData) {
  var duration = noteData.duration;

  // Preserve backwards-compatibility
  var durationStringData = Flow.parseNoteDurationString(duration);
  if (!durationStringData) {
    return null;
  }

  var ticks = Flow.durationToTicks(durationStringData.duration);
  if (ticks == null) {
    return null;
  }

  var type = noteData.type;

  if (type) {
    if (!(type === 'n' || type === 'r' || type === 'h' || type === 'm' || type === 's')) {
      return null;
    }
  } else {
    type = durationStringData.type;
    if (!type) {
      type = 'n';
    }
  }

  var dots = noteData.dots ? noteData.dots : durationStringData.dots;

  if (typeof dots !== 'number') {
    return null;
  }

  var currentTicks = ticks;

  for (var i = 0; i < dots; i++) {
    if (currentTicks <= 1) return null;

    currentTicks = currentTicks / 2;
    ticks += currentTicks;
  }

  return {
    duration: durationStringData.duration,
    type: type,
    dots: dots,
    ticks: ticks
  };
};

// Used to convert duration aliases to the number based duration.
// If the input isn't an alias, simply return the input.
//
// example: 'q' -> '4', '8' -> '8'
Flow.sanitizeDuration = function (duration) {
  var alias = Flow.durationAliases[duration];
  if (alias !== undefined) {
    duration = alias;
  }

  if (Flow.durationToTicks.durations[duration] === undefined) {
    throw new _vex.Vex.RERR('BadArguments', 'The provided duration is not valid: ' + duration);
  }

  return duration;
};

// Convert the `duration` to an fraction
Flow.durationToFraction = function (duration) {
  return new _fraction.Fraction().parse(Flow.sanitizeDuration(duration));
};

// Convert the `duration` to an number
Flow.durationToNumber = function (duration) {
  return Flow.durationToFraction(duration).value();
};

// Convert the `duration` to total ticks
Flow.durationToTicks = function (duration) {
  duration = Flow.sanitizeDuration(duration);

  var ticks = Flow.durationToTicks.durations[duration];
  if (ticks === undefined) {
    return null;
  }

  return ticks;
};

Flow.durationToTicks.durations = {
  '1/2': Flow.RESOLUTION * 2,
  '1': Flow.RESOLUTION / 1,
  '2': Flow.RESOLUTION / 2,
  '4': Flow.RESOLUTION / 4,
  '8': Flow.RESOLUTION / 8,
  '16': Flow.RESOLUTION / 16,
  '32': Flow.RESOLUTION / 32,
  '64': Flow.RESOLUTION / 64,
  '128': Flow.RESOLUTION / 128,
  '256': Flow.RESOLUTION / 256
};

Flow.durationAliases = {
  'w': '1',
  'h': '2',
  'q': '4',

  // This is the default duration used to render bars (BarNote). Bars no longer
  // consume ticks, so this should be a no-op.
  //
  // TODO(0xfe): This needs to be cleaned up.
  'b': '256'
};

Flow.durationToGlyph = function (duration, type) {
  duration = Flow.sanitizeDuration(duration);

  var code = Flow.durationToGlyph.duration_codes[duration];
  if (code === undefined) {
    return null;
  }

  if (!type) {
    type = 'n';
  }

  var glyphTypeProperties = code.type[type];
  if (glyphTypeProperties === undefined) {
    return null;
  }

  return _vex.Vex.Merge(_vex.Vex.Merge({}, code.common), glyphTypeProperties);
};

Flow.durationToGlyph.duration_codes = {
  '1/2': {
    common: {
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'v53', scale).getMetrics().width;
      },

      stem: false,
      stem_offset: 0,
      flag: false,
      stem_up_extension: -Flow.STEM_HEIGHT,
      stem_down_extension: -Flow.STEM_HEIGHT,
      gracenote_stem_up_extension: -Flow.STEM_HEIGHT,
      gracenote_stem_down_extension: -Flow.STEM_HEIGHT,
      tabnote_stem_up_extension: -Flow.STEM_HEIGHT,
      tabnote_stem_down_extension: -Flow.STEM_HEIGHT,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Breve note
        code_head: 'v53'
      },
      'h': { // Breve note harmonic
        code_head: 'v59'
      },
      'm': { // Breve note muted -
        code_head: 'vf',
        stem_offset: 0
      },
      'r': { // Breve rest
        code_head: 'v31',
        rest: true,
        position: 'B/5',
        dot_shiftY: 0.5
      },
      's': { // Breve note slash -
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '1': {
    common: {
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'v1d', scale).getMetrics().width;
      },

      stem: false,
      stem_offset: 0,
      flag: false,
      stem_up_extension: -Flow.STEM_HEIGHT,
      stem_down_extension: -Flow.STEM_HEIGHT,
      gracenote_stem_up_extension: -Flow.STEM_HEIGHT,
      gracenote_stem_down_extension: -Flow.STEM_HEIGHT,
      tabnote_stem_up_extension: -Flow.STEM_HEIGHT,
      tabnote_stem_down_extension: -Flow.STEM_HEIGHT,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Whole note
        code_head: 'v1d'
      },
      'h': { // Whole note harmonic
        code_head: 'v46'
      },
      'm': { // Whole note muted
        code_head: 'v92',
        stem_offset: -3
      },
      'r': { // Whole rest
        code_head: 'v5c',
        rest: true,
        position: 'D/5',
        dot_shiftY: 0.5
      },
      's': { // Whole note slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '2': {
    common: {
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'v81', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: false,
      stem_up_extension: 0,
      stem_down_extension: 0,
      gracenote_stem_up_extension: -14,
      gracenote_stem_down_extension: -14,
      tabnote_stem_up_extension: 0,
      tabnote_stem_down_extension: 0,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Half note
        code_head: 'v81'
      },
      'h': { // Half note harmonic
        code_head: 'v2d'
      },
      'm': { // Half note muted
        code_head: 'v95',
        stem_offset: -3
      },
      'r': { // Half rest
        code_head: 'vc',
        stem: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: -0.5
      },
      's': { // Half note slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '4': {
    common: {
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'vb', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: false,
      stem_up_extension: 0,
      stem_down_extension: 0,
      gracenote_stem_up_extension: -14,
      gracenote_stem_down_extension: -14,
      tabnote_stem_up_extension: 0,
      tabnote_stem_down_extension: 0,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Quarter note
        code_head: 'vb'
      },
      'h': { // Quarter harmonic
        code_head: 'v22'
      },
      'm': { // Quarter muted
        code_head: 'v3e',
        stem_offset: -3
      },
      'r': { // Quarter rest
        code_head: 'v7c',
        stem: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: -0.5,
        line_above: 1.5,
        line_below: 1.5
      },
      's': { // Quarter slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '8': {
    common: {
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'vb', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: true,
      beam_count: 1,
      code_flag_upstem: 'v54',
      code_flag_downstem: 'v9a',
      stem_up_extension: 0,
      stem_down_extension: 0,
      gracenote_stem_up_extension: -14,
      gracenote_stem_down_extension: -14,
      tabnote_stem_up_extension: 0,
      tabnote_stem_down_extension: 0,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Eighth note
        code_head: 'vb'
      },
      'h': { // Eighth note harmonic
        code_head: 'v22'
      },
      'm': { // Eighth note muted
        code_head: 'v3e'
      },
      'r': { // Eighth rest
        code_head: 'va5',
        stem: false,
        flag: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: -0.5,
        line_above: 1.0,
        line_below: 1.0
      },
      's': { // Eight slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '16': {
    common: {
      beam_count: 2,
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'vb', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: true,
      code_flag_upstem: 'v3f',
      code_flag_downstem: 'v8f',
      stem_up_extension: 0,
      stem_down_extension: 0,
      gracenote_stem_up_extension: -14,
      gracenote_stem_down_extension: -14,
      tabnote_stem_up_extension: 0,
      tabnote_stem_down_extension: 0,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Sixteenth note
        code_head: 'vb'
      },
      'h': { // Sixteenth note harmonic
        code_head: 'v22'
      },
      'm': { // Sixteenth note muted
        code_head: 'v3e'
      },
      'r': { // Sixteenth rest
        code_head: 'v3c',
        stem: false,
        flag: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: -0.5,
        line_above: 1.0,
        line_below: 2.0
      },
      's': { // Sixteenth slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '32': {
    common: {
      beam_count: 3,
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'vb', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: true,
      code_flag_upstem: 'v47',
      code_flag_downstem: 'v2a',
      stem_up_extension: 9,
      stem_down_extension: 9,
      gracenote_stem_up_extension: -12,
      gracenote_stem_down_extension: -12,
      tabnote_stem_up_extension: 8,
      tabnote_stem_down_extension: 5,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Thirty-second note
        code_head: 'vb'
      },
      'h': { // Thirty-second harmonic
        code_head: 'v22'
      },
      'm': { // Thirty-second muted
        code_head: 'v3e'
      },
      'r': { // Thirty-second rest
        code_head: 'v55',
        stem: false,
        flag: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: -1.5,
        line_above: 2.0,
        line_below: 2.0
      },
      's': { // Thirty-second slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '64': {
    common: {
      beam_count: 4,
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'vb', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: true,
      code_flag_upstem: 'va9',
      code_flag_downstem: 'v58',
      stem_up_extension: 13,
      stem_down_extension: 13,
      gracenote_stem_up_extension: -10,
      gracenote_stem_down_extension: -10,
      tabnote_stem_up_extension: 12,
      tabnote_stem_down_extension: 9,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Sixty-fourth note
        code_head: 'vb'
      },
      'h': { // Sixty-fourth harmonic
        code_head: 'v22'
      },
      'm': { // Sixty-fourth muted
        code_head: 'v3e'
      },
      'r': { // Sixty-fourth rest
        code_head: 'v38',
        stem: false,
        flag: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: -1.5,
        line_above: 2.0,
        line_below: 3.0
      },
      's': { // Sixty-fourth slash
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  },
  '128': {
    common: {
      beam_count: 5,
      getWidth: function getWidth() {
        var scale = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : Flow.DEFAULT_NOTATION_FONT_SCALE;

        return new _glyph.Glyph(this.code_head || 'vb', scale).getMetrics().width;
      },

      stem: true,
      stem_offset: 0,
      flag: true,
      code_flag_upstem: 'v9b',
      code_flag_downstem: 'v30',
      stem_up_extension: 22,
      stem_down_extension: 22,
      gracenote_stem_up_extension: -8,
      gracenote_stem_down_extension: -8,
      tabnote_stem_up_extension: 21,
      tabnote_stem_down_extension: 18,
      dot_shiftY: 0,
      line_above: 0,
      line_below: 0
    },
    type: {
      'n': { // Hundred-twenty-eight note
        code_head: 'vb'
      },
      'h': { // Hundred-twenty-eight harmonic
        code_head: 'v22'
      },
      'm': { // Hundred-twenty-eight muted
        code_head: 'v3e'
      },
      'r': { // Hundred-twenty-eight rest
        code_head: 'vaa',
        stem: false,
        flag: false,
        rest: true,
        position: 'B/4',
        dot_shiftY: 1.5,
        line_above: 3.0,
        line_below: 3.0
      },
      's': { // Hundred-twenty-eight rest
        // Drawn with canvas primitives
        getWidth: function getWidth() {
          return Flow.SLASH_NOTEHEAD_WIDTH;
        },
        position: 'B/4'
      }
    }
  }
};

// For future collaboration with the SMuFL Standard Music Font Layout

Flow.smufl = {};

// add references between smufl glyph names and code points.
Flow.smufl.to_code_points = {
  // staff brackets and dividers (e000-e00f)
  bracketTop: 'v1b',
  bracketBottom: 'v10',

  // barlines (e030-e03f)
  barlineTick: 'v6f',

  // repeats (e040-e04f)
  segno: 'v8c',
  coda: 'v4d',

  // clefs (e050-e07f)
  gClef: 'v83',
  cClef: 'vad',
  fClef: 'v79',
  unpitchedPercussionClef1: 'v59', // same as breveNoteheadHarmonic
  '6stringTabClef': 'v2f',

  // time signatures (e080-e09f)
  timeSig0: 'v0',
  timeSig1: 'v1',
  timeSig2: 'v2',
  timeSig3: 'v3',
  timeSig4: 'v4',
  timeSig5: 'v5',
  timeSig6: 'v6',
  timeSig7: 'v7',
  timeSig8: 'v8',
  timeSig9: 'v9',
  timeSigCommon: 'v41',
  timeSigCutCommon: 'vb6',

  // notehead (e0a0-e0ff)
  noteheadDoubleWhole: 'v53',
  noteheadWhole: 'v1d',
  noteheadHalf: 'v81',
  noteheadBlack: 'vb',
  noteheadXWhole: 'v92',
  noteheadXHalf: 'v95',
  noteheadXBlack: 'v3e',
  noteheadCircleX: 'v3b',
  noteheadTriangleUpWhole: 'v49',
  noteheadTriangleUpHalf: 'v93',
  noteheadTriangleUpBlack: 'v40',
  noteheadDiamondWhole: 'v46',
  noteheadDiamondHalf: 'v2d',
  noteheadDiamondBlack: 'v22',

  // individual notes (e1d0-e1ef)
  augmentationDot: 'v23',

  // temolos (e220-e23f)
  tremolo1: 'v74',

  // flags (e240-e25f)
  flag8thUp: 'v54',
  flag8thDown: 'v9a',
  flag16thUp: 'v3f',
  flag16thDown: 'v8f',
  flag32ndUp: 'v47',
  flag32ndDown: 'v2a',
  flag64thUp: 'va9',
  flag64thDown: 'v58',
  flag128thUp: 'v9b',
  flag128thDown: 'v30',

  // standard accidentals (e260-e26f)
  accidentalFlat: 'v44',
  accidentalNatural: 'v4e',
  accidentalSharp: 'v18',
  accidentalDoubleSharp: 'v7f',
  accidentalDoubleFlat: 'v26',
  accidentalParensLeft: 'v9c',
  accidentalParensRight: 'v84',

  // stein-zimmermann accidentals (24-edo) (e280-e28f)
  accidentalQuarterToneFlatStein: 'vab',
  accidentalThreeQuarterTonesFlatZimmermann: 'v9e',
  accidentalQuarterToneSharpStein: 'v78',
  accidentalThreeQuarterTonesSharpStein: 'v51',

  // arel-ezgi-uzdilek accidentals (e440-e44f)
  accidentalBuyukMucennebFlat: 'v39',
  accidentalBakiyeFlat: 'vb7',
  accidentalKomaSharp: 'v51', // same as accidentalQuarterToneSharpStein
  accidentalKucukMucennebSharp: 'v8d',

  // persian accidentals (e460-e46f)
  accidentalKoron: 'vd1',
  accidentalSori: 'vd0',

  // articulation (e4a0-e4bf)
  articAccentAbove: 'v42',
  articAccentBelow: 'v42', // same as above
  articTenutoAbove: 'v25',
  articTenutoBelow: 'v25', // same as above
  articStaccatoAbove: 'v23', // = dot
  articStaccatoBelow: 'v23', // = dot
  articStaccatissimoAbove: 'v28',
  articMarcatoAbove: 'va',

  // holds and pauses (e4c0-e4df)
  fermataAbove: 'v43',
  fermataBelow: 'v5b',
  breathMarkComma: 'v6c',
  breathMarkUpbow: 'v8a', // looks better than current upbow
  caesura: 'v34',
  caesuraCurved: 'v4b',

  // rests (e4e0-e4ff)
  restMaxima: 'v59', // not designed for this, but should do the trick
  // need restLonga -- used in multimeasure rests, like above
  restDoubleWhole: 'v31',
  restWhole: 'v5c',
  restHalf: 'vc',
  restQuarter: 'v7c',
  rest8th: 'va5',
  rest16th: 'v3c',
  rest32nd: 'v55',
  rest64th: 'v38',
  rest128th: 'vaa',

  // dynamics (e520-e54f)
  dynamicPiano: 'vbf',
  dynamicMezzo: 'v62',
  dynamicForte: 'vba',
  dynamicRinforzando: 'vba',
  dynamicSforzando: 'v4a',
  dynamicZ: 'v80',

  // common ornaments (e560-e56f)
  ornamentTrill: 'v1f',
  ornamentTurn: 'v72',
  ornamentTurnSlash: 'v33',
  ornamentMordent: 'v45',
  ornamentMordentInverted: 'v1e',
  ornamentTremblement: 'v86',

  // precomposed trills and mordents (e5b0-e5cf)
  ornamentPrecompAppoggTrill: 'v20',
  ornamentPrecompSlideTrillDAnglebert: 'v60',
  ornamentPrecompSlideTrillBach: 'v29',
  ornamentPrecompTrillSuffixDandrieu: 'v6d',
  ornamentPrecompDoubleCadenceUpperPrefix: 'vb4',
  ornamentPrecompDoubleCadenceUpperPrefixTurn: 'v68',
  ornamentPrecompTrillLowerSuffix: 'v2c',

  // string techniques (e610-e62f)
  stringsDownBow: 'v94',
  stringsUpBow: 'v75',
  stringsHarmonic: 'vb9',

  // plucked techniques (e630-e63f)
  pluckedSnapPizzicatoAbove: 'v94',
  pluckedLeftHandPizzicato: 'v8b', // plus sign

  // keyboard techniques (e650-e67f)
  keyboardPedalPed: 'v36',
  keyboardPedalUp: 'v5d',

  // percussion playing technique pictograms (e7f0-e80f)
  pictChokeCymbal: 'vb3',

  // multi-segment lines (eaa0-eb0f)
  wiggleArpeggiatoUp: 'va3', // rotated 90deg from reference implementation

  // arrows and arrowheads (eb60-eb8f)
  arrowheadBlackUp: 'vc3',
  arrowheadBlackDown: 'v52'

  // not found:
  // noteheadDiamondWhole: 'v27', stylistic alternate to v46?
  // noteheadDiamondBlack: 'v70', stylistic alternate to v22?
  // noteheadTriangleUpBlack: 'v7d', stylistic alternate to v40?
  // accidentalSlashedDoubleFlat: 'v90',
  // accidentalOneAndAHalfSharpTurned: 'v7a',
  // unused marcato alternative?  'v5a',
  // arpeggioBrushDown: 'v11',
};

// Some defaults
Flow.TIME4_4 = {
  num_beats: 4,
  beat_value: 4,
  resolution: Flow.RESOLUTION
};
exports.Flow = Flow;

/***/ }),
/* 2 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Glyph = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _boundingboxcomputation = __webpack_require__(65);

var _boundingbox = __webpack_require__(10);

var _vexflow_font = __webpack_require__(40);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

function processOutline(outline, originX, originY, scaleX, scaleY, outlineFns) {
  var command = void 0;
  var x = void 0;
  var y = void 0;
  var i = 0;

  function nextX() {
    return originX + outline[i++] * scaleX;
  }
  function nextY() {
    return originY + outline[i++] * scaleY;
  }

  while (i < outline.length) {
    command = outline[i++];
    switch (command) {
      case 'm':
      case 'l':
        outlineFns[command](nextX(), nextY());
        break;
      case 'q':
        x = nextX();
        y = nextY();
        outlineFns.q(nextX(), nextY(), x, y);
        break;
      case 'b':
        x = nextX();
        y = nextY();
        outlineFns.b(nextX(), nextY(), nextX(), nextY(), x, y);
        break;
      default:
        break;
    }
  }
}

var Glyph = exports.Glyph = function (_Element) {
  _inherits(Glyph, _Element);

  _createClass(Glyph, null, [{
    key: 'loadMetrics',

    /* Static methods used to implement loading / unloading of glyphs */
    value: function loadMetrics(font, code, cache) {
      var glyph = font.glyphs[code];
      if (!glyph) {
        throw new _vex.Vex.RERR('BadGlyph', 'Glyph ' + code + ' does not exist in font.');
      }

      var x_min = glyph.x_min;
      var x_max = glyph.x_max;
      var ha = glyph.ha;

      var outline = void 0;

      if (glyph.o) {
        if (cache) {
          if (glyph.cached_outline) {
            outline = glyph.cached_outline;
          } else {
            outline = glyph.o.split(' ');
            glyph.cached_outline = outline;
          }
        } else {
          if (glyph.cached_outline) delete glyph.cached_outline;
          outline = glyph.o.split(' ');
        }

        return {
          x_min: x_min,
          x_max: x_max,
          ha: ha,
          outline: outline
        };
      } else {
        throw new _vex.Vex.RERR('BadGlyph', 'Glyph ' + code + ' has no outline defined.');
      }
    }

    /**
     * A quick and dirty static glyph renderer. Renders glyphs from the default
     * font defined in Vex.Flow.Font.
     *
     * @param {!Object} ctx The canvas context.
     * @param {number} x_pos X coordinate.
     * @param {number} y_pos Y coordinate.
     * @param {number} point The point size to use.
     * @param {string} val The glyph code in Vex.Flow.Font.
     * @param {boolean} nocache If set, disables caching of font outline.
     */

  }, {
    key: 'renderGlyph',
    value: function renderGlyph(ctx, x_pos, y_pos, point, val, nocache) {
      var scale = point * 72.0 / (_vexflow_font.Font.resolution * 100.0);
      var metrics = Glyph.loadMetrics(_vexflow_font.Font, val, !nocache);
      Glyph.renderOutline(ctx, metrics.outline, scale, x_pos, y_pos);
    }
  }, {
    key: 'renderOutline',
    value: function renderOutline(ctx, outline, scale, x_pos, y_pos) {
      ctx.beginPath();
      ctx.moveTo(x_pos, y_pos);
      processOutline(outline, x_pos, y_pos, scale, -scale, {
        m: ctx.moveTo.bind(ctx),
        l: ctx.lineTo.bind(ctx),
        q: ctx.quadraticCurveTo.bind(ctx),
        b: ctx.bezierCurveTo.bind(ctx)
      });
      ctx.fill();
    }
  }, {
    key: 'getOutlineBoundingBox',
    value: function getOutlineBoundingBox(outline, scale, x_pos, y_pos) {
      var bboxComp = new _boundingboxcomputation.BoundingBoxComputation();

      processOutline(outline, x_pos, y_pos, scale, -scale, {
        m: bboxComp.addPoint.bind(bboxComp),
        l: bboxComp.addPoint.bind(bboxComp),
        q: bboxComp.addQuadraticCurve.bind(bboxComp),
        b: bboxComp.addBezierCurve.bind(bboxComp)
      });

      return new _boundingbox.BoundingBox(bboxComp.x1, bboxComp.y1, bboxComp.width(), bboxComp.height());
    }

    /**
     * @constructor
     */

  }]);

  function Glyph(code, point, options) {
    _classCallCheck(this, Glyph);

    var _this = _possibleConstructorReturn(this, (Glyph.__proto__ || Object.getPrototypeOf(Glyph)).call(this));

    _this.setAttribute('type', 'Glyph');

    _this.code = code;
    _this.point = point;
    _this.options = {
      cache: true,
      font: _vexflow_font.Font
    };

    _this.metrics = null;
    _this.x_shift = 0;
    _this.y_shift = 0;

    _this.originShift = {
      x: 0,
      y: 0
    };

    if (options) {
      _this.setOptions(options);
    } else {
      _this.reset();
    }
    return _this;
  }

  _createClass(Glyph, [{
    key: 'setOptions',
    value: function setOptions(options) {
      _vex.Vex.Merge(this.options, options);
      this.reset();
    }
  }, {
    key: 'setPoint',
    value: function setPoint(point) {
      this.point = point;return this;
    }
  }, {
    key: 'setStave',
    value: function setStave(stave) {
      this.stave = stave;return this;
    }
  }, {
    key: 'setXShift',
    value: function setXShift(x_shift) {
      this.x_shift = x_shift;return this;
    }
  }, {
    key: 'setYShift',
    value: function setYShift(y_shift) {
      this.y_shift = y_shift;return this;
    }
  }, {
    key: 'reset',
    value: function reset() {
      this.scale = this.point * 72 / (this.options.font.resolution * 100);
      this.metrics = Glyph.loadMetrics(this.options.font, this.code, this.options.cache);
      this.bbox = Glyph.getOutlineBoundingBox(this.metrics.outline, this.scale, 0, 0);
    }
  }, {
    key: 'getMetrics',
    value: function getMetrics() {
      if (!this.metrics) {
        throw new _vex.Vex.RuntimeError('BadGlyph', 'Glyph ' + this.code + ' is not initialized.');
      }

      return {
        x_min: this.metrics.x_min * this.scale,
        x_max: this.metrics.x_max * this.scale,
        width: this.bbox.getW(),
        height: this.bbox.getH()
      };
    }
  }, {
    key: 'setOriginX',
    value: function setOriginX(x) {
      var bbox = this.bbox;

      var originX = Math.abs(bbox.getX() / bbox.getW());
      var xShift = (x - originX) * bbox.getW();
      this.originShift.x = -xShift;
    }
  }, {
    key: 'setOriginY',
    value: function setOriginY(y) {
      var bbox = this.bbox;

      var originY = Math.abs(bbox.getY() / bbox.getH());
      var yShift = (y - originY) * bbox.getH();
      this.originShift.y = -yShift;
    }
  }, {
    key: 'setOrigin',
    value: function setOrigin(x, y) {
      this.setOriginX(x);
      this.setOriginY(y);
    }
  }, {
    key: 'render',
    value: function render(ctx, x, y) {
      if (!this.metrics) {
        throw new _vex.Vex.RuntimeError('BadGlyph', 'Glyph ' + this.code + ' is not initialized.');
      }

      var outline = this.metrics.outline;
      var scale = this.scale;

      this.setRendered();
      this.applyStyle(ctx);
      Glyph.renderOutline(ctx, outline, scale, x + this.originShift.x, y + this.originShift.y);
      this.restoreStyle(ctx);
    }
  }, {
    key: 'renderToStave',
    value: function renderToStave(x) {
      this.checkContext();

      if (!this.metrics) {
        throw new _vex.Vex.RuntimeError('BadGlyph', 'Glyph ' + this.code + ' is not initialized.');
      }

      if (!this.stave) {
        throw new _vex.Vex.RuntimeError('GlyphError', 'No valid stave');
      }

      var outline = this.metrics.outline;
      var scale = this.scale;

      this.setRendered();
      this.applyStyle();
      Glyph.renderOutline(this.context, outline, scale, x + this.x_shift, this.stave.getYForGlyphs() + this.y_shift);
      this.restoreStyle();
    }
  }]);

  return Glyph;
}(_element.Element);

/***/ }),
/* 3 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Element = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// @author Mohit Cheppudira
//
// ## Description
//
// This file implements a generic base class for VexFlow, with implementations
// of general functions and properties that can be inherited by all VexFlow elements.

var _vex = __webpack_require__(0);

var _registry = __webpack_require__(39);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Element = exports.Element = function () {
  _createClass(Element, null, [{
    key: 'newID',
    value: function newID() {
      return 'auto' + Element.ID++;
    }
  }]);

  function Element() {
    var _ref = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {},
        type = _ref.type;

    _classCallCheck(this, Element);

    this.attrs = {
      id: Element.newID(),
      el: null,
      type: type || 'Base',
      classes: {}
    };

    this.boundingBox = null;
    this.context = null;
    this.rendered = false;

    // If a default registry exist, then register with it right away.
    if (_registry.Registry.getDefaultRegistry()) {
      _registry.Registry.getDefaultRegistry().register(this);
    }
  }

  // set the draw style of a stemmable note:


  _createClass(Element, [{
    key: 'setStyle',
    value: function setStyle(style) {
      this.style = style;return this;
    }
  }, {
    key: 'getStyle',
    value: function getStyle() {
      return this.style;
    }

    // Apply current style to Canvas `context`

  }, {
    key: 'applyStyle',
    value: function applyStyle() {
      var context = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : this.context;
      var style = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : this.getStyle();

      if (!style) return this;

      context.save();
      if (style.shadowColor) context.setShadowColor(style.shadowColor);
      if (style.shadowBlur) context.setShadowBlur(style.shadowBlur);
      if (style.fillStyle) context.setFillStyle(style.fillStyle);
      if (style.strokeStyle) context.setStrokeStyle(style.strokeStyle);
      if (style.lineWidth) context.setLineWidth(style.lineWidth);
      return this;
    }
  }, {
    key: 'restoreStyle',
    value: function restoreStyle() {
      var context = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : this.context;
      var style = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : this.getStyle();

      if (!style) return this;
      context.restore();
      return this;
    }

    // An element can have multiple class labels.

  }, {
    key: 'hasClass',
    value: function hasClass(className) {
      return this.attrs.classes[className] === true;
    }
  }, {
    key: 'addClass',
    value: function addClass(className) {
      this.attrs.classes[className] = true;
      if (this.registry) {
        this.registry.onUpdate({
          id: this.getAttribute('id'),
          name: 'class',
          value: className,
          oldValue: null
        });
      }
      return this;
    }
  }, {
    key: 'removeClass',
    value: function removeClass(className) {
      delete this.attrs.classes[className];
      if (this.registry) {
        this.registry.onUpdate({
          id: this.getAttribute('id'),
          name: 'class',
          value: null,
          oldValue: className
        });
      }
      return this;
    }

    // This is called by the registry after the element is registered.

  }, {
    key: 'onRegister',
    value: function onRegister(registry) {
      this.registry = registry;return this;
    }
  }, {
    key: 'isRendered',
    value: function isRendered() {
      return this.rendered;
    }
  }, {
    key: 'setRendered',
    value: function setRendered() {
      var rendered = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : true;
      this.rendered = rendered;return this;
    }
  }, {
    key: 'getAttributes',
    value: function getAttributes() {
      return this.attrs;
    }
  }, {
    key: 'getAttribute',
    value: function getAttribute(name) {
      return this.attrs[name];
    }
  }, {
    key: 'setAttribute',
    value: function setAttribute(name, value) {
      var id = this.attrs.id;
      var oldValue = this.attrs[name];
      this.attrs[name] = value;
      if (this.registry) {
        // Register with old id to support id changes.
        this.registry.onUpdate({ id: id, name: name, value: value, oldValue: oldValue });
      }
      return this;
    }
  }, {
    key: 'getContext',
    value: function getContext() {
      return this.context;
    }
  }, {
    key: 'setContext',
    value: function setContext(context) {
      this.context = context;return this;
    }
  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      return this.boundingBox;
    }

    // Validators

  }, {
    key: 'checkContext',
    value: function checkContext() {
      if (!this.context) {
        throw new _vex.Vex.RERR('NoContext', 'No rendering context attached to instance');
      }
      return this.context;
    }
  }]);

  return Element;
}();

Element.ID = 1000;

/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Modifier = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// `Modifier` is an abstract interface for notational elements that modify
// a `Note`. Examples of modifiers are `Accidental`, `Annotation`, `Stroke`, etc.
//
// For a `Modifier` instance to be positioned correctly, it must be part of
// a `ModifierContext`. All modifiers in the same context are rendered relative to
// one another.
//
// Typically, all modifiers to a note are part of the same `ModifierContext` instance. Also,
// in multi-voice staves, all modifiers to notes on the same `tick` are part of the same
// `ModifierContext`. This ensures that multiple voices don't trample all over each other.

// To enable logging for this class. Set `Vex.Flow.Modifier.DEBUG` to `true`.
// function L(...args) { if (Modifier.DEBUG) Vex.L('Vex.Flow.Modifier', args); }

var Modifier = exports.Modifier = function (_Element) {
  _inherits(Modifier, _Element);

  _createClass(Modifier, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'none';
    }

    // Modifiers can be positioned almost anywhere, relative to a note.

  }, {
    key: 'Position',
    get: function get() {
      return {
        LEFT: 1,
        RIGHT: 2,
        ABOVE: 3,
        BELOW: 4
      };
    }
  }, {
    key: 'PositionString',
    get: function get() {
      return {
        above: Modifier.Position.ABOVE,
        below: Modifier.Position.BELOW,
        left: Modifier.Position.LEFT,
        right: Modifier.Position.RIGHT
      };
    }
  }]);

  function Modifier() {
    _classCallCheck(this, Modifier);

    var _this = _possibleConstructorReturn(this, (Modifier.__proto__ || Object.getPrototypeOf(Modifier)).call(this));

    _this.setAttribute('type', 'Modifier');

    _this.width = 0;

    // Modifiers are attached to a note and an index. An index is a
    // specific head in a chord.
    _this.note = null;
    _this.index = null;

    // The `text_line` is reserved space above or below a stave.
    _this.text_line = 0;
    _this.position = Modifier.Position.LEFT;
    _this.modifier_context = null;
    _this.x_shift = 0;
    _this.y_shift = 0;
    _this.spacingFromNextModifier = 0;
    return _this;
  }

  // Every modifier has a category. The `ModifierContext` uses this to determine
  // the type and order of the modifiers.


  _createClass(Modifier, [{
    key: 'getCategory',
    value: function getCategory() {
      return Modifier.CATEGORY;
    }

    // Get and set modifier widths.

  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'setWidth',
    value: function setWidth(width) {
      this.width = width;return this;
    }

    // Get and set attached note (`StaveNote`, `TabNote`, etc.)

  }, {
    key: 'getNote',
    value: function getNote() {
      return this.note;
    }
  }, {
    key: 'setNote',
    value: function setNote(note) {
      this.note = note;return this;
    }

    // Get and set note index, which is a specific note in a chord.

  }, {
    key: 'getIndex',
    value: function getIndex() {
      return this.index;
    }
  }, {
    key: 'setIndex',
    value: function setIndex(index) {
      this.index = index;return this;
    }

    // Every modifier must be part of a `ModifierContext`.

  }, {
    key: 'getModifierContext',
    value: function getModifierContext() {
      return this.modifier_context;
    }
  }, {
    key: 'setModifierContext',
    value: function setModifierContext(c) {
      this.modifier_context = c;return this;
    }

    // Get and set articulation position.

  }, {
    key: 'getPosition',
    value: function getPosition() {
      return this.position;
    }
  }, {
    key: 'setPosition',
    value: function setPosition(position) {
      this.position = typeof position === 'string' ? Modifier.PositionString[position] : position;
      return this;
    }

    // Set the `text_line` for the modifier.

  }, {
    key: 'setTextLine',
    value: function setTextLine(line) {
      this.text_line = line;return this;
    }

    // Shift modifier down `y` pixels. Negative values shift up.

  }, {
    key: 'setYShift',
    value: function setYShift(y) {
      this.y_shift = y;return this;
    }
  }, {
    key: 'setSpacingFromNextModifier',
    value: function setSpacingFromNextModifier(x) {
      this.spacingFromNextModifier = x;
    }
  }, {
    key: 'getSpacingFromNextModifier',
    value: function getSpacingFromNextModifier() {
      return this.spacingFromNextModifier;
    }

    // Shift modifier `x` pixels in the direction of the modifier. Negative values
    // shift reverse.

  }, {
    key: 'setXShift',
    value: function setXShift(x) {
      this.x_shift = 0;
      if (this.position === Modifier.Position.LEFT) {
        this.x_shift -= x;
      } else {
        this.x_shift += x;
      }
    }
  }, {
    key: 'getXShift',
    value: function getXShift() {
      return this.x_shift;
    }

    // Render the modifier onto the canvas.

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      throw new _vex.Vex.RERR('MethodNotImplemented', 'draw() not implemented for this modifier.');
    }

    // aligns sub notes of NoteSubGroup (or GraceNoteGroup) to the main note with correct x-offset

  }, {
    key: 'alignSubNotesWithNote',
    value: function alignSubNotesWithNote(subNotes, note) {
      // Shift over the tick contexts of each note
      var tickContext = note.getTickContext();
      var extraPx = tickContext.getExtraPx();
      var subNoteXOffset = tickContext.getX() - extraPx.left - extraPx.extraLeft + this.getSpacingFromNextModifier();

      subNotes.forEach(function (subNote) {
        var subTickContext = subNote.getTickContext();
        subNote.setStave(note.stave);
        subTickContext.setXOffset(subNoteXOffset); // don't touch baseX to avoid shift each render
      });
    }
  }]);

  return Modifier;
}(_element.Element);

/***/ }),
/* 5 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveNote = undefined;

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _boundingbox = __webpack_require__(10);

var _stem = __webpack_require__(9);

var _notehead = __webpack_require__(41);

var _stemmablenote = __webpack_require__(20);

var _modifier = __webpack_require__(4);

var _dot = __webpack_require__(21);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This file implements notes for standard notation. This consists of one or
// more `NoteHeads`, an optional stem, and an optional flag.
//
// *Throughout these comments, a "note" refers to the entire `StaveNote`,
// and a "key" refers to a specific pitch/notehead within a note.*
//
// See `tests/stavenote_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.StaveNote.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (StaveNote.DEBUG) _vex.Vex.L('Vex.Flow.StaveNote', args);
}

var getStemAdjustment = function getStemAdjustment(note) {
  return _stem.Stem.WIDTH / (2 * -note.getStemDirection());
};

var isInnerNoteIndex = function isInnerNoteIndex(note, index) {
  return index === (note.getStemDirection() === _stem.Stem.UP ? note.keyProps.length - 1 : 0);
};

// Helper methods for rest positioning in ModifierContext.
function shiftRestVertical(rest, note, dir) {
  var delta = (note.isrest ? 0.0 : 1.0) * dir;

  rest.line += delta;
  rest.maxLine += delta;
  rest.minLine += delta;
  rest.note.setKeyLine(0, rest.note.getKeyLine(0) + delta);
}

// Called from formatNotes :: center a rest between two notes
function centerRest(rest, noteU, noteL) {
  var delta = rest.line - _vex.Vex.MidLine(noteU.minLine, noteL.maxLine);
  rest.note.setKeyLine(0, rest.note.getKeyLine(0) - delta);
  rest.line -= delta;
  rest.maxLine -= delta;
  rest.minLine -= delta;
}

var StaveNote = exports.StaveNote = function (_StemmableNote) {
  _inherits(StaveNote, _StemmableNote);

  _createClass(StaveNote, null, [{
    key: 'format',


    // ## Static Methods
    //
    // Format notes inside a ModifierContext.
    value: function format(notes, state) {
      if (!notes || notes.length < 2) return false;

      // FIXME: VexFlow will soon require that a stave be set before formatting.
      // Which, according to the below condition, means that following branch will
      // always be taken and the rest of this function is dead code.
      //
      // Problematically, `Formatter#formatByY` was not designed to work for more
      // than 2 voices (although, doesn't throw on this condition, just tries
      // to power through).
      //
      // Based on the above:
      //   * 2 voices can be formatted *with or without* a stave being set but
      //     the output will be different
      //   * 3 voices can only be formatted *without* a stave
      if (notes[0].getStave()) {
        return StaveNote.formatByY(notes, state);
      }

      var notesList = [];

      for (var i = 0; i < notes.length; i++) {
        var props = notes[i].getKeyProps();
        var line = props[0].line;
        var minL = props[props.length - 1].line;
        var stemDirection = notes[i].getStemDirection();
        var stemMax = notes[i].getStemLength() / 10;
        var stemMin = notes[i].getStemMinumumLength() / 10;

        var maxL = void 0;
        if (notes[i].isRest()) {
          maxL = line + notes[i].glyph.line_above;
          minL = line - notes[i].glyph.line_below;
        } else {
          maxL = stemDirection === 1 ? props[props.length - 1].line + stemMax : props[props.length - 1].line;

          minL = stemDirection === 1 ? props[0].line : props[0].line - stemMax;
        }

        notesList.push({
          line: props[0].line, // note/rest base line
          maxLine: maxL, // note/rest upper bounds line
          minLine: minL, // note/rest lower bounds line
          isrest: notes[i].isRest(),
          stemDirection: stemDirection,
          stemMax: stemMax, // Maximum (default) note stem length;
          stemMin: stemMin, // minimum note stem length
          voice_shift: notes[i].getVoiceShiftWidth(),
          is_displaced: notes[i].isDisplaced(), // note manually displaced
          note: notes[i]
        });
      }

      var voices = notesList.length;

      var noteU = notesList[0];
      var noteM = voices > 2 ? notesList[1] : null;
      var noteL = voices > 2 ? notesList[2] : notesList[1];

      // for two voice backward compatibility, ensure upper voice is stems up
      // for three voices, the voices must be in order (upper, middle, lower)
      if (voices === 2 && noteU.stemDirection === -1 && noteL.stemDirection === 1) {
        noteU = notesList[1];
        noteL = notesList[0];
      }

      var voiceXShift = Math.max(noteU.voice_shift, noteL.voice_shift);
      var xShift = 0;
      var stemDelta = void 0;

      // Test for two voice note intersection
      if (voices === 2) {
        var lineSpacing = noteU.stemDirection === noteL.stemDirection ? 0.0 : 0.5;
        // if top voice is a middle voice, check stem intersection with lower voice
        if (noteU.stemDirection === noteL.stemDirection && noteU.minLine <= noteL.maxLine) {
          if (!noteU.isrest) {
            stemDelta = Math.abs(noteU.line - (noteL.maxLine + 0.5));
            stemDelta = Math.max(stemDelta, noteU.stemMin);
            noteU.minLine = noteU.line - stemDelta;
            noteU.note.setStemLength(stemDelta * 10);
          }
        }
        if (noteU.minLine <= noteL.maxLine + lineSpacing) {
          if (noteU.isrest) {
            // shift rest up
            shiftRestVertical(noteU, noteL, 1);
          } else if (noteL.isrest) {
            // shift rest down
            shiftRestVertical(noteL, noteU, -1);
          } else {
            xShift = voiceXShift;
            if (noteU.stemDirection === noteL.stemDirection) {
              // upper voice is middle voice, so shift it right
              noteU.note.setXShift(xShift + 3);
            } else {
              // shift lower voice right
              noteL.note.setXShift(xShift);
            }
          }
        }

        // format complete
        return true;
      }

      // Check middle voice stem intersection with lower voice
      if (noteM !== null && noteM.minLine < noteL.maxLine + 0.5) {
        if (!noteM.isrest) {
          stemDelta = Math.abs(noteM.line - (noteL.maxLine + 0.5));
          stemDelta = Math.max(stemDelta, noteM.stemMin);
          noteM.minLine = noteM.line - stemDelta;
          noteM.note.setStemLength(stemDelta * 10);
        }
      }

      // For three voices, test if rests can be repositioned
      //
      // Special case 1 :: middle voice rest between two notes
      //
      if (noteM.isrest && !noteU.isrest && !noteL.isrest) {
        if (noteU.minLine <= noteM.maxLine || noteM.minLine <= noteL.maxLine) {
          var restHeight = noteM.maxLine - noteM.minLine;
          var space = noteU.minLine - noteL.maxLine;
          if (restHeight < space) {
            // center middle voice rest between the upper and lower voices
            centerRest(noteM, noteU, noteL);
          } else {
            xShift = voiceXShift + 3; // shift middle rest right
            noteM.note.setXShift(xShift);
          }
          // format complete
          return true;
        }
      }

      // Special case 2 :: all voices are rests
      if (noteU.isrest && noteM.isrest && noteL.isrest) {
        // Shift upper voice rest up
        shiftRestVertical(noteU, noteM, 1);
        // Shift lower voice rest down
        shiftRestVertical(noteL, noteM, -1);
        // format complete
        return true;
      }

      // Test if any other rests can be repositioned
      if (noteM.isrest && noteU.isrest && noteM.minLine <= noteL.maxLine) {
        // Shift middle voice rest up
        shiftRestVertical(noteM, noteL, 1);
      }
      if (noteM.isrest && noteL.isrest && noteU.minLine <= noteM.maxLine) {
        // Shift middle voice rest down
        shiftRestVertical(noteM, noteU, -1);
      }
      if (noteU.isrest && noteU.minLine <= noteM.maxLine) {
        // shift upper voice rest up;
        shiftRestVertical(noteU, noteM, 1);
      }
      if (noteL.isrest && noteM.minLine <= noteL.maxLine) {
        // shift lower voice rest down
        shiftRestVertical(noteL, noteM, -1);
      }

      // If middle voice intersects upper or lower voice
      if (!noteU.isrest && !noteM.isrest && noteU.minLine <= noteM.maxLine + 0.5 || !noteM.isrest && !noteL.isrest && noteM.minLine <= noteL.maxLine) {
        xShift = voiceXShift + 3; // shift middle note right
        noteM.note.setXShift(xShift);
      }

      return true;
    }
  }, {
    key: 'formatByY',
    value: function formatByY(notes, state) {
      // NOTE: this function does not support more than two voices per stave
      // use with care.
      var hasStave = true;

      for (var i = 0; i < notes.length; i++) {
        hasStave = hasStave && notes[i].getStave() != null;
      }

      if (!hasStave) {
        throw new _vex.Vex.RERR('Stave Missing', 'All notes must have a stave - Vex.Flow.ModifierContext.formatMultiVoice!');
      }

      var xShift = 0;

      for (var _i = 0; _i < notes.length - 1; _i++) {
        var topNote = notes[_i];
        var bottomNote = notes[_i + 1];

        if (topNote.getStemDirection() === _stem.Stem.DOWN) {
          topNote = notes[_i + 1];
          bottomNote = notes[_i];
        }

        var topKeys = topNote.getKeyProps();
        var bottomKeys = bottomNote.getKeyProps();

        var HALF_NOTEHEAD_HEIGHT = 0.5;

        // `keyProps` and `stave.getYForLine` have different notions of a `line`
        // so we have to convert the keyProps value by subtracting 5.
        // See https://github.com/0xfe/vexflow/wiki/Development-Gotchas
        //
        // We also extend the y for each note by a half notehead because the
        // notehead's origin is centered
        var topNotBottomY = topNote.getStave().getYForLine(5 - topKeys[0].line + HALF_NOTEHEAD_HEIGHT);

        var bottomNoteTopY = bottomNote.getStave().getYForLine(5 - bottomKeys[bottomKeys.length - 1].line - HALF_NOTEHEAD_HEIGHT);

        var areNotesColliding = bottomNoteTopY - topNotBottomY < 0;

        if (areNotesColliding) {
          xShift = topNote.getVoiceShiftWidth() + 2;
          bottomNote.setXShift(xShift);
        }
      }

      state.right_shift += xShift;
    }
  }, {
    key: 'postFormat',
    value: function postFormat(notes) {
      if (!notes) return false;

      notes.forEach(function (note) {
        return note.postFormat();
      });

      return true;
    }
  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'stavenotes';
    }
  }, {
    key: 'STEM_UP',
    get: function get() {
      return _stem.Stem.UP;
    }
  }, {
    key: 'STEM_DOWN',
    get: function get() {
      return _stem.Stem.DOWN;
    }
  }, {
    key: 'DEFAULT_LEDGER_LINE_OFFSET',
    get: function get() {
      return 3;
    }
  }]);

  function StaveNote(noteStruct) {
    _classCallCheck(this, StaveNote);

    var _this = _possibleConstructorReturn(this, (StaveNote.__proto__ || Object.getPrototypeOf(StaveNote)).call(this, noteStruct));

    _this.setAttribute('type', 'StaveNote');

    _this.keys = noteStruct.keys;
    _this.clef = noteStruct.clef;
    _this.octave_shift = noteStruct.octave_shift;
    _this.beam = null;

    // Pull note rendering properties
    _this.glyph = _tables.Flow.durationToGlyph(_this.duration, _this.noteType);

    if (!_this.glyph) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Invalid note initialization data (No glyph found): ' + JSON.stringify(noteStruct));
    }

    // if true, displace note to right
    _this.displaced = false;
    _this.dot_shiftY = 0;
    // per-pitch properties
    _this.keyProps = [];
    // for displaced ledger lines
    _this.use_default_head_x = false;

    // Drawing
    _this.note_heads = [];
    _this.modifiers = [];

    _vex.Vex.Merge(_this.render_options, {
      // font size for note heads and rests
      glyph_font_scale: noteStruct.glyph_font_scale || _tables.Flow.DEFAULT_NOTATION_FONT_SCALE,
      // number of stroke px to the left and right of head
      stroke_px: noteStruct.stroke_px || StaveNote.DEFAULT_LEDGER_LINE_OFFSET
    });

    _this.calculateKeyProps();
    _this.buildStem();

    // Set the stem direction
    if (noteStruct.auto_stem) {
      _this.autoStem();
    } else {
      _this.setStemDirection(noteStruct.stem_direction);
    }
    _this.reset();
    _this.buildFlag();
    return _this;
  }

  _createClass(StaveNote, [{
    key: 'reset',
    value: function reset() {
      var _this2 = this;

      _get(StaveNote.prototype.__proto__ || Object.getPrototypeOf(StaveNote.prototype), 'reset', this).call(this);

      // Save prior noteHead styles & reapply them after making new noteheads.
      var noteHeadStyles = this.note_heads.map(function (noteHead) {
        return noteHead.getStyle();
      });
      this.buildNoteHeads();
      this.note_heads.forEach(function (noteHead, index) {
        return noteHead.setStyle(noteHeadStyles[index]);
      });

      if (this.stave) {
        this.note_heads.forEach(function (head) {
          return head.setStave(_this2.stave);
        });
      }
      this.calcExtraPx();
    }
  }, {
    key: 'getCategory',
    value: function getCategory() {
      return StaveNote.CATEGORY;
    }

    // Builds a `Stem` for the note

  }, {
    key: 'buildStem',
    value: function buildStem() {
      var glyph = this.getGlyph();
      var yExtend = glyph.code_head === 'v95' || glyph.code_head === 'v3e' ? -4 : 0;

      this.setStem(new _stem.Stem({
        yExtend: yExtend,
        hide: !!this.isRest()
      }));
    }

    // Builds a `NoteHead` for each key in the note

  }, {
    key: 'buildNoteHeads',
    value: function buildNoteHeads() {
      this.note_heads = [];
      var stemDirection = this.getStemDirection();
      var keys = this.getKeys();

      var lastLine = null;
      var lineDiff = null;
      var displaced = false;

      // Draw notes from bottom to top.

      // For down-stem notes, we draw from top to bottom.
      var start = void 0;
      var end = void 0;
      var step = void 0;
      if (stemDirection === _stem.Stem.UP) {
        start = 0;
        end = keys.length;
        step = 1;
      } else if (stemDirection === _stem.Stem.DOWN) {
        start = keys.length - 1;
        end = -1;
        step = -1;
      }

      for (var i = start; i !== end; i += step) {
        var noteProps = this.keyProps[i];
        var line = noteProps.line;

        // Keep track of last line with a note head, so that consecutive heads
        // are correctly displaced.
        if (lastLine === null) {
          lastLine = line;
        } else {
          lineDiff = Math.abs(lastLine - line);
          if (lineDiff === 0 || lineDiff === 0.5) {
            displaced = !displaced;
          } else {
            displaced = false;
            this.use_default_head_x = true;
          }
        }
        lastLine = line;

        var notehead = new _notehead.NoteHead({
          duration: this.duration,
          note_type: this.noteType,
          displaced: displaced,
          stem_direction: stemDirection,
          custom_glyph_code: noteProps.code,
          glyph_font_scale: this.render_options.glyph_font_scale,
          x_shift: noteProps.shift_right,
          line: noteProps.line
        });

        this.note_heads[i] = notehead;
      }
    }

    // Automatically sets the stem direction based on the keys in the note

  }, {
    key: 'autoStem',
    value: function autoStem() {
      // Figure out optimal stem direction based on given notes
      this.minLine = this.keyProps[0].line;
      this.maxLine = this.keyProps[this.keyProps.length - 1].line;

      var MIDDLE_LINE = 3;
      var decider = (this.minLine + this.maxLine) / 2;
      var stemDirection = decider < MIDDLE_LINE ? _stem.Stem.UP : _stem.Stem.DOWN;

      this.setStemDirection(stemDirection);
    }

    // Calculates and stores the properties for each key in the note

  }, {
    key: 'calculateKeyProps',
    value: function calculateKeyProps() {
      var lastLine = null;
      for (var i = 0; i < this.keys.length; ++i) {
        var key = this.keys[i];

        // All rests use the same position on the line.
        // if (this.glyph.rest) key = this.glyph.position;
        if (this.glyph.rest) this.glyph.position = key;

        var options = { octave_shift: this.octave_shift || 0 };
        var props = _tables.Flow.keyProperties(key, this.clef, options);

        if (!props) {
          throw new _vex.Vex.RuntimeError('BadArguments', 'Invalid key for note properties: ' + key);
        }

        // Override line placement for default rests
        if (props.key === 'R') {
          if (this.duration === '1' || this.duration === 'w') {
            props.line = 4;
          } else {
            props.line = 3;
          }
        }

        // Calculate displacement of this note
        var line = props.line;
        if (lastLine === null) {
          lastLine = line;
        } else {
          if (Math.abs(lastLine - line) === 0.5) {
            this.displaced = true;
            props.displaced = true;

            // Have to mark the previous note as
            // displaced as well, for modifier placement
            if (this.keyProps.length > 0) {
              this.keyProps[i - 1].displaced = true;
            }
          }
        }

        lastLine = line;
        this.keyProps.push(props);
      }

      // Sort the notes from lowest line to highest line
      lastLine = -Infinity;
      this.keyProps.forEach(function (key) {
        if (key.line < lastLine) {
          _vex.Vex.W('Unsorted keys in note will be sorted. ' + 'See https://github.com/0xfe/vexflow/issues/104 for details.');
        }
        lastLine = key.line;
      });
      this.keyProps.sort(function (a, b) {
        return a.line - b.line;
      });
    }

    // Get the `BoundingBox` for the entire note

  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call getBoundingBox on an unformatted note.");
      }

      var _getMetrics = this.getMetrics(),
          w = _getMetrics.width,
          modLeftPx = _getMetrics.modLeftPx,
          extraLeftPx = _getMetrics.extraLeftPx;

      var x = this.getAbsoluteX() - modLeftPx - extraLeftPx;

      var minY = 0;
      var maxY = 0;
      var halfLineSpacing = this.getStave().getSpacingBetweenLines() / 2;
      var lineSpacing = halfLineSpacing * 2;

      if (this.isRest()) {
        var y = this.ys[0];
        var frac = _tables.Flow.durationToFraction(this.duration);
        if (frac.equals(1) || frac.equals(2)) {
          minY = y - halfLineSpacing;
          maxY = y + halfLineSpacing;
        } else {
          minY = y - this.glyph.line_above * lineSpacing;
          maxY = y + this.glyph.line_below * lineSpacing;
        }
      } else if (this.glyph.stem) {
        var ys = this.getStemExtents();
        ys.baseY += halfLineSpacing * this.stem_direction;
        minY = Math.min(ys.topY, ys.baseY);
        maxY = Math.max(ys.topY, ys.baseY);
      } else {
        minY = null;
        maxY = null;

        for (var i = 0; i < this.ys.length; ++i) {
          var yy = this.ys[i];
          if (i === 0) {
            minY = yy;
            maxY = yy;
          } else {
            minY = Math.min(yy, minY);
            maxY = Math.max(yy, maxY);
          }
        }
        minY -= halfLineSpacing;
        maxY += halfLineSpacing;
      }

      return new _boundingbox.BoundingBox(x, minY, w, maxY - minY);
    }

    // Gets the line number of the top or bottom note in the chord.
    // If `isTopNote` is `true` then get the top note

  }, {
    key: 'getLineNumber',
    value: function getLineNumber(isTopNote) {
      if (!this.keyProps.length) {
        throw new _vex.Vex.RERR('NoKeyProps', "Can't get bottom note line, because note is not initialized properly.");
      }

      var resultLine = this.keyProps[0].line;

      // No precondition assumed for sortedness of keyProps array
      for (var i = 0; i < this.keyProps.length; i++) {
        var thisLine = this.keyProps[i].line;
        if (isTopNote) {
          if (thisLine > resultLine) resultLine = thisLine;
        } else {
          if (thisLine < resultLine) resultLine = thisLine;
        }
      }

      return resultLine;
    }

    // Determine if current note is a rest

  }, {
    key: 'isRest',
    value: function isRest() {
      return this.glyph.rest;
    }

    // Determine if the current note is a chord

  }, {
    key: 'isChord',
    value: function isChord() {
      return !this.isRest() && this.keys.length > 1;
    }

    // Determine if the `StaveNote` has a stem

  }, {
    key: 'hasStem',
    value: function hasStem() {
      return this.glyph.stem;
    }
  }, {
    key: 'hasFlag',
    value: function hasFlag() {
      return _get(StaveNote.prototype.__proto__ || Object.getPrototypeOf(StaveNote.prototype), 'hasFlag', this).call(this) && !this.isRest();
    }
  }, {
    key: 'getStemX',
    value: function getStemX() {
      if (this.noteType === 'r') {
        return this.getCenterGlyphX();
      } else {
        // We adjust the origin of the stem because we want the stem left-aligned
        // with the notehead if stemmed-down, and right-aligned if stemmed-up
        return _get(StaveNote.prototype.__proto__ || Object.getPrototypeOf(StaveNote.prototype), 'getStemX', this).call(this) + getStemAdjustment(this);
      }
    }

    // Get the `y` coordinate for text placed on the top/bottom of a
    // note at a desired `text_line`

  }, {
    key: 'getYForTopText',
    value: function getYForTopText(textLine) {
      var extents = this.getStemExtents();
      return Math.min(this.stave.getYForTopText(textLine), extents.topY - this.render_options.annotation_spacing * (textLine + 1));
    }
  }, {
    key: 'getYForBottomText',
    value: function getYForBottomText(textLine) {
      var extents = this.getStemExtents();
      return Math.max(this.stave.getYForTopText(textLine), extents.baseY + this.render_options.annotation_spacing * textLine);
    }

    // Sets the current note to the provided `stave`. This applies
    // `y` values to the `NoteHeads`.

  }, {
    key: 'setStave',
    value: function setStave(stave) {
      _get(StaveNote.prototype.__proto__ || Object.getPrototypeOf(StaveNote.prototype), 'setStave', this).call(this, stave);

      var ys = this.note_heads.map(function (notehead) {
        notehead.setStave(stave);
        return notehead.getY();
      });

      this.setYs(ys);

      if (this.stem) {
        var _getNoteHeadBounds = this.getNoteHeadBounds(),
            y_top = _getNoteHeadBounds.y_top,
            y_bottom = _getNoteHeadBounds.y_bottom;

        this.stem.setYBounds(y_top, y_bottom);
      }

      return this;
    }

    // Get the pitches in the note

  }, {
    key: 'getKeys',
    value: function getKeys() {
      return this.keys;
    }

    // Get the properties for all the keys in the note

  }, {
    key: 'getKeyProps',
    value: function getKeyProps() {
      return this.keyProps;
    }

    // Check if note is shifted to the right

  }, {
    key: 'isDisplaced',
    value: function isDisplaced() {
      return this.displaced;
    }

    // Sets whether shift note to the right. `displaced` is a `boolean`

  }, {
    key: 'setNoteDisplaced',
    value: function setNoteDisplaced(displaced) {
      this.displaced = displaced;
      return this;
    }

    // Get the starting `x` coordinate for a `StaveTie`

  }, {
    key: 'getTieRightX',
    value: function getTieRightX() {
      var tieStartX = this.getAbsoluteX();
      tieStartX += this.getGlyphWidth() + this.x_shift + this.extraRightPx;
      if (this.modifierContext) tieStartX += this.modifierContext.getExtraRightPx();
      return tieStartX;
    }

    // Get the ending `x` coordinate for a `StaveTie`

  }, {
    key: 'getTieLeftX',
    value: function getTieLeftX() {
      var tieEndX = this.getAbsoluteX();
      tieEndX += this.x_shift - this.extraLeftPx;
      return tieEndX;
    }

    // Get the stave line on which to place a rest

  }, {
    key: 'getLineForRest',
    value: function getLineForRest() {
      var restLine = this.keyProps[0].line;
      if (this.keyProps.length > 1) {
        var lastLine = this.keyProps[this.keyProps.length - 1].line;
        var top = Math.max(restLine, lastLine);
        var bot = Math.min(restLine, lastLine);
        restLine = _vex.Vex.MidLine(top, bot);
      }

      return restLine;
    }

    // Get the default `x` and `y` coordinates for the provided `position`
    // and key `index`

  }, {
    key: 'getModifierStartXY',
    value: function getModifierStartXY(position, index) {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call GetModifierStartXY on an unformatted note");
      }

      if (this.ys.length === 0) {
        throw new _vex.Vex.RERR('NoYValues', 'No Y-Values calculated for this note.');
      }

      var _Modifier$Position = _modifier.Modifier.Position,
          ABOVE = _Modifier$Position.ABOVE,
          BELOW = _Modifier$Position.BELOW,
          LEFT = _Modifier$Position.LEFT,
          RIGHT = _Modifier$Position.RIGHT;

      var x = 0;
      if (position === LEFT) {
        // extra_left_px
        // FIXME: What are these magic numbers?
        x = -1 * 2;
      } else if (position === RIGHT) {
        // extra_right_px
        // FIXME: What is this magical +2?
        x = this.getGlyphWidth() + this.x_shift + 2;

        if (this.stem_direction === _stem.Stem.UP && this.hasFlag() && isInnerNoteIndex(this, index)) {
          x += this.flag.getMetrics().width;
        }
      } else if (position === BELOW || position === ABOVE) {
        x = this.getGlyphWidth() / 2;
      }

      return {
        x: this.getAbsoluteX() + x,
        y: this.ys[index]
      };
    }

    // Sets the style of the complete StaveNote, including all keys
    // and the stem.

  }, {
    key: 'setStyle',
    value: function setStyle(style) {
      _get(StaveNote.prototype.__proto__ || Object.getPrototypeOf(StaveNote.prototype), 'setStyle', this).call(this, style);
      this.note_heads.forEach(function (notehead) {
        return notehead.setStyle(style);
      });
      this.stem.setStyle(style);
    }
  }, {
    key: 'setStemStyle',
    value: function setStemStyle(style) {
      var stem = this.getStem();
      stem.setStyle(style);
    }
  }, {
    key: 'getStemStyle',
    value: function getStemStyle() {
      return this.stem.getStyle();
    }
  }, {
    key: 'setLedgerLineStyle',
    value: function setLedgerLineStyle(style) {
      this.ledgerLineStyle = style;
    }
  }, {
    key: 'getLedgerLineStyle',
    value: function getLedgerLineStyle() {
      return this.ledgerLineStyle;
    }
  }, {
    key: 'setFlagStyle',
    value: function setFlagStyle(style) {
      this.flagStyle = style;
    }
  }, {
    key: 'getFlagStyle',
    value: function getFlagStyle() {
      return this.flagStyle;
    }

    // Sets the notehead at `index` to the provided coloring `style`.
    //
    // `style` is an `object` with the following properties: `shadowColor`,
    // `shadowBlur`, `fillStyle`, `strokeStyle`

  }, {
    key: 'setKeyStyle',
    value: function setKeyStyle(index, style) {
      this.note_heads[index].setStyle(style);
      return this;
    }
  }, {
    key: 'setKeyLine',
    value: function setKeyLine(index, line) {
      this.keyProps[index].line = line;
      this.reset();
      return this;
    }
  }, {
    key: 'getKeyLine',
    value: function getKeyLine(index) {
      return this.keyProps[index].line;
    }

    // Add self to modifier context. `mContext` is the `ModifierContext`
    // to be added to.

  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext(mContext) {
      this.setModifierContext(mContext);
      for (var i = 0; i < this.modifiers.length; ++i) {
        this.modifierContext.addModifier(this.modifiers[i]);
      }
      this.modifierContext.addModifier(this);
      this.setPreFormatted(false);
      return this;
    }

    // Generic function to add modifiers to a note
    //
    // Parameters:
    // * `index`: The index of the key that we're modifying
    // * `modifier`: The modifier to add

  }, {
    key: 'addModifier',
    value: function addModifier(index, modifier) {
      modifier.setNote(this);
      modifier.setIndex(index);
      this.modifiers.push(modifier);
      this.setPreFormatted(false);
      return this;
    }

    // Helper function to add an accidental to a key

  }, {
    key: 'addAccidental',
    value: function addAccidental(index, accidental) {
      return this.addModifier(index, accidental);
    }

    // Helper function to add an articulation to a key

  }, {
    key: 'addArticulation',
    value: function addArticulation(index, articulation) {
      return this.addModifier(index, articulation);
    }

    // Helper function to add an annotation to a key

  }, {
    key: 'addAnnotation',
    value: function addAnnotation(index, annotation) {
      return this.addModifier(index, annotation);
    }

    // Helper function to add a dot on a specific key

  }, {
    key: 'addDot',
    value: function addDot(index) {
      var dot = new _dot.Dot();
      dot.setDotShiftY(this.glyph.dot_shiftY);
      this.dots++;
      return this.addModifier(index, dot);
    }

    // Convenience method to add dot to all keys in note

  }, {
    key: 'addDotToAll',
    value: function addDotToAll() {
      for (var i = 0; i < this.keys.length; ++i) {
        this.addDot(i);
      }
      return this;
    }

    // Get all accidentals in the `ModifierContext`

  }, {
    key: 'getAccidentals',
    value: function getAccidentals() {
      return this.modifierContext.getModifiers('accidentals');
    }

    // Get all dots in the `ModifierContext`

  }, {
    key: 'getDots',
    value: function getDots() {
      return this.modifierContext.getModifiers('dots');
    }

    // Get the width of the note if it is displaced. Used for `Voice`
    // formatting

  }, {
    key: 'getVoiceShiftWidth',
    value: function getVoiceShiftWidth() {
      // TODO: may need to accomodate for dot here.
      return this.getGlyphWidth() * (this.displaced ? 2 : 1);
    }

    // Calculates and sets the extra pixels to the left or right
    // if the note is displaced.

  }, {
    key: 'calcExtraPx',
    value: function calcExtraPx() {
      this.setExtraLeftPx(this.displaced && this.stem_direction === _stem.Stem.DOWN ? this.getGlyphWidth() : 0);

      // For upstems with flags, the extra space is unnecessary, since it's taken
      // up by the flag.
      this.setExtraRightPx(!this.hasFlag() && this.displaced && this.stem_direction === _stem.Stem.UP ? this.getGlyphWidth() : 0);
    }

    // Pre-render formatting

  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return;
      if (this.modifierContext) this.modifierContext.preFormat();

      var width = this.getGlyphWidth() + this.extraLeftPx + this.extraRightPx;

      // For upward flagged notes, the width of the flag needs to be added
      if (this.glyph.flag && this.beam === null && this.stem_direction === _stem.Stem.UP) {
        width += this.getGlyphWidth();
      }

      this.setWidth(width);
      this.setPreFormatted(true);
    }

    /**
     * @typedef {Object} noteHeadBounds
     * @property {number} y_top the highest notehead bound
     * @property {number} y_bottom the lowest notehead bound
     * @property {number|Null} displaced_x the starting x for displaced noteheads
     * @property {number|Null} non_displaced_x the starting x for non-displaced noteheads
     * @property {number} highest_line the highest notehead line in traditional music line
     *  numbering (bottom line = 1, top line = 5)
     * @property {number} lowest_line the lowest notehead line
     * @property {number|false} highest_displaced_line the highest staff line number
     *   for a displaced notehead
     * @property {number|false} lowest_displaced_line
     * @property {number} highest_non_displaced_line
     * @property {number} lowest_non_displaced_line
     */

    /**
     * Get the staff line and y value for the highest & lowest noteheads
     * @returns {noteHeadBounds}
     */

  }, {
    key: 'getNoteHeadBounds',
    value: function getNoteHeadBounds() {
      // Top and bottom Y values for stem.
      var yTop = null;
      var yBottom = null;
      var nonDisplacedX = null;
      var displacedX = null;

      var highestLine = this.stave.getNumLines();
      var lowestLine = 1;
      var highestDisplacedLine = false;
      var lowestDisplacedLine = false;
      var highestNonDisplacedLine = highestLine;
      var lowestNonDisplacedLine = lowestLine;

      this.note_heads.forEach(function (notehead) {
        var line = notehead.getLine();
        var y = notehead.getY();

        if (yTop === null || y < yTop) {
          yTop = y;
        }

        if (yBottom === null || y > yBottom) {
          yBottom = y;
        }

        if (displacedX === null && notehead.isDisplaced()) {
          displacedX = notehead.getAbsoluteX();
        }

        if (nonDisplacedX === null && !notehead.isDisplaced()) {
          nonDisplacedX = notehead.getAbsoluteX();
        }

        highestLine = line > highestLine ? line : highestLine;
        lowestLine = line < lowestLine ? line : lowestLine;

        if (notehead.isDisplaced()) {
          highestDisplacedLine = highestDisplacedLine === false ? line : Math.max(line, highestDisplacedLine);
          lowestDisplacedLine = lowestDisplacedLine === false ? line : Math.min(line, lowestDisplacedLine);
        } else {
          highestNonDisplacedLine = Math.max(line, highestNonDisplacedLine);
          lowestNonDisplacedLine = Math.min(line, lowestNonDisplacedLine);
        }
      }, this);

      return {
        y_top: yTop,
        y_bottom: yBottom,
        displaced_x: displacedX,
        non_displaced_x: nonDisplacedX,
        highest_line: highestLine,
        lowest_line: lowestLine,
        highest_displaced_line: highestDisplacedLine,
        lowest_displaced_line: lowestDisplacedLine,
        highest_non_displaced_line: highestNonDisplacedLine,
        lowest_non_displaced_line: lowestNonDisplacedLine
      };
    }

    // Get the starting `x` coordinate for the noteheads

  }, {
    key: 'getNoteHeadBeginX',
    value: function getNoteHeadBeginX() {
      return this.getAbsoluteX() + this.x_shift;
    }

    // Get the ending `x` coordinate for the noteheads

  }, {
    key: 'getNoteHeadEndX',
    value: function getNoteHeadEndX() {
      var xBegin = this.getNoteHeadBeginX();
      return xBegin + this.getGlyphWidth();
    }

    // Draw the ledger lines between the stave and the highest/lowest keys

  }, {
    key: 'drawLedgerLines',
    value: function drawLedgerLines() {
      var stave = this.stave,
          glyph = this.glyph,
          stroke_px = this.render_options.stroke_px,
          ctx = this.context;


      var width = glyph.getWidth() + stroke_px * 2;
      var doubleWidth = 2 * (glyph.getWidth() + stroke_px) - _stem.Stem.WIDTH / 2;

      if (this.isRest()) return;
      if (!ctx) {
        throw new _vex.Vex.RERR('NoCanvasContext', "Can't draw without a canvas context.");
      }

      var _getNoteHeadBounds2 = this.getNoteHeadBounds(),
          highest_line = _getNoteHeadBounds2.highest_line,
          lowest_line = _getNoteHeadBounds2.lowest_line,
          highest_displaced_line = _getNoteHeadBounds2.highest_displaced_line,
          highest_non_displaced_line = _getNoteHeadBounds2.highest_non_displaced_line,
          lowest_displaced_line = _getNoteHeadBounds2.lowest_displaced_line,
          lowest_non_displaced_line = _getNoteHeadBounds2.lowest_non_displaced_line,
          displaced_x = _getNoteHeadBounds2.displaced_x,
          non_displaced_x = _getNoteHeadBounds2.non_displaced_x;

      var min_x = Math.min(displaced_x, non_displaced_x);

      var drawLedgerLine = function drawLedgerLine(y, normal, displaced) {
        var x = void 0;
        if (displaced && normal) x = min_x - stroke_px;else if (normal) x = non_displaced_x - stroke_px;else x = displaced_x - stroke_px;
        var ledgerWidth = normal && displaced ? doubleWidth : width;

        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.lineTo(x + ledgerWidth, y);
        ctx.stroke();
      };

      var style = _extends({}, stave.getStyle() || {}, this.getLedgerLineStyle() || {});
      this.applyStyle(ctx, style);

      // Draw ledger lines below the staff:
      for (var line = 6; line <= highest_line; ++line) {
        var normal = non_displaced_x !== null && line <= highest_non_displaced_line;
        var displaced = displaced_x !== null && line <= highest_displaced_line;
        drawLedgerLine(stave.getYForNote(line), normal, displaced);
      }

      // Draw ledger lines above the staff:
      for (var _line = 0; _line >= lowest_line; --_line) {
        var _normal = non_displaced_x !== null && _line >= lowest_non_displaced_line;
        var _displaced = displaced_x !== null && _line >= lowest_displaced_line;
        drawLedgerLine(stave.getYForNote(_line), _normal, _displaced);
      }

      this.restoreStyle(ctx, style);
    }

    // Draw all key modifiers

  }, {
    key: 'drawModifiers',
    value: function drawModifiers() {
      if (!this.context) {
        throw new _vex.Vex.RERR('NoCanvasContext', "Can't draw without a canvas context.");
      }

      var ctx = this.context;
      ctx.openGroup('modifiers');
      for (var i = 0; i < this.modifiers.length; i++) {
        var modifier = this.modifiers[i];
        var notehead = this.note_heads[modifier.getIndex()];
        var noteheadStyle = notehead.getStyle();
        if (noteheadStyle) {
          ctx.save();
          notehead.applyStyle(ctx);
        }
        modifier.setContext(ctx);
        modifier.draw();
        if (noteheadStyle) {
          notehead.restoreStyle(ctx);
        }
      }
      ctx.closeGroup();
    }

    // Draw the flag for the note

  }, {
    key: 'drawFlag',
    value: function drawFlag() {
      var stem = this.stem,
          beam = this.beam,
          ctx = this.context;


      if (!ctx) {
        throw new _vex.Vex.RERR('NoCanvasContext', "Can't draw without a canvas context.");
      }

      var shouldRenderFlag = beam === null;
      var glyph = this.getGlyph();

      if (glyph.flag && shouldRenderFlag) {
        var _getNoteHeadBounds3 = this.getNoteHeadBounds(),
            y_top = _getNoteHeadBounds3.y_top,
            y_bottom = _getNoteHeadBounds3.y_bottom;

        var noteStemHeight = stem.getHeight();
        var flagX = this.getStemX();
        // FIXME: What's with the magic +/- 2
        var flagY = this.getStemDirection() === _stem.Stem.DOWN
        // Down stems have flags on the left
        ? y_top - noteStemHeight + 2
        // Up stems have flags on the eft.
        : y_bottom - noteStemHeight - 2;

        // Draw the Flag
        ctx.openGroup('flag', null, { pointerBBox: true });
        this.applyStyle(ctx, this.getFlagStyle() || false);
        this.flag.render(ctx, flagX, flagY);
        this.restoreStyle(ctx, this.getFlagStyle() || false);
        ctx.closeGroup();
      }
    }

    // Draw the NoteHeads

  }, {
    key: 'drawNoteHeads',
    value: function drawNoteHeads() {
      var _this3 = this;

      this.note_heads.forEach(function (notehead) {
        _this3.context.openGroup('notehead', null, { pointerBBox: true });
        notehead.setContext(_this3.context).draw();
        _this3.context.closeGroup();
      });
    }

    // Render the stem onto the canvas

  }, {
    key: 'drawStem',
    value: function drawStem(stemStruct) {
      // GCR TODO: I can't find any context in which this is called with the stemStruct
      // argument in the codebase or tests. Nor can I find a case where super.drawStem
      // is called at all. Perhaps these should be removed?
      if (!this.context) {
        throw new _vex.Vex.RERR('NoCanvasContext', "Can't draw without a canvas context.");
      }

      if (stemStruct) {
        this.setStem(new _stem.Stem(stemStruct));
      }

      this.context.openGroup('stem', null, { pointerBBox: true });
      this.stem.setContext(this.context).draw();
      this.context.closeGroup();
    }

    // Draws all the `StaveNote` parts. This is the main drawing method.

  }, {
    key: 'draw',
    value: function draw() {
      if (!this.context) {
        throw new _vex.Vex.RERR('NoCanvasContext', "Can't draw without a canvas context.");
      }
      if (!this.stave) {
        throw new _vex.Vex.RERR('NoStave', "Can't draw without a stave.");
      }
      if (this.ys.length === 0) {
        throw new _vex.Vex.RERR('NoYValues', "Can't draw note without Y values.");
      }

      var xBegin = this.getNoteHeadBeginX();
      var shouldRenderStem = this.hasStem() && !this.beam;

      // Format note head x positions
      this.note_heads.forEach(function (notehead) {
        return notehead.setX(xBegin);
      });

      // Format stem x positions
      var stemX = this.getStemX();
      this.stem.setNoteHeadXBounds(stemX, stemX);

      L('Rendering ', this.isChord() ? 'chord :' : 'note :', this.keys);

      // Draw each part of the note
      this.drawLedgerLines();

      // Apply the overall style -- may be contradicted by local settings:
      this.applyStyle();
      this.setAttribute('el', this.context.openGroup('stavenote', this.getAttribute('id')));
      this.context.openGroup('note', null, { pointerBBox: true });
      if (shouldRenderStem) this.drawStem();
      this.drawNoteHeads();
      this.drawFlag();
      this.context.closeGroup();
      this.drawModifiers();
      this.context.closeGroup();
      this.restoreStyle();
      this.setRendered();
    }
  }]);

  return StaveNote;
}(_stemmablenote.StemmableNote);

/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Note = undefined;

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _tickable = __webpack_require__(42);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements an abstract interface for notes and chords that
// are rendered on a stave. Notes have some common properties: All of them
// have a value (e.g., pitch, fret, etc.) and a duration (quarter, half, etc.)
//
// Some notes have stems, heads, dots, etc. Most notational elements that
// surround a note are called *modifiers*, and every note has an associated
// array of them. All notes also have a rendering context and belong to a stave.

var Note = exports.Note = function (_Tickable) {
  _inherits(Note, _Tickable);

  _createClass(Note, null, [{
    key: 'plotMetrics',


    // Debug helper. Displays various note metrics for the given
    // note.
    value: function plotMetrics(ctx, note, yPos) {
      var metrics = note.getMetrics();
      var xStart = note.getAbsoluteX() - metrics.modLeftPx - metrics.extraLeftPx;
      var xPre1 = note.getAbsoluteX() - metrics.extraLeftPx;
      var xAbs = note.getAbsoluteX();
      var xPost1 = note.getAbsoluteX() + metrics.noteWidth;
      var xPost2 = note.getAbsoluteX() + metrics.noteWidth + metrics.extraRightPx;
      var xEnd = note.getAbsoluteX() + metrics.noteWidth + metrics.extraRightPx + metrics.modRightPx;
      var xFreedomRight = xEnd + note.getFormatterMetrics().freedom.right;

      var xWidth = xEnd - xStart;
      ctx.save();
      ctx.setFont('Arial', 8, '');
      ctx.fillText(Math.round(xWidth) + 'px', xStart + note.getXShift(), yPos);

      var y = yPos + 7;
      function stroke(x1, x2, color) {
        var yy = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : y;

        ctx.beginPath();
        ctx.setStrokeStyle(color);
        ctx.setFillStyle(color);
        ctx.setLineWidth(3);
        ctx.moveTo(x1 + note.getXShift(), yy);
        ctx.lineTo(x2 + note.getXShift(), yy);
        ctx.stroke();
      }

      stroke(xStart, xPre1, 'red');
      stroke(xPre1, xAbs, '#999');
      stroke(xAbs, xPost1, 'green');
      stroke(xPost1, xPost2, '#999');
      stroke(xPost2, xEnd, 'red');
      stroke(xEnd, xFreedomRight, '#DD0');
      stroke(xStart - note.getXShift(), xStart, '#BBB'); // Shift
      _vex.Vex.drawDot(ctx, xAbs + note.getXShift(), y, 'blue');

      var formatterMetrics = note.getFormatterMetrics();
      if (formatterMetrics.iterations > 0) {
        var spaceDeviation = formatterMetrics.space.deviation;
        var prefix = spaceDeviation >= 0 ? '+' : '';
        ctx.setFillStyle('red');
        ctx.fillText(prefix + Math.round(spaceDeviation), xAbs + note.getXShift(), yPos - 10);
      }
      ctx.restore();
    }

    // Every note is a tickable, i.e., it can be mutated by the `Formatter` class for
    // positioning and layout.
    // To create a new note you need to provide a `note_struct`, which consists
    // of the following fields:
    //
    // `type`: The note type (e.g., `r` for rest, `s` for slash notes, etc.)
    // `dots`: The number of dots, which affects the duration.
    // `duration`: The time length (e.g., `q` for quarter, `h` for half, `8` for eighth etc.)
    //
    // The range of values for these parameters are available in `src/tables.js`.

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'note';
    }
  }, {
    key: 'STAVEPADDING',
    get: function get() {
      return 12;
    }
  }]);

  function Note(note_struct) {
    _classCallCheck(this, Note);

    var _this = _possibleConstructorReturn(this, (Note.__proto__ || Object.getPrototypeOf(Note)).call(this));

    _this.setAttribute('type', 'Note');

    if (!note_struct) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Note must have valid initialization data to identify duration and type.');
    }

    // Parse `note_struct` and get note properties.
    var initData = _tables.Flow.parseNoteData(note_struct);
    if (!initData) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Invalid note initialization object: ' + JSON.stringify(note_struct));
    }

    // Set note properties from parameters.
    _this.duration = initData.duration;
    _this.dots = initData.dots;
    _this.noteType = initData.type;

    if (note_struct.duration_override) {
      // Custom duration
      _this.setDuration(note_struct.duration_override);
    } else {
      // Default duration
      _this.setIntrinsicTicks(initData.ticks);
    }

    _this.modifiers = [];

    // Get the glyph code for this note from the font.
    _this.glyph = _tables.Flow.durationToGlyph(_this.duration, _this.noteType);

    if (_this.positions && (_typeof(_this.positions) !== 'object' || !_this.positions.length)) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Note keys must be array type.');
    }

    // Note to play for audio players.
    _this.playNote = null;

    // Positioning contexts used by the Formatter.
    _this.tickContext = null; // The current tick context.
    _this.modifierContext = null;
    _this.ignore_ticks = false;

    // Positioning variables
    _this.width = 0; // Width in pixels calculated after preFormat
    _this.extraLeftPx = 0; // Extra room on left for offset note head
    _this.extraRightPx = 0; // Extra room on right for offset note head
    _this.x_shift = 0; // X shift from tick context X
    _this.left_modPx = 0; // Max width of left modifiers
    _this.right_modPx = 0; // Max width of right modifiers
    _this.voice = null; // The voice that this note is in
    _this.preFormatted = false; // Is this note preFormatted?
    _this.ys = []; // list of y coordinates for each note
    // we need to hold on to these for ties and beams.

    if (note_struct.align_center) {
      _this.setCenterAlignment(note_struct.align_center);
    }

    // The render surface.
    _this.stave = null;
    _this.render_options = {
      annotation_spacing: 5,
      stave_padding: Note.STAVEPADDING
    };
    return _this;
  }

  // Get and set the play note, which is arbitrary data that can be used by an
  // audio player.


  _createClass(Note, [{
    key: 'getPlayNote',
    value: function getPlayNote() {
      return this.playNote;
    }
  }, {
    key: 'setPlayNote',
    value: function setPlayNote(note) {
      this.playNote = note;return this;
    }

    // Don't play notes by default, call them rests. This is also used by things like
    // beams and dots for positioning.

  }, {
    key: 'isRest',
    value: function isRest() {
      return false;
    }

    // TODO(0xfe): Why is this method here?

  }, {
    key: 'addStroke',
    value: function addStroke(index, stroke) {
      stroke.setNote(this);
      stroke.setIndex(index);
      this.modifiers.push(stroke);
      this.setPreFormatted(false);
      return this;
    }

    // Get and set the target stave.

  }, {
    key: 'getStave',
    value: function getStave() {
      return this.stave;
    }
  }, {
    key: 'setStave',
    value: function setStave(stave) {
      this.stave = stave;
      this.setYs([stave.getYForLine(0)]); // Update Y values if the stave is changed.
      this.context = this.stave.context;
      return this;
    }

    // `Note` is not really a modifier, but is used in
    // a `ModifierContext`.

  }, {
    key: 'getCategory',
    value: function getCategory() {
      return Note.CATEGORY;
    }

    // Set the rendering context for the note.

  }, {
    key: 'setContext',
    value: function setContext(context) {
      this.context = context;return this;
    }

    // Get and set spacing to the left and right of the notes.

  }, {
    key: 'getExtraLeftPx',
    value: function getExtraLeftPx() {
      return this.extraLeftPx;
    }
  }, {
    key: 'getExtraRightPx',
    value: function getExtraRightPx() {
      return this.extraRightPx;
    }
  }, {
    key: 'setExtraLeftPx',
    value: function setExtraLeftPx(x) {
      this.extraLeftPx = x;return this;
    }
  }, {
    key: 'setExtraRightPx',
    value: function setExtraRightPx(x) {
      this.extraRightPx = x;return this;
    }

    // Returns true if this note has no duration (e.g., bar notes, spacers, etc.)

  }, {
    key: 'shouldIgnoreTicks',
    value: function shouldIgnoreTicks() {
      return this.ignore_ticks;
    }

    // Get the stave line number for the note.

  }, {
    key: 'getLineNumber',
    value: function getLineNumber() {
      return 0;
    }

    // Get the stave line number for rest.

  }, {
    key: 'getLineForRest',
    value: function getLineForRest() {
      return 0;
    }

    // Get the glyph associated with this note.

  }, {
    key: 'getGlyph',
    value: function getGlyph() {
      return this.glyph;
    }
  }, {
    key: 'getGlyphWidth',
    value: function getGlyphWidth() {
      return this.glyph.getWidth(this.render_options.glyph_font_scale);
    }

    // Set and get Y positions for this note. Each Y value is associated with
    // an individual pitch/key within the note/chord.

  }, {
    key: 'setYs',
    value: function setYs(ys) {
      this.ys = ys;return this;
    }
  }, {
    key: 'getYs',
    value: function getYs() {
      if (this.ys.length === 0) {
        throw new _vex.Vex.RERR('NoYValues', 'No Y-values calculated for this note.');
      }

      return this.ys;
    }

    // Get the Y position of the space above the stave onto which text can
    // be rendered.

  }, {
    key: 'getYForTopText',
    value: function getYForTopText(text_line) {
      if (!this.stave) {
        throw new _vex.Vex.RERR('NoStave', 'No stave attached to this note.');
      }

      return this.stave.getYForTopText(text_line);
    }

    // Get a `BoundingBox` for this note.

  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      return null;
    }

    // Returns the voice that this note belongs in.

  }, {
    key: 'getVoice',
    value: function getVoice() {
      if (!this.voice) throw new _vex.Vex.RERR('NoVoice', 'Note has no voice.');
      return this.voice;
    }

    // Attach this note to `voice`.

  }, {
    key: 'setVoice',
    value: function setVoice(voice) {
      this.voice = voice;
      this.preFormatted = false;
      return this;
    }

    // Get and set the `TickContext` for this note.

  }, {
    key: 'getTickContext',
    value: function getTickContext() {
      return this.tickContext;
    }
  }, {
    key: 'setTickContext',
    value: function setTickContext(tc) {
      this.tickContext = tc;
      this.preFormatted = false;
      return this;
    }

    // Accessors for the note type.

  }, {
    key: 'getDuration',
    value: function getDuration() {
      return this.duration;
    }
  }, {
    key: 'isDotted',
    value: function isDotted() {
      return this.dots > 0;
    }
  }, {
    key: 'hasStem',
    value: function hasStem() {
      return false;
    }
  }, {
    key: 'getDots',
    value: function getDots() {
      return this.dots;
    }
  }, {
    key: 'getNoteType',
    value: function getNoteType() {
      return this.noteType;
    }
  }, {
    key: 'setBeam',
    value: function setBeam() {
      return this;
    } // ignore parameters

    // Attach this note to a modifier context.

  }, {
    key: 'setModifierContext',
    value: function setModifierContext(mc) {
      this.modifierContext = mc;return this;
    }

    // Attach a modifier to this note.

  }, {
    key: 'addModifier',
    value: function addModifier(modifier) {
      var index = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;

      modifier.setNote(this);
      modifier.setIndex(index);
      this.modifiers.push(modifier);
      this.setPreFormatted(false);
      return this;
    }

    // Get the coordinates for where modifiers begin.

  }, {
    key: 'getModifierStartXY',
    value: function getModifierStartXY() {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call GetModifierStartXY on an unformatted note");
      }

      return {
        x: this.getAbsoluteX(),
        y: this.ys[0]
      };
    }

    // Get bounds and metrics for this note.
    //
    // Returns a struct with fields:
    // `width`: The total width of the note (including modifiers.)
    // `noteWidth`: The width of the note head only.
    // `left_shift`: The horizontal displacement of the note.
    // `modLeftPx`: Start `X` for left modifiers.
    // `modRightPx`: Start `X` for right modifiers.
    // `extraLeftPx`: Extra space on left of note.
    // `extraRightPx`: Extra space on right of note.

  }, {
    key: 'getMetrics',
    value: function getMetrics() {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call getMetrics on an unformatted note.");
      }

      var modLeftPx = 0;
      var modRightPx = 0;
      if (this.modifierContext != null) {
        modLeftPx = this.modifierContext.state.left_shift;
        modRightPx = this.modifierContext.state.right_shift;
      }

      var width = this.getWidth();
      return {
        width: width,
        noteWidth: width - modLeftPx - modRightPx - this.extraLeftPx - this.extraRightPx,
        left_shift: this.x_shift, // TODO(0xfe): Make style consistent

        // Modifiers, accidentals etc.
        modLeftPx: modLeftPx,
        modRightPx: modRightPx,

        // Displaced note head on left or right.
        extraLeftPx: this.extraLeftPx,
        extraRightPx: this.extraRightPx
      };
    }

    // Get and set width of note. Used by the formatter for positioning.

  }, {
    key: 'setWidth',
    value: function setWidth(width) {
      this.width = width;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call GetWidth on an unformatted note.");
      }

      return this.width + (this.modifierContext ? this.modifierContext.getWidth() : 0);
    }

    // Displace note by `x` pixels. Used by the formatter.

  }, {
    key: 'setXShift',
    value: function setXShift(x) {
      this.x_shift = x;return this;
    }
  }, {
    key: 'getXShift',
    value: function getXShift() {
      return this.x_shift;
    }

    // Get `X` position of this tick context.

  }, {
    key: 'getX',
    value: function getX() {
      if (!this.tickContext) {
        throw new _vex.Vex.RERR('NoTickContext', 'Note needs a TickContext assigned for an X-Value');
      }

      return this.tickContext.getX() + this.x_shift;
    }

    // Get the absolute `X` position of this note's tick context. This
    // excludes x_shift, so you'll need to factor it in if you're
    // looking for the post-formatted x-position.

  }, {
    key: 'getAbsoluteX',
    value: function getAbsoluteX() {
      if (!this.tickContext) {
        throw new _vex.Vex.RERR('NoTickContext', 'Note needs a TickContext assigned for an X-Value');
      }

      // Position note to left edge of tick context.
      var x = this.tickContext.getX();
      if (this.stave) {
        x += this.stave.getNoteStartX() + this.render_options.stave_padding;
      }

      if (this.isCenterAligned()) {
        x += this.getCenterXShift();
      }

      return x;
    }
  }, {
    key: 'setPreFormatted',
    value: function setPreFormatted(value) {
      this.preFormatted = value;

      // Maintain the width of left and right modifiers in pixels.
      if (this.preFormatted) {
        var extra = this.tickContext.getExtraPx();
        this.left_modPx = Math.max(this.left_modPx, extra.left);
        this.right_modPx = Math.max(this.right_modPx, extra.right);
      }
    }
  }]);

  return Note;
}(_tickable.Tickable);

/***/ }),
/* 7 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveModifier = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _element = __webpack_require__(3);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// A base class for stave modifiers (e.g. clefs, key signatures)

var StaveModifier = exports.StaveModifier = function (_Element) {
  _inherits(StaveModifier, _Element);

  _createClass(StaveModifier, null, [{
    key: 'Position',
    get: function get() {
      return {
        LEFT: 1,
        RIGHT: 2,
        ABOVE: 3,
        BELOW: 4,
        BEGIN: 5,
        END: 6
      };
    }
  }]);

  function StaveModifier() {
    _classCallCheck(this, StaveModifier);

    var _this = _possibleConstructorReturn(this, (StaveModifier.__proto__ || Object.getPrototypeOf(StaveModifier)).call(this));

    _this.setAttribute('type', 'StaveModifier');

    _this.padding = 10;
    _this.position = StaveModifier.Position.ABOVE;
    return _this;
  }

  _createClass(StaveModifier, [{
    key: 'getPosition',
    value: function getPosition() {
      return this.position;
    }
  }, {
    key: 'setPosition',
    value: function setPosition(position) {
      this.position = position;return this;
    }
  }, {
    key: 'getStave',
    value: function getStave() {
      return this.stave;
    }
  }, {
    key: 'setStave',
    value: function setStave(stave) {
      this.stave = stave;return this;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'setWidth',
    value: function setWidth(width) {
      this.width = width;return this;
    }
  }, {
    key: 'getX',
    value: function getX() {
      return this.x;
    }
  }, {
    key: 'setX',
    value: function setX(x) {
      this.x = x;return this;
    }
  }, {
    key: 'getCategory',
    value: function getCategory() {
      return '';
    }
  }, {
    key: 'makeSpacer',
    value: function makeSpacer(padding) {
      // TODO(0xfe): Return an instance of type `Spacer` based on `GhostNote`
      // instead of this hack.

      return {
        getContext: function getContext() {
          return true;
        },
        setStave: function setStave() {},
        renderToStave: function renderToStave() {},
        getMetrics: function getMetrics() {
          return { width: padding };
        }
      };
    }
  }, {
    key: 'placeGlyphOnLine',
    value: function placeGlyphOnLine(glyph, stave, line) {
      glyph.setYShift(stave.getYForLine(line) - stave.getYForGlyphs());
    }
  }, {
    key: 'getPadding',
    value: function getPadding(index) {
      return index !== undefined && index < 2 ? 0 : this.padding;
    }
  }, {
    key: 'setPadding',
    value: function setPadding(padding) {
      this.padding = padding;return this;
    }
  }]);

  return StaveModifier;
}(_element.Element);

/***/ }),
/* 8 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Fraction = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// Fraction class that represents a rational number
//
// @author zz85
// @author incompleteopus (modifications)

/* eslint-disable no-underscore-dangle */

var _vex = __webpack_require__(0);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Fraction = function () {
  _createClass(Fraction, null, [{
    key: 'GCD',


    /**
     * GCD: Find greatest common divisor using Euclidean algorithm
     */
    value: function GCD(a, b) {
      if (typeof a !== 'number' || typeof b !== 'number') {
        throw new _vex.Vex.RERR('BadArgument', 'Invalid numbers: ' + a + ', ' + b);
      }

      var t = void 0;

      while (b !== 0) {
        t = b;
        b = a % b;
        a = t;
      }

      return a;
    }

    /**
     * LCM: Lowest common multiple
     */

  }, {
    key: 'LCM',
    value: function LCM(a, b) {
      return a * b / Fraction.GCD(a, b);
    }

    /**
     * LCMM: Lowest common multiple for more than two numbers
     */

  }, {
    key: 'LCMM',
    value: function LCMM(args) {
      if (args.length === 0) {
        return 0;
      } else if (args.length === 1) {
        return args[0];
      } else if (args.length === 2) {
        return Fraction.LCM(args[0], args[1]);
      } else {
        var arg0 = args[0];
        args.shift();
        return Fraction.LCM(arg0, Fraction.LCMM(args));
      }
    }
  }]);

  function Fraction(numerator, denominator) {
    _classCallCheck(this, Fraction);

    this.set(numerator, denominator);
  }

  _createClass(Fraction, [{
    key: 'set',
    value: function set(numerator, denominator) {
      this.numerator = numerator === undefined ? 1 : numerator;
      this.denominator = denominator === undefined ? 1 : denominator;
      return this;
    }
  }, {
    key: 'value',
    value: function value() {
      return this.numerator / this.denominator;
    }
  }, {
    key: 'simplify',
    value: function simplify() {
      var u = this.numerator;
      var d = this.denominator;

      var gcd = Fraction.GCD(u, d);
      u /= gcd;
      d /= gcd;

      if (d < 0) {
        d = -d;
        u = -u;
      }
      return this.set(u, d);
    }
  }, {
    key: 'add',
    value: function add(param1, param2) {
      var otherNumerator = void 0;
      var otherDenominator = void 0;

      if (param1 instanceof Fraction) {
        otherNumerator = param1.numerator;
        otherDenominator = param1.denominator;
      } else {
        if (param1 !== undefined) {
          otherNumerator = param1;
        } else {
          otherNumerator = 0;
        }

        if (param2 !== undefined) {
          otherDenominator = param2;
        } else {
          otherDenominator = 1;
        }
      }

      var lcm = Fraction.LCM(this.denominator, otherDenominator);
      var a = lcm / this.denominator;
      var b = lcm / otherDenominator;

      var u = this.numerator * a + otherNumerator * b;
      return this.set(u, lcm);
    }
  }, {
    key: 'subtract',
    value: function subtract(param1, param2) {
      var otherNumerator = void 0;
      var otherDenominator = void 0;

      if (param1 instanceof Fraction) {
        otherNumerator = param1.numerator;
        otherDenominator = param1.denominator;
      } else {
        if (param1 !== undefined) {
          otherNumerator = param1;
        } else {
          otherNumerator = 0;
        }

        if (param2 !== undefined) {
          otherDenominator = param2;
        } else {
          otherDenominator = 1;
        }
      }

      var lcm = Fraction.LCM(this.denominator, otherDenominator);
      var a = lcm / this.denominator;
      var b = lcm / otherDenominator;

      var u = this.numerator * a - otherNumerator * b;
      return this.set(u, lcm);
    }
  }, {
    key: 'multiply',
    value: function multiply(param1, param2) {
      var otherNumerator = void 0;
      var otherDenominator = void 0;

      if (param1 instanceof Fraction) {
        otherNumerator = param1.numerator;
        otherDenominator = param1.denominator;
      } else {
        if (param1 !== undefined) {
          otherNumerator = param1;
        } else {
          otherNumerator = 1;
        }

        if (param2 !== undefined) {
          otherDenominator = param2;
        } else {
          otherDenominator = 1;
        }
      }

      return this.set(this.numerator * otherNumerator, this.denominator * otherDenominator);
    }
  }, {
    key: 'divide',
    value: function divide(param1, param2) {
      var otherNumerator = void 0;
      var otherDenominator = void 0;

      if (param1 instanceof Fraction) {
        otherNumerator = param1.numerator;
        otherDenominator = param1.denominator;
      } else {
        if (param1 !== undefined) {
          otherNumerator = param1;
        } else {
          otherNumerator = 1;
        }

        if (param2 !== undefined) {
          otherDenominator = param2;
        } else {
          otherDenominator = 1;
        }
      }

      return this.set(this.numerator * otherDenominator, this.denominator * otherNumerator);
    }

    // Simplifies both sides and checks if they are equal.

  }, {
    key: 'equals',
    value: function equals(compare) {
      var a = Fraction.__compareA.copy(compare).simplify();
      var b = Fraction.__compareB.copy(this).simplify();

      return a.numerator === b.numerator && a.denominator === b.denominator;
    }

    // Greater than operator.

  }, {
    key: 'greaterThan',
    value: function greaterThan(compare) {
      var a = Fraction.__compareB.copy(this);
      a.subtract(compare);
      return a.numerator > 0;
    }

    // Greater than or equals operator.

  }, {
    key: 'greaterThanEquals',
    value: function greaterThanEquals(compare) {
      var a = Fraction.__compareB.copy(this);
      a.subtract(compare);
      return a.numerator >= 0;
    }

    // Less than operator.

  }, {
    key: 'lessThan',
    value: function lessThan(compare) {
      return !this.greaterThanEquals(compare);
    }

    // Less than or equals operator.

  }, {
    key: 'lessThanEquals',
    value: function lessThanEquals(compare) {
      return !this.greaterThan(compare);
    }

    // Creates a new copy with this current values.

  }, {
    key: 'clone',
    value: function clone() {
      return new Fraction(this.numerator, this.denominator);
    }

    // Copies value of another Fraction into itself.

  }, {
    key: 'copy',
    value: function copy(_copy) {
      return this.set(_copy.numerator, _copy.denominator);
    }

    // Returns the integer component eg. (4/2) == 2

  }, {
    key: 'quotient',
    value: function quotient() {
      return Math.floor(this.numerator / this.denominator);
    }

    // Returns the fraction component when reduced to a mixed number

  }, {
    key: 'fraction',
    value: function fraction() {
      return this.numerator % this.denominator;
    }

    // Returns the absolute value

  }, {
    key: 'abs',
    value: function abs() {
      this.denominator = Math.abs(this.denominator);
      this.numerator = Math.abs(this.numerator);
      return this;
    }

    // Returns a raw string representation

  }, {
    key: 'toString',
    value: function toString() {
      return this.numerator + '/' + this.denominator;
    }

    // Returns a simplified string respresentation

  }, {
    key: 'toSimplifiedString',
    value: function toSimplifiedString() {
      return Fraction.__tmp.copy(this).simplify().toString();
    }

    // Returns string representation in mixed form

  }, {
    key: 'toMixedString',
    value: function toMixedString() {
      var s = '';
      var q = this.quotient();
      var f = Fraction.__tmp.copy(this);

      if (q < 0) {
        f.abs().fraction();
      } else {
        f.fraction();
      }

      if (q !== 0) {
        s += q;

        if (f.numerator !== 0) {
          s += ' ' + f.toSimplifiedString();
        }
      } else {
        if (f.numerator === 0) {
          s = '0';
        } else {
          s = f.toSimplifiedString();
        }
      }

      return s;
    }

    // Parses a fraction string

  }, {
    key: 'parse',
    value: function parse(str) {
      var i = str.split('/');
      var n = parseInt(i[0], 10);
      var d = i[1] ? parseInt(i[1], 10) : 1;

      return this.set(n, d);
    }
  }]);

  return Fraction;
}();

// Temporary cached objects


exports.Fraction = Fraction;
Fraction.__compareA = new Fraction();
Fraction.__compareB = new Fraction();
Fraction.__tmp = new Fraction();

/***/ }),
/* 9 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Stem = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _tables = __webpack_require__(1);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This file implements the `Stem` object. Generally this object is handled
// by its parent `StemmableNote`.

// To enable logging for this class. Set `Vex.Flow.Stem.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Stem.DEBUG) _vex.Vex.L('Vex.Flow.Stem', args);
}

var Stem = exports.Stem = function (_Element) {
  _inherits(Stem, _Element);

  _createClass(Stem, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'stem';
    }

    // Stem directions

  }, {
    key: 'UP',
    get: function get() {
      return 1;
    }
  }, {
    key: 'DOWN',
    get: function get() {
      return -1;
    }

    // Theme

  }, {
    key: 'WIDTH',
    get: function get() {
      return _tables.Flow.STEM_WIDTH;
    }
  }, {
    key: 'HEIGHT',
    get: function get() {
      return _tables.Flow.STEM_HEIGHT;
    }
  }]);

  function Stem() {
    var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

    _classCallCheck(this, Stem);

    var _this = _possibleConstructorReturn(this, (Stem.__proto__ || Object.getPrototypeOf(Stem)).call(this));

    _this.setAttribute('type', 'Stem');

    // Default notehead x bounds
    _this.x_begin = options.x_begin || 0;
    _this.x_end = options.x_end || 0;

    // Y bounds for top/bottom most notehead
    _this.y_top = options.y_top || 0;
    _this.y_bottom = options.y_bottom || 0;

    // Stem top extension
    _this.stem_extension = options.stem_extension || 0;

    // Direction of the stem
    _this.stem_direction = options.stem_direction || 0;

    // Flag to override all draw calls
    _this.hide = options.hide || false;

    _this.isStemlet = options.isStemlet || false;
    _this.stemletHeight = options.stemletHeight || 0;

    // Use to adjust the rendered height without affecting
    // the results of `.getExtents()`
    _this.renderHeightAdjustment = 0;
    return _this;
  }

  // Set the x bounds for the default notehead


  _createClass(Stem, [{
    key: 'setNoteHeadXBounds',
    value: function setNoteHeadXBounds(x_begin, x_end) {
      this.x_begin = x_begin;
      this.x_end = x_end;
      return this;
    }

    // Set the direction of the stem in relation to the noteheads

  }, {
    key: 'setDirection',
    value: function setDirection(direction) {
      this.stem_direction = direction;
    }

    // Set the extension for the stem, generally for flags or beams

  }, {
    key: 'setExtension',
    value: function setExtension(ext) {
      this.stem_extension = ext;
    }
  }, {
    key: 'getExtension',
    value: function getExtension() {
      return this.stem_extension;
    }

    // The the y bounds for the top and bottom noteheads

  }, {
    key: 'setYBounds',
    value: function setYBounds(y_top, y_bottom) {
      this.y_top = y_top;
      this.y_bottom = y_bottom;
    }

    // The category of the object

  }, {
    key: 'getCategory',
    value: function getCategory() {
      return Stem.CATEGORY;
    }

    // Gets the entire height for the stem

  }, {
    key: 'getHeight',
    value: function getHeight() {
      return (this.y_bottom - this.y_top) * this.stem_direction + (Stem.HEIGHT + this.stem_extension) * this.stem_direction;
    }
  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      throw new _vex.Vex.RERR('NotImplemented', 'getBoundingBox() not implemented.');
    }

    // Get the y coordinates for the very base of the stem to the top of
    // the extension

  }, {
    key: 'getExtents',
    value: function getExtents() {
      var isStemUp = this.stem_direction === Stem.UP;
      var ys = [this.y_top, this.y_bottom];
      var stemHeight = Stem.HEIGHT + this.stem_extension;
      var innerMostNoteheadY = (isStemUp ? Math.min : Math.max).apply(undefined, ys);
      var outerMostNoteheadY = (isStemUp ? Math.max : Math.min).apply(undefined, ys);
      var stemTipY = innerMostNoteheadY + stemHeight * -this.stem_direction;

      return { topY: stemTipY, baseY: outerMostNoteheadY };
    }
  }, {
    key: 'setVisibility',
    value: function setVisibility(isVisible) {
      this.hide = !isVisible;
      return this;
    }
  }, {
    key: 'setStemlet',
    value: function setStemlet(isStemlet, stemletHeight) {
      this.isStemlet = isStemlet;
      this.stemletHeight = stemletHeight;
      return this;
    }

    // Render the stem onto the canvas

  }, {
    key: 'draw',
    value: function draw() {
      this.setRendered();
      if (this.hide) return;
      var ctx = this.checkContext();

      var stem_x = void 0;
      var stem_y = void 0;
      var stem_direction = this.stem_direction;

      if (stem_direction === Stem.DOWN) {
        // Down stems are rendered to the left of the head.
        stem_x = this.x_begin;
        stem_y = this.y_top;
      } else {
        // Up stems are rendered to the right of the head.
        stem_x = this.x_end;
        stem_y = this.y_bottom;
      }

      var stemHeight = this.getHeight();

      L('Rendering stem - ', 'Top Y: ', this.y_top, 'Bottom Y: ', this.y_bottom);

      // The offset from the stem's base which is required fo satisfy the stemlet height
      var stemletYOffset = this.isStemlet ? stemHeight - this.stemletHeight * this.stem_direction : 0;

      // Draw the stem
      ctx.save();
      this.applyStyle(ctx);
      ctx.beginPath();
      ctx.setLineWidth(Stem.WIDTH);
      ctx.moveTo(stem_x, stem_y - stemletYOffset);
      ctx.lineTo(stem_x, stem_y - stemHeight - this.renderHeightAdjustment * stem_direction);
      ctx.stroke();
      this.restoreStyle(ctx);
      ctx.restore();
    }
  }]);

  return Stem;
}(_element.Element);

/***/ }),
/* 10 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// Vex Music Notation
// Mohit Muthanna <mohit@muthanna.com>
//
// Copyright Mohit Muthanna 2010

// Bounding boxes for interactive notation

var BoundingBox = exports.BoundingBox = function () {
  _createClass(BoundingBox, null, [{
    key: "copy",
    value: function copy(that) {
      return new BoundingBox(that.x, that.y, that.w, that.h);
    }
  }]);

  function BoundingBox(x, y, w, h) {
    _classCallCheck(this, BoundingBox);

    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }

  _createClass(BoundingBox, [{
    key: "getX",
    value: function getX() {
      return this.x;
    }
  }, {
    key: "getY",
    value: function getY() {
      return this.y;
    }
  }, {
    key: "getW",
    value: function getW() {
      return this.w;
    }
  }, {
    key: "getH",
    value: function getH() {
      return this.h;
    }
  }, {
    key: "setX",
    value: function setX(x) {
      this.x = x;return this;
    }
  }, {
    key: "setY",
    value: function setY(y) {
      this.y = y;return this;
    }
  }, {
    key: "setW",
    value: function setW(w) {
      this.w = w;return this;
    }
  }, {
    key: "setH",
    value: function setH(h) {
      this.h = h;return this;
    }
  }, {
    key: "move",
    value: function move(x, y) {
      this.x += x;this.y += y;
    }
  }, {
    key: "clone",
    value: function clone() {
      return BoundingBox.copy(this);
    }

    // Merge my box with given box. Creates a bigger bounding box unless
    // the given box is contained in this one.

  }, {
    key: "mergeWith",
    value: function mergeWith(boundingBox, ctx) {
      var that = boundingBox;

      var new_x = this.x < that.x ? this.x : that.x;
      var new_y = this.y < that.y ? this.y : that.y;
      var new_w = Math.max(this.x + this.w, that.x + that.w) - new_x;
      var new_h = Math.max(this.y + this.h, that.y + that.h) - new_y;

      this.x = new_x;
      this.y = new_y;
      this.w = new_w;
      this.h = new_h;

      if (ctx) this.draw(ctx);
      return this;
    }
  }, {
    key: "draw",
    value: function draw(ctx, x, y) {
      if (!x) x = 0;
      if (!y) y = 0;
      ctx.rect(this.x + x, this.y + y, this.w, this.h);
      ctx.stroke();
    }
  }]);

  return BoundingBox;
}();

/***/ }),
/* 11 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Formatter = undefined;

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements the formatting and layout algorithms that are used
// to position notes in a voice. The algorithm can align multiple voices both
// within a stave, and across multiple staves.
//
// To do this, the formatter breaks up voices into a grid of rational-valued
// `ticks`, to which each note is assigned. Then, minimum widths are assigned
// to each tick based on the widths of the notes and modifiers in that tick. This
// establishes the smallest amount of space required for each tick.
//
// Finally, the formatter distributes the left over space proportionally to
// all the ticks, setting the `x` values of the notes in each tick.
//
// See `tests/formatter_tests.js` for usage examples. The helper functions included
// here (`FormatAndDraw`, `FormatAndDrawTab`) also serve as useful usage examples.

var _vex = __webpack_require__(0);

var _beam = __webpack_require__(15);

var _tables = __webpack_require__(1);

var _fraction = __webpack_require__(8);

var _voice = __webpack_require__(12);

var _staveconnector = __webpack_require__(19);

var _stavenote = __webpack_require__(5);

var _note = __webpack_require__(6);

var _modifiercontext = __webpack_require__(22);

var _tickcontext = __webpack_require__(13);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// To enable logging for this class. Set `Vex.Flow.Formatter.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Formatter.DEBUG) _vex.Vex.L('Vex.Flow.Formatter', args);
}

// Helper function to locate the next non-rest note(s).
function lookAhead(notes, restLine, i, compare) {
  // If no valid next note group, nextRestLine is same as current.
  var nextRestLine = restLine;

  // Get the rest line for next valid non-rest note group.
  for (i += 1; i < notes.length; i += 1) {
    var note = notes[i];
    if (!note.isRest() && !note.shouldIgnoreTicks()) {
      nextRestLine = note.getLineForRest();
      break;
    }
  }

  // Locate the mid point between two lines.
  if (compare && restLine !== nextRestLine) {
    var top = Math.max(restLine, nextRestLine);
    var bot = Math.min(restLine, nextRestLine);
    nextRestLine = _vex.Vex.MidLine(top, bot);
  }
  return nextRestLine;
}

// Take an array of `voices` and place aligned tickables in the same context. Returns
// a mapping from `tick` to `ContextType`, a list of `tick`s, and the resolution
// multiplier.
//
// Params:
// * `voices`: Array of `Voice` instances.
// * `ContextType`: A context class (e.g., `ModifierContext`, `TickContext`)
// * `addToContext`: Function to add tickable to context.
function createContexts(voices, ContextType, addToContext) {
  if (!voices || !voices.length) {
    throw new _vex.Vex.RERR('BadArgument', 'No voices to format');
  }

  // Find out highest common multiple of resolution multipliers.
  // The purpose of this is to find out a common denominator
  // for all fractional tick values in all tickables of all voices,
  // so that the values can be expanded and the numerator used
  // as an integer tick value.
  var totalTicks = voices[0].getTotalTicks();
  var resolutionMultiplier = voices.reduce(function (resolutionMultiplier, voice) {
    if (!voice.getTotalTicks().equals(totalTicks)) {
      throw new _vex.Vex.RERR('TickMismatch', 'Voices should have same total note duration in ticks.');
    }

    if (voice.getMode() === _voice.Voice.Mode.STRICT && !voice.isComplete()) {
      throw new _vex.Vex.RERR('IncompleteVoice', 'Voice does not have enough notes.');
    }

    return Math.max(resolutionMultiplier, _fraction.Fraction.LCM(resolutionMultiplier, voice.getResolutionMultiplier()));
  }, 1);

  // Initialize tick maps.
  var tickToContextMap = {};
  var tickList = [];
  var contexts = [];

  // For each voice, extract notes and create a context for every
  // new tick that hasn't been seen before.
  voices.forEach(function (voice) {
    // Use resolution multiplier as denominator to expand ticks
    // to suitable integer values, so that no additional expansion
    // of fractional tick values is needed.
    var ticksUsed = new _fraction.Fraction(0, resolutionMultiplier);

    voice.getTickables().forEach(function (tickable) {
      var integerTicks = ticksUsed.numerator;

      // If we have no tick context for this tick, create one.
      if (!tickToContextMap[integerTicks]) {
        var newContext = new ContextType();
        contexts.push(newContext);
        tickToContextMap[integerTicks] = newContext;
      }

      // Add this tickable to the TickContext.
      addToContext(tickable, tickToContextMap[integerTicks]);

      // Maintain a sorted list of tick contexts.
      tickList.push(integerTicks);
      ticksUsed.add(tickable.getTicks());
    });
  });

  return {
    map: tickToContextMap,
    array: contexts,
    list: _vex.Vex.SortAndUnique(tickList, function (a, b) {
      return a - b;
    }, function (a, b) {
      return a === b;
    }),
    resolutionMultiplier: resolutionMultiplier
  };
}

var Formatter = exports.Formatter = function () {
  _createClass(Formatter, null, [{
    key: 'SimpleFormat',

    // Helper function to layout "notes" one after the other without
    // regard for proportions. Useful for tests and debugging.
    value: function SimpleFormat(notes) {
      var x = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;

      var _ref = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {},
          _ref$paddingBetween = _ref.paddingBetween,
          paddingBetween = _ref$paddingBetween === undefined ? 10 : _ref$paddingBetween;

      notes.reduce(function (x, note) {
        note.addToModifierContext(new _modifiercontext.ModifierContext());
        var tick = new _tickcontext.TickContext().addTickable(note).preFormat();
        var extra = tick.getExtraPx();
        tick.setX(x + extra.left);

        return x + tick.getWidth() + extra.right + paddingBetween;
      }, x);
    }

    // Helper function to plot formatter debug info.

  }, {
    key: 'plotDebugging',
    value: function plotDebugging(ctx, formatter, xPos, y1, y2) {
      var x = xPos + _note.Note.STAVEPADDING;
      var contextGaps = formatter.contextGaps;
      function stroke(x1, x2, color) {
        ctx.beginPath();
        ctx.setStrokeStyle(color);
        ctx.setFillStyle(color);
        ctx.setLineWidth(1);
        ctx.fillRect(x1, y1, x2 - x1, y2 - y1);
      }

      ctx.save();
      ctx.setFont('Arial', 8, '');

      contextGaps.gaps.forEach(function (gap) {
        stroke(x + gap.x1, x + gap.x2, '#aaa');
        // Vex.drawDot(ctx, xPos + gap.x1, yPos, 'blue');
        ctx.fillText(Math.round(gap.x2 - gap.x1), x + gap.x1, y2 + 12);
      });

      ctx.fillText(Math.round(contextGaps.total) + 'px', x - 20, y2 + 12);
      ctx.setFillStyle('red');

      ctx.fillText('Loss: ' + formatter.lossHistory.map(function (loss) {
        return Math.round(loss);
      }), x - 20, y2 + 22);
      ctx.restore();
    }

    // Helper function to format and draw a single voice. Returns a bounding
    // box for the notation.
    //
    // Parameters:
    // * `ctx` - The rendering context
    // * `stave` - The stave to which to draw (`Stave` or `TabStave`)
    // * `notes` - Array of `Note` instances (`StaveNote`, `TextNote`, `TabNote`, etc.)
    // * `params` - One of below:
    //    * Setting `autobeam` only `(context, stave, notes, true)` or
    //      `(ctx, stave, notes, {autobeam: true})`
    //    * Setting `align_rests` a struct is needed `(context, stave, notes, {align_rests: true})`
    //    * Setting both a struct is needed `(context, stave, notes, {
    //      autobeam: true, align_rests: true})`
    //
    // `autobeam` automatically generates beams for the notes.
    // `align_rests` aligns rests with nearby notes.

  }, {
    key: 'FormatAndDraw',
    value: function FormatAndDraw(ctx, stave, notes, params) {
      var options = {
        auto_beam: false,
        align_rests: false
      };

      if ((typeof params === 'undefined' ? 'undefined' : _typeof(params)) === 'object') {
        _vex.Vex.Merge(options, params);
      } else if (typeof params === 'boolean') {
        options.auto_beam = params;
      }

      // Start by creating a voice and adding all the notes to it.
      var voice = new _voice.Voice(_tables.Flow.TIME4_4).setMode(_voice.Voice.Mode.SOFT).addTickables(notes);

      // Then create beams, if requested.
      var beams = options.auto_beam ? _beam.Beam.applyAndGetBeams(voice) : [];

      // Instantiate a `Formatter` and format the notes.
      new Formatter().joinVoices([voice], { align_rests: options.align_rests }).formatToStave([voice], stave, { align_rests: options.align_rests, stave: stave });

      // Render the voice and beams to the stave.
      voice.setStave(stave).draw(ctx, stave);
      beams.forEach(function (beam) {
        return beam.setContext(ctx).draw();
      });

      // Return the bounding box of the voice.
      return voice.getBoundingBox();
    }

    // Helper function to format and draw aligned tab and stave notes in two
    // separate staves.
    //
    // Parameters:
    // * `ctx` - The rendering context
    // * `tabstave` - A `TabStave` instance on which to render `TabNote`s.
    // * `stave` - A `Stave` instance on which to render `Note`s.
    // * `notes` - Array of `Note` instances for the stave (`StaveNote`, `BarNote`, etc.)
    // * `tabnotes` - Array of `Note` instances for the tab stave (`TabNote`, `BarNote`, etc.)
    // * `autobeam` - Automatically generate beams.
    // * `params` - A configuration object:
    //    * `autobeam` automatically generates beams for the notes.
    //    * `align_rests` aligns rests with nearby notes.

  }, {
    key: 'FormatAndDrawTab',
    value: function FormatAndDrawTab(ctx, tabstave, stave, tabnotes, notes, autobeam, params) {
      var opts = {
        auto_beam: autobeam,
        align_rests: false
      };

      if ((typeof params === 'undefined' ? 'undefined' : _typeof(params)) === 'object') {
        _vex.Vex.Merge(opts, params);
      } else if (typeof params === 'boolean') {
        opts.auto_beam = params;
      }

      // Create a `4/4` voice for `notes`.
      var notevoice = new _voice.Voice(_tables.Flow.TIME4_4).setMode(_voice.Voice.Mode.SOFT).addTickables(notes);

      // Create a `4/4` voice for `tabnotes`.
      var tabvoice = new _voice.Voice(_tables.Flow.TIME4_4).setMode(_voice.Voice.Mode.SOFT).addTickables(tabnotes);

      // Then create beams, if requested.
      var beams = opts.auto_beam ? _beam.Beam.applyAndGetBeams(notevoice) : [];

      // Instantiate a `Formatter` and align tab and stave notes.
      new Formatter().joinVoices([notevoice], { align_rests: opts.align_rests }).joinVoices([tabvoice]).formatToStave([notevoice, tabvoice], stave, { align_rests: opts.align_rests });

      // Render voices and beams to staves.
      notevoice.draw(ctx, stave);
      tabvoice.draw(ctx, tabstave);
      beams.forEach(function (beam) {
        return beam.setContext(ctx).draw();
      });

      // Draw a connector between tab and note staves.
      new _staveconnector.StaveConnector(stave, tabstave).setContext(ctx).draw();
    }

    // Auto position rests based on previous/next note positions.
    //
    // Params:
    // * `notes`: An array of notes.
    // * `alignAllNotes`: If set to false, only aligns non-beamed notes.
    // * `alignTuplets`: If set to false, ignores tuplets.

  }, {
    key: 'AlignRestsToNotes',
    value: function AlignRestsToNotes(notes, alignAllNotes, alignTuplets) {
      notes.forEach(function (note, index) {
        if (note instanceof _stavenote.StaveNote && note.isRest()) {
          if (note.tuplet && !alignTuplets) return;

          // If activated rests not on default can be rendered as specified.
          var position = note.getGlyph().position.toUpperCase();
          if (position !== 'R/4' && position !== 'B/4') return;

          if (alignAllNotes || note.beam != null) {
            // Align rests with previous/next notes.
            var props = note.getKeyProps()[0];
            if (index === 0) {
              props.line = lookAhead(notes, props.line, index, false);
              note.setKeyLine(0, props.line);
            } else if (index > 0 && index < notes.length) {
              // If previous note is a rest, use its line number.
              var restLine = void 0;
              if (notes[index - 1].isRest()) {
                restLine = notes[index - 1].getKeyProps()[0].line;
                props.line = restLine;
              } else {
                restLine = notes[index - 1].getLineForRest();
                // Get the rest line for next valid non-rest note group.
                props.line = lookAhead(notes, restLine, index, true);
              }
              note.setKeyLine(0, props.line);
            }
          }
        }
      });

      return this;
    }
  }]);

  function Formatter() {
    _classCallCheck(this, Formatter);

    // Minimum width required to render all the notes in the voices.
    this.minTotalWidth = 0;

    // This is set to `true` after `minTotalWidth` is calculated.
    this.hasMinTotalWidth = false;

    // Total number of ticks in the voice.
    this.totalTicks = new _fraction.Fraction(0, 1);

    // Arrays of tick and modifier contexts.
    this.tickContexts = null;
    this.modiferContexts = null;

    // Gaps between contexts, for free movement of notes post
    // formatting.
    this.contextGaps = {
      total: 0,
      gaps: []
    };

    this.voices = [];
  }

  // Find all the rests in each of the `voices` and align them
  // to neighboring notes. If `alignAllNotes` is `false`, then only
  // align non-beamed notes.


  _createClass(Formatter, [{
    key: 'alignRests',
    value: function alignRests(voices, alignAllNotes) {
      if (!voices || !voices.length) {
        throw new _vex.Vex.RERR('BadArgument', 'No voices to format rests');
      }

      voices.forEach(function (voice) {
        return Formatter.AlignRestsToNotes(voice.getTickables(), alignAllNotes);
      });
    }

    // Calculate the minimum width required to align and format `voices`.

  }, {
    key: 'preCalculateMinTotalWidth',
    value: function preCalculateMinTotalWidth(voices) {
      // Cache results.
      if (this.hasMinTotalWidth) return this.minTotalWidth;

      // Create tick contexts if not already created.
      if (!this.tickContexts) {
        if (!voices) {
          throw new _vex.Vex.RERR('BadArgument', "'voices' required to run preCalculateMinTotalWidth");
        }

        this.createTickContexts(voices);
      }

      var _tickContexts = this.tickContexts,
          contextList = _tickContexts.list,
          contextMap = _tickContexts.map;

      // Go through each tick context and calculate total width.

      this.minTotalWidth = contextList.map(function (tick) {
        var context = contextMap[tick];
        context.preFormat();
        return context.getWidth();
      }).reduce(function (a, b) {
        return a + b;
      }, 0);

      this.hasMinTotalWidth = true;

      return this.minTotalWidth;
    }

    // Get minimum width required to render all voices. Either `format` or
    // `preCalculateMinTotalWidth` must be called before this method.

  }, {
    key: 'getMinTotalWidth',
    value: function getMinTotalWidth() {
      if (!this.hasMinTotalWidth) {
        throw new _vex.Vex.RERR('NoMinTotalWidth', "Call 'preCalculateMinTotalWidth' or 'preFormat' before calling 'getMinTotalWidth'");
      }

      return this.minTotalWidth;
    }

    // Create `ModifierContext`s for each tick in `voices`.

  }, {
    key: 'createModifierContexts',
    value: function createModifierContexts(voices) {
      var contexts = createContexts(voices, _modifiercontext.ModifierContext, function (tickable, context) {
        return tickable.addToModifierContext(context);
      });

      this.modiferContexts = contexts;
      return contexts;
    }

    // Create `TickContext`s for each tick in `voices`. Also calculate the
    // total number of ticks in voices.

  }, {
    key: 'createTickContexts',
    value: function createTickContexts(voices) {
      var contexts = createContexts(voices, _tickcontext.TickContext, function (tickable, context) {
        return context.addTickable(tickable);
      });

      contexts.array.forEach(function (context) {
        context.tContexts = contexts.array;
      });

      this.totalTicks = voices[0].getTicksUsed().clone();
      this.tickContexts = contexts;
      return contexts;
    }

    // This is the core formatter logic. Format voices and justify them
    // to `justifyWidth` pixels. `renderingContext` is required to justify elements
    // that can't retreive widths without a canvas. This method sets the `x` positions
    // of all the tickables/notes in the formatter.

  }, {
    key: 'preFormat',
    value: function preFormat() {
      var justifyWidth = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
      var renderingContext = arguments[1];

      var _this = this;

      var voices = arguments[2];
      var stave = arguments[3];

      // Initialize context maps.
      var contexts = this.tickContexts;
      var contextList = contexts.list,
          contextMap = contexts.map,
          resolutionMultiplier = contexts.resolutionMultiplier;

      // If voices and a stave were provided, set the Stave for each voice
      // and preFormat to apply Y values to the notes;

      if (voices && stave) {
        voices.forEach(function (voice) {
          return voice.setStave(stave).preFormat();
        });
      }

      // Now distribute the ticks to each tick context, and assign them their
      // own X positions.
      var x = 0;
      var shift = 0;
      var centerX = justifyWidth / 2;
      this.minTotalWidth = 0;

      // Pass 1: Give each note maximum width requested by context.
      contextList.forEach(function (tick) {
        var context = contextMap[tick];
        if (renderingContext) context.setContext(renderingContext);

        // Make sure that all tickables in this context have calculated their
        // space requirements.
        context.preFormat();

        var width = context.getWidth();
        _this.minTotalWidth += width;

        var metrics = context.getMetrics();
        x = x + shift + metrics.extraLeftPx;
        context.setX(x);

        // Calculate shift for the next tick.
        shift = width - metrics.extraLeftPx;
      });

      this.minTotalWidth = x + shift;
      this.hasMinTotalWidth = true;

      // No justification needed. End formatting.
      if (justifyWidth <= 0) return;

      // Pass 2: Take leftover width, and distribute it to proportionately to
      // all notes.
      var remainingX = justifyWidth - this.minTotalWidth;
      var leftoverPxPerTick = remainingX / (this.totalTicks.value() * resolutionMultiplier);
      var spaceAccum = 0;

      contextList.forEach(function (tick, index) {
        var prevTick = contextList[index - 1] || 0;
        var context = contextMap[tick];
        var tickSpace = (tick - prevTick) * leftoverPxPerTick;

        spaceAccum += tickSpace;
        context.setX(context.getX() + spaceAccum);

        // Move center aligned tickables to middle
        context.getCenterAlignedTickables().forEach(function (tickable) {
          // eslint-disable-line
          tickable.center_x_shift = centerX - context.getX();
        });
      });

      // Just one context. Done formatting.
      if (contextList.length === 1) return;

      this.justifyWidth = justifyWidth;
      this.lossHistory = [];
      this.evaluate();
    }

    // Calculate the total cost of this formatting decision.

  }, {
    key: 'evaluate',
    value: function evaluate() {
      var _this2 = this;

      var justifyWidth = this.justifyWidth;
      // Calculate available slack per tick context. This works out how much freedom
      // to move a context has in either direction, without affecting other notes.
      this.contextGaps = { total: 0, gaps: [] };
      this.tickContexts.list.forEach(function (tick, index) {
        if (index === 0) return;
        var prevTick = _this2.tickContexts.list[index - 1];
        var prevContext = _this2.tickContexts.map[prevTick];
        var context = _this2.tickContexts.map[tick];
        var prevMetrics = prevContext.getMetrics();

        var insideRightEdge = prevContext.getX() + prevMetrics.width;
        var insideLeftEdge = context.getX();
        var gap = insideLeftEdge - insideRightEdge;
        _this2.contextGaps.total += gap;
        _this2.contextGaps.gaps.push({ x1: insideRightEdge, x2: insideLeftEdge });

        // Tell the tick contexts how much they can reposition themselves.
        context.getFormatterMetrics().freedom.left = gap;
        prevContext.getFormatterMetrics().freedom.right = gap;
      });

      // Calculate mean distance in each voice for each duration type, then calculate
      // how far each note is from the mean.
      var durationStats = this.durationStats = {};

      function updateStats(duration, space) {
        var stats = durationStats[duration];
        if (stats === undefined) {
          durationStats[duration] = { mean: space, count: 1 };
        } else {
          stats.count += 1;
          stats.mean = (stats.mean + space) / 2;
        }
      }

      this.voices.forEach(function (voice) {
        voice.getTickables().forEach(function (note, i, notes) {
          var duration = note.getTicks().clone().simplify().toString();
          var metrics = note.getMetrics();
          var formatterMetrics = note.getFormatterMetrics();
          var leftNoteEdge = note.getX() + metrics.noteWidth + metrics.modRightPx + metrics.extraRightPx;
          var space = 0;

          if (i < notes.length - 1) {
            var rightNote = notes[i + 1];
            var rightMetrics = rightNote.getMetrics();
            var rightNoteEdge = rightNote.getX() - rightMetrics.modLeftPx - rightMetrics.extraLeftPx;

            space = rightNoteEdge - leftNoteEdge;
            formatterMetrics.space.used = rightNote.getX() - note.getX();
            rightNote.getFormatterMetrics().freedom.left = space;
          } else {
            space = justifyWidth - leftNoteEdge;
            formatterMetrics.space.used = justifyWidth - note.getX();
          }

          formatterMetrics.freedom.right = space;
          updateStats(duration, formatterMetrics.space.used);
        });
      });

      // Calculate how much each note deviates from the mean. Loss function is square
      // root of the sum of squared deviations.
      var totalDeviation = 0;
      this.voices.forEach(function (voice) {
        voice.getTickables().forEach(function (note) {
          var duration = note.getTicks().clone().simplify().toString();
          var metrics = note.getFormatterMetrics();
          metrics.iterations += 1;
          metrics.space.deviation = metrics.space.used - durationStats[duration].mean;
          metrics.duration = duration;
          metrics.space.mean = durationStats[duration].mean;

          totalDeviation += Math.pow(durationStats[duration].mean, 2);
        });
      });

      this.totalCost = Math.sqrt(totalDeviation);
      this.lossHistory.push(this.totalCost);
      return this;
    }

    // Run a single iteration of rejustification. At a high level, this method calculates
    // the overall "loss" (or cost) of this layout, and repositions tickcontexts in an
    // attempt to reduce the cost. You can call this method multiple times until it finds
    // and oscillates around a global minimum.

  }, {
    key: 'tune',
    value: function tune() {
      var _this3 = this;

      var sum = function sum(means) {
        return means.reduce(function (a, b) {
          return a + b;
        });
      };

      // Move `current` tickcontext by `shift` pixels, and adjust the freedom
      // on adjacent tickcontexts.
      function move(current, prev, next, shift) {
        current.setX(current.getX() + shift);
        current.getFormatterMetrics().freedom.left += shift;
        current.getFormatterMetrics().freedom.right -= shift;

        if (prev) prev.getFormatterMetrics().freedom.right += shift;
        if (next) next.getFormatterMetrics().freedom.left -= shift;
      }

      var shift = 0;
      this.tickContexts.list.forEach(function (tick, index, list) {
        var context = _this3.tickContexts.map[tick];
        var prevContext = index > 0 ? _this3.tickContexts.map[list[index - 1]] : null;
        var nextContext = index < list.length - 1 ? _this3.tickContexts.map[list[index + 1]] : null;

        move(context, prevContext, nextContext, shift);

        var cost = -sum(context.getTickables().map(function (t) {
          return t.getFormatterMetrics().space.deviation;
        }));

        if (cost > 0) {
          shift = -Math.min(context.getFormatterMetrics().freedom.right, Math.abs(cost));
        } else if (cost < 0) {
          if (nextContext) {
            shift = Math.min(nextContext.getFormatterMetrics().freedom.right, Math.abs(cost));
          } else {
            shift = 0;
          }
        }

        var minShift = Math.min(5, Math.abs(shift));
        shift = shift > 0 ? minShift : -minShift;
      });

      return this.evaluate();
    }

    // This is the top-level call for all formatting logic completed
    // after `x` *and* `y` values have been computed for the notes
    // in the voices.

  }, {
    key: 'postFormat',
    value: function postFormat() {
      var postFormatContexts = function postFormatContexts(contexts) {
        return contexts.list.forEach(function (tick) {
          return contexts.map[tick].postFormat();
        });
      };

      postFormatContexts(this.modiferContexts);
      postFormatContexts(this.tickContexts);

      return this;
    }

    // Take all `voices` and create `ModifierContext`s out of them. This tells
    // the formatters that the voices belong on a single stave.

  }, {
    key: 'joinVoices',
    value: function joinVoices(voices) {
      this.createModifierContexts(voices);
      this.hasMinTotalWidth = false;
      return this;
    }

    // Align rests in voices, justify the contexts, and position the notes
    // so voices are aligned and ready to render onto the stave. This method
    // mutates the `x` positions of all tickables in `voices`.
    //
    // Voices are full justified to fit in `justifyWidth` pixels.
    //
    // Set `options.context` to the rendering context. Set `options.align_rests`
    // to true to enable rest alignment.

  }, {
    key: 'format',
    value: function format(voices, justifyWidth, options) {
      var opts = {
        align_rests: false,
        context: null,
        stave: null
      };

      _vex.Vex.Merge(opts, options);
      this.voices = voices;
      this.alignRests(voices, opts.align_rests);
      this.createTickContexts(voices);
      this.preFormat(justifyWidth, opts.context, voices, opts.stave);

      // Only postFormat if a stave was supplied for y value formatting
      if (opts.stave) this.postFormat();

      return this;
    }

    // This method is just like `format` except that the `justifyWidth` is inferred
    // from the `stave`.

  }, {
    key: 'formatToStave',
    value: function formatToStave(voices, stave, options) {
      var justifyWidth = stave.getNoteEndX() - stave.getNoteStartX() - 10;
      L('Formatting voices to width: ', justifyWidth);
      var opts = { context: stave.getContext() };
      _vex.Vex.Merge(opts, options);
      return this.format(voices, justifyWidth, opts);
    }
  }]);

  return Formatter;
}();

/***/ }),
/* 12 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Voice = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _tables = __webpack_require__(1);

var _fraction = __webpack_require__(8);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements the main Voice class. It's mainly a container
// object to group `Tickables` for formatting.

var Voice = exports.Voice = function (_Element) {
  _inherits(Voice, _Element);

  _createClass(Voice, null, [{
    key: 'Mode',

    // Modes allow the addition of ticks in three different ways:
    //
    // STRICT: This is the default. Ticks must fill the voice.
    // SOFT:   Ticks can be added without restrictions.
    // FULL:   Ticks do not need to fill the voice, but can't exceed the maximum
    //         tick length.
    get: function get() {
      return {
        STRICT: 1,
        SOFT: 2,
        FULL: 3
      };
    }
  }]);

  function Voice(time) {
    _classCallCheck(this, Voice);

    var _this = _possibleConstructorReturn(this, (Voice.__proto__ || Object.getPrototypeOf(Voice)).call(this));

    _this.setAttribute('type', 'Voice');

    // Time signature shortcut: "4/4", "3/8", etc.
    if (typeof time === 'string') {
      var match = time.match(/(\d+)\/(\d+)/);
      if (match) {
        time = {
          num_beats: match[1],
          beat_value: match[2],
          resolution: _tables.Flow.RESOLUTION
        };
      }
    }

    // Default time sig is 4/4
    _this.time = _vex.Vex.Merge({
      num_beats: 4,
      beat_value: 4,
      resolution: _tables.Flow.RESOLUTION
    }, time);

    // Recalculate total ticks.
    _this.totalTicks = new _fraction.Fraction(_this.time.num_beats * (_this.time.resolution / _this.time.beat_value), 1);

    _this.resolutionMultiplier = 1;

    // Set defaults
    _this.tickables = [];
    _this.ticksUsed = new _fraction.Fraction(0, 1);
    _this.smallestTickCount = _this.totalTicks.clone();
    _this.largestTickWidth = 0;
    _this.stave = null;
    // Do we care about strictly timed notes
    _this.mode = Voice.Mode.STRICT;

    // This must belong to a VoiceGroup
    _this.voiceGroup = null;
    return _this;
  }

  // Get the total ticks in the voice


  _createClass(Voice, [{
    key: 'getTotalTicks',
    value: function getTotalTicks() {
      return this.totalTicks;
    }

    // Get the total ticks used in the voice by all the tickables

  }, {
    key: 'getTicksUsed',
    value: function getTicksUsed() {
      return this.ticksUsed;
    }

    // Get the largest width of all the tickables

  }, {
    key: 'getLargestTickWidth',
    value: function getLargestTickWidth() {
      return this.largestTickWidth;
    }

    // Get the tick count for the shortest tickable

  }, {
    key: 'getSmallestTickCount',
    value: function getSmallestTickCount() {
      return this.smallestTickCount;
    }

    // Get the tickables in the voice

  }, {
    key: 'getTickables',
    value: function getTickables() {
      return this.tickables;
    }

    // Get/set the voice mode, use a value from `Voice.Mode`

  }, {
    key: 'getMode',
    value: function getMode() {
      return this.mode;
    }
  }, {
    key: 'setMode',
    value: function setMode(mode) {
      this.mode = mode;return this;
    }

    // Get the resolution multiplier for the voice

  }, {
    key: 'getResolutionMultiplier',
    value: function getResolutionMultiplier() {
      return this.resolutionMultiplier;
    }

    // Get the actual tick resolution for the voice

  }, {
    key: 'getActualResolution',
    value: function getActualResolution() {
      return this.resolutionMultiplier * this.time.resolution;
    }

    // Set the voice's stave

  }, {
    key: 'setStave',
    value: function setStave(stave) {
      this.stave = stave;
      this.boundingBox = null; // Reset bounding box so we can reformat
      return this;
    }

    // Get the bounding box for the voice

  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      var stave = void 0;
      var boundingBox = void 0;
      var bb = void 0;
      var i = void 0;

      if (!this.boundingBox) {
        if (!this.stave) throw new _vex.Vex.RERR('NoStave', "Can't get bounding box without stave.");
        stave = this.stave;
        boundingBox = null;

        for (i = 0; i < this.tickables.length; ++i) {
          this.tickables[i].setStave(stave);

          bb = this.tickables[i].getBoundingBox();
          if (!bb) continue;

          boundingBox = boundingBox ? boundingBox.mergeWith(bb) : bb;
        }

        this.boundingBox = boundingBox;
      }
      return this.boundingBox;
    }

    // Every tickable must be associated with a voiceGroup. This allows formatters
    // and preformatters to associate them with the right modifierContexts.

  }, {
    key: 'getVoiceGroup',
    value: function getVoiceGroup() {
      if (!this.voiceGroup) {
        throw new _vex.Vex.RERR('NoVoiceGroup', 'No voice group for voice.');
      }

      return this.voiceGroup;
    }

    // Set the voice group

  }, {
    key: 'setVoiceGroup',
    value: function setVoiceGroup(g) {
      this.voiceGroup = g;return this;
    }

    // Set the voice mode to strict or soft

  }, {
    key: 'setStrict',
    value: function setStrict(strict) {
      this.mode = strict ? Voice.Mode.STRICT : Voice.Mode.SOFT;
      return this;
    }

    // Determine if the voice is complete according to the voice mode

  }, {
    key: 'isComplete',
    value: function isComplete() {
      if (this.mode === Voice.Mode.STRICT || this.mode === Voice.Mode.FULL) {
        return this.ticksUsed.equals(this.totalTicks);
      } else {
        return true;
      }
    }

    // Add a tickable to the voice

  }, {
    key: 'addTickable',
    value: function addTickable(tickable) {
      if (!tickable.shouldIgnoreTicks()) {
        var ticks = tickable.getTicks();

        // Update the total ticks for this line.
        this.ticksUsed.add(ticks);

        if ((this.mode === Voice.Mode.STRICT || this.mode === Voice.Mode.FULL) && this.ticksUsed.greaterThan(this.totalTicks)) {
          this.ticksUsed.subtract(ticks);
          throw new _vex.Vex.RERR('BadArgument', 'Too many ticks.');
        }

        // Track the smallest tickable for formatting.
        if (ticks.lessThan(this.smallestTickCount)) {
          this.smallestTickCount = ticks.clone();
        }

        this.resolutionMultiplier = this.ticksUsed.denominator;

        // Expand total ticks using denominator from ticks used.
        this.totalTicks.add(0, this.ticksUsed.denominator);
      }

      // Add the tickable to the line.
      this.tickables.push(tickable);
      tickable.setVoice(this);
      return this;
    }

    // Add an array of tickables to the voice.

  }, {
    key: 'addTickables',
    value: function addTickables(tickables) {
      for (var i = 0; i < tickables.length; ++i) {
        this.addTickable(tickables[i]);
      }

      return this;
    }

    // Preformats the voice by applying the voice's stave to each note.

  }, {
    key: 'preFormat',
    value: function preFormat() {
      var _this2 = this;

      if (this.preFormatted) return this;

      this.tickables.forEach(function (tickable) {
        if (!tickable.getStave()) {
          tickable.setStave(_this2.stave);
        }
      });

      this.preFormatted = true;
      return this;
    }

    // Render the voice onto the canvas `context` and an optional `stave`.
    // If `stave` is omitted, it is expected that the notes have staves
    // already set.

  }, {
    key: 'draw',
    value: function draw() {
      var context = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : this.context;
      var stave = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : this.stave;

      this.setRendered();
      var boundingBox = null;
      for (var i = 0; i < this.tickables.length; ++i) {
        var tickable = this.tickables[i];

        // Set the stave if provided
        if (stave) tickable.setStave(stave);

        if (!tickable.getStave()) {
          throw new _vex.Vex.RuntimeError('MissingStave', 'The voice cannot draw tickables without staves.');
        }

        if (i === 0) boundingBox = tickable.getBoundingBox();

        if (i > 0 && boundingBox) {
          var tickable_bb = tickable.getBoundingBox();
          if (tickable_bb) boundingBox.mergeWith(tickable_bb);
        }

        tickable.setContext(context);
        tickable.draw();
      }

      this.boundingBox = boundingBox;
    }
  }]);

  return Voice;
}(_element.Element);

/***/ }),
/* 13 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TickContext = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tickable = __webpack_require__(42);

var _fraction = __webpack_require__(8);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// A formatter for abstract tickable objects, such as notes, chords,
// tabs, etc.

var TickContext = exports.TickContext = function (_Tickable) {
  _inherits(TickContext, _Tickable);

  _createClass(TickContext, null, [{
    key: 'getNextContext',
    value: function getNextContext(tContext) {
      var contexts = tContext.tContexts;
      var index = contexts.indexOf(tContext);

      return contexts[index + 1];
    }
  }]);

  function TickContext() {
    _classCallCheck(this, TickContext);

    var _this = _possibleConstructorReturn(this, (TickContext.__proto__ || Object.getPrototypeOf(TickContext)).call(this));

    _this.setAttribute('type', 'TickContext');
    _this.currentTick = new _fraction.Fraction(0, 1);
    _this.maxTicks = new _fraction.Fraction(0, 1);
    _this.minTicks = null;
    _this.padding = 3; // padding on each side (width += padding * 2)
    _this.x = 0;
    _this.xBase = 0; // base x position without xOffset
    _this.xOffset = 0; // xBase and xOffset are an alternative way to describe x (x = xB + xO)
    _this.tickables = []; // Notes, tabs, chords, lyrics.
    _this.notePx = 0; // width of widest note in this context
    _this.extraLeftPx = 0; // Extra left pixels for modifers & displace notes
    _this.extraRightPx = 0; // Extra right pixels for modifers & displace notes
    _this.tContexts = []; // Parent array of tick contexts
    return _this;
  }

  _createClass(TickContext, [{
    key: 'getX',
    value: function getX() {
      return this.x;
    }
  }, {
    key: 'setX',
    value: function setX(x) {
      this.x = x;this.xBase = x;this.xOffset = 0;return this;
    }
  }, {
    key: 'getXBase',
    value: function getXBase() {
      return this.xBase;
    } // use of xBase and xOffset is optional, avoids offset creep

  }, {
    key: 'setXBase',
    value: function setXBase(xBase) {
      this.xBase = xBase;this.x = xBase + this.xOffset;
    }
  }, {
    key: 'getXOffset',
    value: function getXOffset() {
      return this.xOffset;
    }
  }, {
    key: 'setXOffset',
    value: function setXOffset(xOffset) {
      this.xOffset = xOffset;this.x = this.xBase + xOffset;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width + this.padding * 2;
    }
  }, {
    key: 'setPadding',
    value: function setPadding(padding) {
      this.padding = padding;return this;
    }
  }, {
    key: 'getMaxTicks',
    value: function getMaxTicks() {
      return this.maxTicks;
    }
  }, {
    key: 'getMinTicks',
    value: function getMinTicks() {
      return this.minTicks;
    }
  }, {
    key: 'getTickables',
    value: function getTickables() {
      return this.tickables;
    }
  }, {
    key: 'getCenterAlignedTickables',
    value: function getCenterAlignedTickables() {
      return this.tickables.filter(function (tickable) {
        return tickable.isCenterAligned();
      });
    }

    // Get widths context, note and left/right modifiers for formatting

  }, {
    key: 'getMetrics',
    value: function getMetrics() {
      var width = this.width,
          notePx = this.notePx,
          extraLeftPx = this.extraLeftPx,
          extraRightPx = this.extraRightPx;

      return { width: width, notePx: notePx, extraLeftPx: extraLeftPx, extraRightPx: extraRightPx };
    }
  }, {
    key: 'getCurrentTick',
    value: function getCurrentTick() {
      return this.currentTick;
    }
  }, {
    key: 'setCurrentTick',
    value: function setCurrentTick(tick) {
      this.currentTick = tick;
      this.preFormatted = false;
    }

    // ### DEPRECATED ###
    // Get left & right pixels used for modifiers. THIS METHOD IS DEPRECATED. Use
    // the getMetrics() method instead!

  }, {
    key: 'getExtraPx',
    value: function getExtraPx() {
      var left_shift = 0;
      var right_shift = 0;
      var extraLeftPx = 0;
      var extraRightPx = 0;
      for (var i = 0; i < this.tickables.length; i++) {
        extraLeftPx = Math.max(this.tickables[i].extraLeftPx || 0, extraLeftPx);
        extraRightPx = Math.max(this.tickables[i].extraRightPx || 0, extraRightPx);
        var mContext = this.tickables[i].modifierContext;
        if (mContext && mContext != null) {
          left_shift = Math.max(left_shift, mContext.state.left_shift);
          right_shift = Math.max(right_shift, mContext.state.right_shift);
        }
      }
      return {
        left: left_shift,
        right: right_shift,
        extraLeft: extraLeftPx,
        extraRight: extraRightPx
      };
    }
  }, {
    key: 'addTickable',
    value: function addTickable(tickable) {
      if (!tickable) {
        throw new _vex.Vex.RERR('BadArgument', 'Invalid tickable added.');
      }

      if (!tickable.shouldIgnoreTicks()) {
        this.ignore_ticks = false;

        var ticks = tickable.getTicks();

        if (ticks.greaterThan(this.maxTicks)) {
          this.maxTicks = ticks.clone();
        }

        if (this.minTicks == null) {
          this.minTicks = ticks.clone();
        } else if (ticks.lessThan(this.minTicks)) {
          this.minTicks = ticks.clone();
        }
      }

      tickable.setTickContext(this);
      this.tickables.push(tickable);
      this.preFormatted = false;
      return this;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return this;

      for (var i = 0; i < this.tickables.length; ++i) {
        var tickable = this.tickables[i];
        tickable.preFormat();
        var metrics = tickable.getMetrics();

        // Maintain max extra pixels from all tickables in the context
        this.extraLeftPx = Math.max(this.extraLeftPx, metrics.extraLeftPx + metrics.modLeftPx);
        this.extraRightPx = Math.max(this.extraRightPx, metrics.extraRightPx + metrics.modRightPx);

        // Maintain the widest note for all tickables in the context
        this.notePx = Math.max(this.notePx, metrics.noteWidth);

        // Recalculate the tick context total width
        this.width = this.notePx + this.extraLeftPx + this.extraRightPx;
      }

      return this;
    }
  }, {
    key: 'postFormat',
    value: function postFormat() {
      if (this.postFormatted) return this;
      this.postFormatted = true;
      return this;
    }
  }]);

  return TickContext;
}(_tickable.Tickable);

/***/ }),
/* 14 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Renderer = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// Support for different rendering contexts: Canvas, Raphael

/* global document: false */

var _canvascontext = __webpack_require__(66);

var _raphaelcontext = __webpack_require__(67);

var _svgcontext = __webpack_require__(68);

var _vex = __webpack_require__(0);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var lastContext = null;

var Renderer = exports.Renderer = function () {
  _createClass(Renderer, null, [{
    key: 'buildContext',
    value: function buildContext(elementId, backend, width, height, background) {
      var renderer = new Renderer(elementId, backend);
      if (width && height) {
        renderer.resize(width, height);
      }

      if (!background) background = '#FFF';
      var ctx = renderer.getContext();
      ctx.setBackgroundFillStyle(background);
      Renderer.lastContext = ctx;
      return ctx;
    }
  }, {
    key: 'getCanvasContext',
    value: function getCanvasContext(elementId, width, height, background) {
      return Renderer.buildContext(elementId, Renderer.Backends.CANVAS, width, height, background);
    }
  }, {
    key: 'getRaphaelContext',
    value: function getRaphaelContext(elementId, width, height, background) {
      return Renderer.buildContext(elementId, Renderer.Backends.RAPHAEL, width, height, background);
    }
  }, {
    key: 'getSVGContext',
    value: function getSVGContext(elementId, width, height, background) {
      return Renderer.buildContext(elementId, Renderer.Backends.SVG, width, height, background);
    }
  }, {
    key: 'bolsterCanvasContext',
    value: function bolsterCanvasContext(ctx) {
      if (Renderer.USE_CANVAS_PROXY) {
        return new _canvascontext.CanvasContext(ctx);
      }

      var methodNames = ['clear', 'setFont', 'setRawFont', 'setFillStyle', 'setBackgroundFillStyle', 'setStrokeStyle', 'setShadowColor', 'setShadowBlur', 'setLineWidth', 'setLineCap', 'setLineDash', 'openGroup', 'closeGroup', 'getGroup'];

      ctx.vexFlowCanvasContext = ctx;

      methodNames.forEach(function (methodName) {
        ctx[methodName] = ctx[methodName] || _canvascontext.CanvasContext.prototype[methodName];
      });

      return ctx;
    }

    // Draw a dashed line (horizontal, vertical or diagonal
    // dashPattern = [3,3] draws a 3 pixel dash followed by a three pixel space.
    // setting the second number to 0 draws a solid line.

  }, {
    key: 'drawDashedLine',
    value: function drawDashedLine(context, fromX, fromY, toX, toY, dashPattern) {
      context.beginPath();

      var dx = toX - fromX;
      var dy = toY - fromY;
      var angle = Math.atan2(dy, dx);
      var x = fromX;
      var y = fromY;
      context.moveTo(fromX, fromY);
      var idx = 0;
      var draw = true;
      while (!((dx < 0 ? x <= toX : x >= toX) && (dy < 0 ? y <= toY : y >= toY))) {
        var dashLength = dashPattern[idx++ % dashPattern.length];
        var nx = x + Math.cos(angle) * dashLength;
        x = dx < 0 ? Math.max(toX, nx) : Math.min(toX, nx);
        var ny = y + Math.sin(angle) * dashLength;
        y = dy < 0 ? Math.max(toY, ny) : Math.min(toY, ny);
        if (draw) {
          context.lineTo(x, y);
        } else {
          context.moveTo(x, y);
        }
        draw = !draw;
      }

      context.closePath();
      context.stroke();
    }
  }, {
    key: 'Backends',
    get: function get() {
      return {
        CANVAS: 1,
        RAPHAEL: 2,
        SVG: 3,
        VML: 4
      };
    }

    // End of line types

  }, {
    key: 'LineEndType',
    get: function get() {
      return {
        NONE: 1, // No leg
        UP: 2, // Upward leg
        DOWN: 3 // Downward leg
      };
    }

    // Set this to true if you're using VexFlow inside a runtime
    // that does not allow modifiying canvas objects. There is a small
    // performance degradation due to the extra indirection.

  }, {
    key: 'USE_CANVAS_PROXY',
    get: function get() {
      return false;
    }
  }, {
    key: 'lastContext',
    get: function get() {
      return lastContext;
    },
    set: function set(ctx) {
      lastContext = ctx;
    }
  }]);

  function Renderer(elementId, backend) {
    _classCallCheck(this, Renderer);

    this.elementId = elementId;
    if (!this.elementId) {
      throw new _vex.Vex.RERR('BadArgument', 'Invalid id for renderer.');
    }

    this.element = document.getElementById(elementId);
    if (!this.element) this.element = elementId;

    // Verify backend and create context
    this.ctx = null;
    this.paper = null;
    this.backend = backend;
    if (this.backend === Renderer.Backends.CANVAS) {
      // Create context.
      if (!this.element.getContext) {
        throw new _vex.Vex.RERR('BadElement', 'Can\'t get canvas context from element: ' + elementId);
      }
      this.ctx = Renderer.bolsterCanvasContext(this.element.getContext('2d'));
    } else if (this.backend === Renderer.Backends.RAPHAEL) {
      this.ctx = new _raphaelcontext.RaphaelContext(this.element);
    } else if (this.backend === Renderer.Backends.SVG) {
      this.ctx = new _svgcontext.SVGContext(this.element);
    } else {
      throw new _vex.Vex.RERR('InvalidBackend', 'No support for backend: ' + this.backend);
    }
  }

  _createClass(Renderer, [{
    key: 'resize',
    value: function resize(width, height) {
      if (this.backend === Renderer.Backends.CANVAS) {
        if (!this.element.getContext) {
          throw new _vex.Vex.RERR('BadElement', 'Can\'t get canvas context from element: ' + this.elementId);
        }

        var devicePixelRatio = window.devicePixelRatio || 1;

        this.element.width = width * devicePixelRatio;
        this.element.height = height * devicePixelRatio;
        this.element.style.width = width + 'px';
        this.element.style.height = height + 'px';

        this.ctx = Renderer.bolsterCanvasContext(this.element.getContext('2d'));
        this.ctx.scale(devicePixelRatio, devicePixelRatio);
      } else {
        this.ctx.resize(width, height);
      }

      return this;
    }
  }, {
    key: 'getContext',
    value: function getContext() {
      return this.ctx;
    }
  }]);

  return Renderer;
}();

/***/ }),
/* 15 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Beam = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _element = __webpack_require__(3);

var _fraction = __webpack_require__(8);

var _tuplet = __webpack_require__(18);

var _stem = __webpack_require__(9);

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements `Beams` that span over a set of `StemmableNotes`.

function calculateStemDirection(notes) {
  var lineSum = 0;
  notes.forEach(function (note) {
    if (note.keyProps) {
      note.keyProps.forEach(function (keyProp) {
        lineSum += keyProp.line - 3;
      });
    }
  });

  if (lineSum >= 0) {
    return _stem.Stem.DOWN;
  }
  return _stem.Stem.UP;
}

var getStemSlope = function getStemSlope(firstNote, lastNote) {
  var firstStemTipY = firstNote.getStemExtents().topY;
  var firstStemX = firstNote.getStemX();
  var lastStemTipY = lastNote.getStemExtents().topY;
  var lastStemX = lastNote.getStemX();
  return (lastStemTipY - firstStemTipY) / (lastStemX - firstStemX);
};

var Beam = exports.Beam = function (_Element) {
  _inherits(Beam, _Element);

  _createClass(Beam, null, [{
    key: 'getDefaultBeamGroups',

    // Gets the default beam groups for a provided time signature.
    // Attempts to guess if the time signature is not found in table.
    // Currently this is fairly naive.
    value: function getDefaultBeamGroups(time_sig) {
      if (!time_sig || time_sig === 'c') {
        time_sig = '4/4';
      }

      var defaults = {
        '1/2': ['1/2'],
        '2/2': ['1/2'],
        '3/2': ['1/2'],
        '4/2': ['1/2'],

        '1/4': ['1/4'],
        '2/4': ['1/4'],
        '3/4': ['1/4'],
        '4/4': ['1/4'],

        '1/8': ['1/8'],
        '2/8': ['2/8'],
        '3/8': ['3/8'],
        '4/8': ['2/8'],

        '1/16': ['1/16'],
        '2/16': ['2/16'],
        '3/16': ['3/16'],
        '4/16': ['2/16']
      };

      var groups = defaults[time_sig];

      if (groups === undefined) {
        // If no beam groups found, naively determine
        // the beam groupings from the time signature
        var beatTotal = parseInt(time_sig.split('/')[0], 10);
        var beatValue = parseInt(time_sig.split('/')[1], 10);

        var tripleMeter = beatTotal % 3 === 0;

        if (tripleMeter) {
          return [new _fraction.Fraction(3, beatValue)];
        } else if (beatValue > 4) {
          return [new _fraction.Fraction(2, beatValue)];
        } else if (beatValue <= 4) {
          return [new _fraction.Fraction(1, beatValue)];
        }
      } else {
        return groups.map(function (group) {
          return new _fraction.Fraction().parse(group);
        });
      }

      return [new _fraction.Fraction(1, 4)];
    }

    // A helper function to automatically build basic beams for a voice. For more
    // complex auto-beaming use `Beam.generateBeams()`.
    //
    // Parameters:
    // * `voice` - The voice to generate the beams for
    // * `stem_direction` - A stem direction to apply to the entire voice
    // * `groups` - An array of `Fraction` representing beat groupings for the beam

  }, {
    key: 'applyAndGetBeams',
    value: function applyAndGetBeams(voice, stem_direction, groups) {
      return Beam.generateBeams(voice.getTickables(), {
        groups: groups,
        stem_direction: stem_direction
      });
    }

    // A helper function to autimatically build beams for a voice with
    // configuration options.
    //
    // Example configuration object:
    //
    // ```
    // config = {
    //   groups: [new Vex.Flow.Fraction(2, 8)],
    //   stem_direction: -1,
    //   beam_rests: true,
    //   beam_middle_only: true,
    //   show_stemlets: false
    // };
    // ```
    //
    // Parameters:
    // * `notes` - An array of notes to create the beams for
    // * `config` - The configuration object
    //    * `groups` - Array of `Fractions` that represent the beat structure to beam the notes
    //    * `stem_direction` - Set to apply the same direction to all notes
    //    * `beam_rests` - Set to `true` to include rests in the beams
    //    * `beam_middle_only` - Set to `true` to only beam rests in the middle of the beat
    //    * `show_stemlets` - Set to `true` to draw stemlets for rests
    //    * `maintain_stem_directions` - Set to `true` to not apply new stem directions
    //

  }, {
    key: 'generateBeams',
    value: function generateBeams(notes, config) {
      if (!config) config = {};

      if (!config.groups || !config.groups.length) {
        config.groups = [new _fraction.Fraction(2, 8)];
      }

      // Convert beam groups to tick amounts
      var tickGroups = config.groups.map(function (group) {
        if (!group.multiply) {
          throw new _vex.Vex.RuntimeError('InvalidBeamGroups', 'The beam groups must be an array of Vex.Flow.Fractions');
        }
        return group.clone().multiply(_tables.Flow.RESOLUTION, 1);
      });

      var unprocessedNotes = notes;
      var currentTickGroup = 0;
      var noteGroups = [];
      var currentGroup = [];

      function getTotalTicks(vf_notes) {
        return vf_notes.reduce(function (memo, note) {
          return note.getTicks().clone().add(memo);
        }, new _fraction.Fraction(0, 1));
      }

      function nextTickGroup() {
        if (tickGroups.length - 1 > currentTickGroup) {
          currentTickGroup += 1;
        } else {
          currentTickGroup = 0;
        }
      }

      function createGroups() {
        var nextGroup = [];

        unprocessedNotes.forEach(function (unprocessedNote) {
          nextGroup = [];
          if (unprocessedNote.shouldIgnoreTicks()) {
            noteGroups.push(currentGroup);
            currentGroup = nextGroup;
            return; // Ignore untickables (like bar notes)
          }

          currentGroup.push(unprocessedNote);
          var ticksPerGroup = tickGroups[currentTickGroup].clone();
          var totalTicks = getTotalTicks(currentGroup);

          // Double the amount of ticks in a group, if it's an unbeamable tuplet
          var unbeamable = _tables.Flow.durationToNumber(unprocessedNote.duration) < 8;
          if (unbeamable && unprocessedNote.tuplet) {
            ticksPerGroup.numerator *= 2;
          }

          // If the note that was just added overflows the group tick total
          if (totalTicks.greaterThan(ticksPerGroup)) {
            // If the overflow note can be beamed, start the next group
            // with it. Unbeamable notes leave the group overflowed.
            if (!unbeamable) {
              nextGroup.push(currentGroup.pop());
            }
            noteGroups.push(currentGroup);
            currentGroup = nextGroup;
            nextTickGroup();
          } else if (totalTicks.equals(ticksPerGroup)) {
            noteGroups.push(currentGroup);
            currentGroup = nextGroup;
            nextTickGroup();
          }
        });

        // Adds any remainder notes
        if (currentGroup.length > 0) {
          noteGroups.push(currentGroup);
        }
      }

      function getBeamGroups() {
        return noteGroups.filter(function (group) {
          if (group.length > 1) {
            var beamable = true;
            group.forEach(function (note) {
              if (note.getIntrinsicTicks() >= _tables.Flow.durationToTicks('4')) {
                beamable = false;
              }
            });
            return beamable;
          }
          return false;
        });
      }

      // Splits up groups by Rest
      function sanitizeGroups() {
        var sanitizedGroups = [];
        noteGroups.forEach(function (group) {
          var tempGroup = [];
          group.forEach(function (note, index, group) {
            var isFirstOrLast = index === 0 || index === group.length - 1;
            var prevNote = group[index - 1];

            var breaksOnEachRest = !config.beam_rests && note.isRest();
            var breaksOnFirstOrLastRest = config.beam_rests && config.beam_middle_only && note.isRest() && isFirstOrLast;

            var breakOnStemChange = false;
            if (config.maintain_stem_directions && prevNote && !note.isRest() && !prevNote.isRest()) {
              var prevDirection = prevNote.getStemDirection();
              var currentDirection = note.getStemDirection();
              breakOnStemChange = currentDirection !== prevDirection;
            }

            var isUnbeamableDuration = parseInt(note.duration, 10) < 8;

            // Determine if the group should be broken at this note
            var shouldBreak = breaksOnEachRest || breaksOnFirstOrLastRest || breakOnStemChange || isUnbeamableDuration;

            if (shouldBreak) {
              // Add current group
              if (tempGroup.length > 0) {
                sanitizedGroups.push(tempGroup);
              }

              // Start a new group. Include the current note if the group
              // was broken up by stem direction, as that note needs to start
              // the next group of notes
              tempGroup = breakOnStemChange ? [note] : [];
            } else {
              // Add note to group
              tempGroup.push(note);
            }
          });

          // If there is a remaining group, add it as well
          if (tempGroup.length > 0) {
            sanitizedGroups.push(tempGroup);
          }
        });

        noteGroups = sanitizedGroups;
      }

      function formatStems() {
        noteGroups.forEach(function (group) {
          var stemDirection = void 0;
          if (config.maintain_stem_directions) {
            var _note = findFirstNote(group);
            stemDirection = _note ? _note.getStemDirection() : _stem.Stem.UP;
          } else {
            if (config.stem_direction) {
              stemDirection = config.stem_direction;
            } else {
              stemDirection = calculateStemDirection(group);
            }
          }
          applyStemDirection(group, stemDirection);
        });
      }

      function findFirstNote(group) {
        for (var _i = 0; _i < group.length; _i++) {
          var _note2 = group[_i];
          if (!_note2.isRest()) {
            return _note2;
          }
        }

        return false;
      }

      function applyStemDirection(group, direction) {
        group.forEach(function (note) {
          note.setStemDirection(direction);
        });
      }

      // Get all of the tuplets in all of the note groups
      function getTuplets() {
        var uniqueTuplets = [];

        // Go through all of the note groups and inspect for tuplets
        noteGroups.forEach(function (group) {
          var tuplet = null;
          group.forEach(function (note) {
            if (note.tuplet && tuplet !== note.tuplet) {
              tuplet = note.tuplet;
              uniqueTuplets.push(tuplet);
            }
          });
        });
        return uniqueTuplets;
      }

      // Using closures to store the variables throughout the various functions
      // IMO Keeps it this process lot cleaner - but not super consistent with
      // the rest of the API's style - Silverwolf90 (Cyril)
      createGroups();
      sanitizeGroups();
      formatStems();

      // Get the notes to be beamed
      var beamedNoteGroups = getBeamGroups();

      // Get the tuplets in order to format them accurately
      var allTuplets = getTuplets();

      // Create a Vex.Flow.Beam from each group of notes to be beamed
      var beams = [];
      beamedNoteGroups.forEach(function (group) {
        var beam = new Beam(group);

        if (config.show_stemlets) {
          beam.render_options.show_stemlets = true;
        }
        if (config.secondary_breaks) {
          beam.render_options.secondary_break_ticks = _tables.Flow.durationToTicks(config.secondary_breaks);
        }
        if (config.flat_beams === true) {
          beam.render_options.flat_beams = true;
          beam.render_options.flat_beam_offset = config.flat_beam_offset;
        }
        beams.push(beam);
      });

      // Reformat tuplets
      allTuplets.forEach(function (tuplet) {
        // Set the tuplet location based on the stem direction
        var direction = tuplet.notes[0].stem_direction === _stem.Stem.DOWN ? _tuplet.Tuplet.LOCATION_BOTTOM : _tuplet.Tuplet.LOCATION_TOP;
        tuplet.setTupletLocation(direction);

        // If any of the notes in the tuplet are not beamed, draw a bracket.
        var bracketed = false;
        for (var _i2 = 0; _i2 < tuplet.notes.length; _i2++) {
          var _note3 = tuplet.notes[_i2];
          if (_note3.beam === null) {
            bracketed = true;
            break;
          }
        }
        tuplet.setBracketed(bracketed);
      });

      return beams;
    }
  }]);

  function Beam(notes, auto_stem) {
    _classCallCheck(this, Beam);

    var _this = _possibleConstructorReturn(this, (Beam.__proto__ || Object.getPrototypeOf(Beam)).call(this));

    _this.setAttribute('type', 'Beam');

    if (!notes || notes === []) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'No notes provided for beam.');
    }

    if (notes.length === 1) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Too few notes for beam.');
    }

    // Validate beam line, direction and ticks.
    _this.ticks = notes[0].getIntrinsicTicks();

    if (_this.ticks >= _tables.Flow.durationToTicks('4')) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Beams can only be applied to notes shorter than a quarter note.');
    }

    var i = void 0; // shared iterator
    var note = void 0;

    _this.stem_direction = _stem.Stem.UP;

    for (i = 0; i < notes.length; ++i) {
      note = notes[i];
      if (note.hasStem()) {
        _this.stem_direction = note.getStemDirection();
        break;
      }
    }

    var stem_direction = _this.stem_direction;
    // Figure out optimal stem direction based on given notes
    if (auto_stem && notes[0].getCategory() === 'stavenotes') {
      stem_direction = calculateStemDirection(notes);
    } else if (auto_stem && notes[0].getCategory() === 'tabnotes') {
      // Auto Stem TabNotes
      var stem_weight = notes.reduce(function (memo, note) {
        return memo + note.stem_direction;
      }, 0);

      stem_direction = stem_weight > -1 ? _stem.Stem.UP : _stem.Stem.DOWN;
    }

    // Apply stem directions and attach beam to notes
    for (i = 0; i < notes.length; ++i) {
      note = notes[i];
      if (auto_stem) {
        note.setStemDirection(stem_direction);
        _this.stem_direction = stem_direction;
      }
      note.setBeam(_this);
    }

    _this.postFormatted = false;
    _this.notes = notes;
    _this.beam_count = _this.getBeamCount();
    _this.break_on_indices = [];
    _this.render_options = {
      beam_width: 5,
      max_slope: 0.25,
      min_slope: -0.25,
      slope_iterations: 20,
      slope_cost: 100,
      show_stemlets: false,
      stemlet_extension: 7,
      partial_beam_length: 10,
      flat_beams: false,
      min_flat_beam_offset: 15
    };
    return _this;
  }

  // Get the notes in this beam


  _createClass(Beam, [{
    key: 'getNotes',
    value: function getNotes() {
      return this.notes;
    }

    // Get the max number of beams in the set of notes

  }, {
    key: 'getBeamCount',
    value: function getBeamCount() {
      var beamCounts = this.notes.map(function (note) {
        return note.getGlyph().beam_count;
      });

      var maxBeamCount = beamCounts.reduce(function (max, beamCount) {
        return beamCount > max ? beamCount : max;
      });

      return maxBeamCount;
    }

    // Set which note `indices` to break the secondary beam at

  }, {
    key: 'breakSecondaryAt',
    value: function breakSecondaryAt(indices) {
      this.break_on_indices = indices;
      return this;
    }

    // Return the y coordinate for linear function

  }, {
    key: 'getSlopeY',
    value: function getSlopeY(x, first_x_px, first_y_px, slope) {
      return first_y_px + (x - first_x_px) * slope;
    }

    // Calculate the best possible slope for the provided notes

  }, {
    key: 'calculateSlope',
    value: function calculateSlope() {
      var notes = this.notes,
          stemDirection = this.stem_direction,
          _render_options = this.render_options,
          max_slope = _render_options.max_slope,
          min_slope = _render_options.min_slope,
          slope_iterations = _render_options.slope_iterations,
          slope_cost = _render_options.slope_cost;


      var firstNote = notes[0];
      var initialSlope = getStemSlope(firstNote, notes[notes.length - 1]);
      var increment = (max_slope - min_slope) / slope_iterations;
      var minCost = Number.MAX_VALUE;
      var bestSlope = 0;
      var yShift = 0;

      // iterate through slope values to find best weighted fit
      for (var slope = min_slope; slope <= max_slope; slope += increment) {
        var totalStemExtension = 0;
        var yShiftTemp = 0;

        // iterate through notes, calculating y shift and stem extension
        for (var _i3 = 1; _i3 < notes.length; ++_i3) {
          var _note4 = notes[_i3];
          var adjustedStemTipY = this.getSlopeY(_note4.getStemX(), firstNote.getStemX(), firstNote.getStemExtents().topY, slope) + yShiftTemp;

          var stemTipY = _note4.getStemExtents().topY;
          // beam needs to be shifted up to accommodate note
          if (stemTipY * stemDirection < adjustedStemTipY * stemDirection) {
            var diff = Math.abs(stemTipY - adjustedStemTipY);
            yShiftTemp += diff * -stemDirection;
            totalStemExtension += diff * _i3;
          } else {
            // beam overshoots note, account for the difference
            totalStemExtension += (stemTipY - adjustedStemTipY) * stemDirection;
          }
        }

        // most engraving books suggest aiming for a slope about half the angle of the
        // difference between the first and last notes' stem length;
        var idealSlope = initialSlope / 2;
        var distanceFromIdeal = Math.abs(idealSlope - slope);

        // This tries to align most beams to something closer to the idealSlope, but
        // doesn't go crazy. To disable, set this.render_options.slope_cost = 0
        var cost = slope_cost * distanceFromIdeal + Math.abs(totalStemExtension);

        // update state when a more ideal slope is found
        if (cost < minCost) {
          minCost = cost;
          bestSlope = slope;
          yShift = yShiftTemp;
        }
      }

      this.slope = bestSlope;
      this.y_shift = yShift;
    }

    // Calculate a slope and y-shift for flat beams

  }, {
    key: 'calculateFlatSlope',
    value: function calculateFlatSlope() {
      var notes = this.notes,
          stem_direction = this.stem_direction,
          _render_options2 = this.render_options,
          beam_width = _render_options2.beam_width,
          min_flat_beam_offset = _render_options2.min_flat_beam_offset,
          flat_beam_offset = _render_options2.flat_beam_offset;

      // If a flat beam offset has not yet been supplied or calculated,
      // generate one based on the notes in this particular note group

      var total = 0;
      var extremeY = 0; // Store the highest or lowest note here
      var extremeBeamCount = 0; // The beam count of the extreme note
      var currentExtreme = 0;
      for (var _i4 = 0; _i4 < notes.length; _i4++) {
        // Total up all of the offsets so we can average them out later
        var _note5 = notes[_i4];
        var stemTipY = _note5.getStemExtents().topY;
        total += stemTipY;

        // Store the highest (stems-up) or lowest (stems-down) note so the
        //  offset can be adjusted in case the average isn't enough
        if (stem_direction === _stem.Stem.DOWN && currentExtreme < stemTipY) {
          currentExtreme = stemTipY;
          extremeY = Math.max.apply(Math, _toConsumableArray(_note5.getYs()));
          extremeBeamCount = _note5.getBeamCount();
        } else if (stem_direction === _stem.Stem.UP && (currentExtreme === 0 || currentExtreme > stemTipY)) {
          currentExtreme = stemTipY;
          extremeY = Math.min.apply(Math, _toConsumableArray(_note5.getYs()));
          extremeBeamCount = _note5.getBeamCount();
        }
      }

      // Average the offsets to try and come up with a reasonable one that
      //  works for all of the notes in the beam group.
      var offset = total / notes.length;

      // In case the average isn't long enough, add or subtract some more
      //  based on the highest or lowest note (again, based on the stem
      //  direction). This also takes into account the added height due to
      //  the width of the beams.
      var beamWidth = beam_width * 1.5;
      var extremeTest = min_flat_beam_offset + extremeBeamCount * beamWidth;
      var newOffset = extremeY + extremeTest * -stem_direction;
      if (stem_direction === _stem.Stem.DOWN && offset < newOffset) {
        offset = extremeY + extremeTest;
      } else if (stem_direction === _stem.Stem.UP && offset > newOffset) {
        offset = extremeY - extremeTest;
      }

      if (!flat_beam_offset) {
        // Set the offset for the group based on the calculations above.
        this.render_options.flat_beam_offset = offset;
      } else if (stem_direction === _stem.Stem.DOWN && offset > flat_beam_offset) {
        this.render_options.flat_beam_offset = offset;
      } else if (stem_direction === _stem.Stem.UP && offset < flat_beam_offset) {
        this.render_options.flat_beam_offset = offset;
      }

      // for flat beams, the slope and y_shift are simply 0
      this.slope = 0;
      this.y_shift = 0;
    }

    // Create new stems for the notes in the beam, so that each stem
    // extends into the beams.

  }, {
    key: 'applyStemExtensions',
    value: function applyStemExtensions() {
      var notes = this.notes,
          slope = this.slope,
          y_shift = this.y_shift,
          stem_direction = this.stem_direction,
          beam_count = this.beam_count,
          _render_options3 = this.render_options,
          show_stemlets = _render_options3.show_stemlets,
          flat_beam_offset = _render_options3.flat_beam_offset,
          flat_beams = _render_options3.flat_beams,
          stemlet_extension = _render_options3.stemlet_extension,
          beam_width = _render_options3.beam_width;


      var firstNote = notes[0];
      var firstStemTipY = firstNote.getStemExtents().topY;

      // If rendering flat beams, and an offset exists, set the y-coordinat`e to
      //  the offset so the stems all end at the beam offset.
      if (flat_beams && flat_beam_offset) {
        firstStemTipY = flat_beam_offset;
      }
      var firstStemX = firstNote.getStemX();

      for (var _i5 = 0; _i5 < notes.length; ++_i5) {
        var _note6 = notes[_i5];
        var stemX = _note6.getStemX();

        var _note6$getStemExtents = _note6.getStemExtents(),
            stemTipY = _note6$getStemExtents.topY;

        var beamedStemTipY = this.getSlopeY(stemX, firstStemX, firstStemTipY, slope) + y_shift;
        var preBeamExtension = _note6.getStem().getExtension();
        var beamExtension = stem_direction === _stem.Stem.UP ? stemTipY - beamedStemTipY : beamedStemTipY - stemTipY;

        _note6.stem.setExtension(preBeamExtension + beamExtension);
        _note6.stem.renderHeightAdjustment = -_stem.Stem.WIDTH / 2;

        if (_note6.isRest() && show_stemlets) {
          var beamWidth = beam_width;
          var totalBeamWidth = (beam_count - 1) * beamWidth * 1.5 + beamWidth;
          _note6.stem.setVisibility(true).setStemlet(true, totalBeamWidth + stemlet_extension);
        }
      }
    }

    // Get the x coordinates for the beam lines of specific `duration`

  }, {
    key: 'getBeamLines',
    value: function getBeamLines(duration) {
      var beam_lines = [];
      var beam_started = false;
      var current_beam = null;
      var partial_beam_length = this.render_options.partial_beam_length;
      var previous_should_break = false;
      var tick_tally = 0;
      for (var _i6 = 0; _i6 < this.notes.length; ++_i6) {
        var _note7 = this.notes[_i6];

        // See if we need to break secondary beams on this note.
        var ticks = _note7.ticks.value();
        tick_tally += ticks;
        var should_break = false;

        // 8th note beams are always drawn.
        if (parseInt(duration, 10) >= 8) {
          // First, check to see if any indices were set up through breakSecondaryAt()
          should_break = this.break_on_indices.indexOf(_i6) !== -1;

          // If the secondary breaks were auto-configured in the render options,
          //  handle that as well.
          if (this.render_options.secondary_break_ticks && tick_tally >= this.render_options.secondary_break_ticks) {
            tick_tally = 0;
            should_break = true;
          }
        }
        var note_gets_beam = _note7.getIntrinsicTicks() < _tables.Flow.durationToTicks(duration);

        var stem_x = _note7.getStemX() - _stem.Stem.WIDTH / 2;

        // Check to see if the next note in the group will get a beam at this
        //  level. This will help to inform the partial beam logic below.
        var next_note = this.notes[_i6 + 1];
        var beam_next = next_note && next_note.getIntrinsicTicks() < _tables.Flow.durationToTicks(duration);
        if (note_gets_beam) {
          // This note gets a beam at the current level
          if (beam_started) {
            // We're currently in the middle of a beam. Just continue it on to
            //  the stem X of the current note.
            current_beam = beam_lines[beam_lines.length - 1];
            current_beam.end = stem_x;

            // If a secondary beam break is set up, end the beam right now.
            if (should_break) {
              beam_started = false;
              if (next_note && !beam_next && current_beam.end === null) {
                // This note gets a beam,.but the next one does not. This means
                //  we need a partial pointing right.
                current_beam.end = current_beam.start - partial_beam_length;
              }
            }
          } else {
            // No beam started yet. Start a new one.
            current_beam = { start: stem_x, end: null };
            beam_started = true;
            if (!beam_next) {
              // The next note doesn't get a beam. Draw a partial.
              if ((previous_should_break || _i6 === 0) && next_note) {
                // This is the first note (but not the last one), or it is
                //  following a secondary break. Draw a partial to the right.
                current_beam.end = current_beam.start + partial_beam_length;
              } else {
                // By default, draw a partial to the left.
                current_beam.end = current_beam.start - partial_beam_length;
              }
            } else if (should_break) {
              // This note should have a secondary break after it. Even though
              //  we just started a beam, it needs to end immediately.
              current_beam.end = current_beam.start - partial_beam_length;
              beam_started = false;
            }
            beam_lines.push(current_beam);
          }
        } else {
          // The current note does not get a beam.
          beam_started = false;
        }

        // Store the secondary break flag to inform the partial beam logic in
        //  the next iteration of the loop.
        previous_should_break = should_break;
      }

      // Add a partial beam pointing left if this is the last note in the group
      var last_beam = beam_lines[beam_lines.length - 1];
      if (last_beam && last_beam.end === null) {
        last_beam.end = last_beam.start - partial_beam_length;
      }
      return beam_lines;
    }

    // Render the stems for each notes

  }, {
    key: 'drawStems',
    value: function drawStems() {
      var _this2 = this;

      this.notes.forEach(function (note) {
        if (note.getStem()) {
          note.getStem().setContext(_this2.context).draw();
        }
      }, this);
    }

    // Render the beam lines

  }, {
    key: 'drawBeamLines',
    value: function drawBeamLines() {
      this.checkContext();

      var valid_beam_durations = ['4', '8', '16', '32', '64'];

      var firstNote = this.notes[0];

      var firstStemTipY = firstNote.getStemExtents().topY;
      var beamY = firstStemTipY;

      // For flat beams, set the first and last Y to the offset, rather than
      //  using the note's stem extents.
      if (this.render_options.flat_beams && this.render_options.flat_beam_offset) {
        beamY = this.render_options.flat_beam_offset;
      }

      var firstStemX = firstNote.getStemX();
      var beamThickness = this.render_options.beam_width * this.stem_direction;

      // Draw the beams.
      for (var _i7 = 0; _i7 < valid_beam_durations.length; ++_i7) {
        var duration = valid_beam_durations[_i7];
        var beamLines = this.getBeamLines(duration);

        for (var j = 0; j < beamLines.length; ++j) {
          var beam_line = beamLines[j];
          var startBeamX = beam_line.start;

          var startBeamY = this.getSlopeY(startBeamX, firstStemX, beamY, this.slope);
          var lastBeamX = beam_line.end;
          var lastBeamY = this.getSlopeY(lastBeamX, firstStemX, beamY, this.slope);

          this.context.beginPath();
          this.context.moveTo(startBeamX, startBeamY);
          this.context.lineTo(startBeamX, startBeamY + beamThickness);
          this.context.lineTo(lastBeamX + 1, lastBeamY + beamThickness);
          this.context.lineTo(lastBeamX + 1, lastBeamY);
          this.context.closePath();
          this.context.fill();
        }

        beamY += beamThickness * 1.5;
      }
    }

    // Pre-format the beam

  }, {
    key: 'preFormat',
    value: function preFormat() {
      return this;
    }

    // Post-format the beam. This can only be called after
    // the notes in the beam have both `x` and `y` values. ie: they've
    // been formatted and have staves

  }, {
    key: 'postFormat',
    value: function postFormat() {
      if (this.postFormatted) return;

      // Calculate a smart slope if we're not forcing the beams to be flat.
      if (this.notes[0].getCategory() === 'tabnotes' || this.render_options.flat_beams) {
        this.calculateFlatSlope();
      } else {
        this.calculateSlope();
      }
      this.applyStemExtensions();

      this.postFormatted = true;
    }

    // Render the beam to the canvas context

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();
      if (this.unbeamable) return;

      if (!this.postFormatted) {
        this.postFormat();
      }

      this.drawStems();
      this.applyStyle();
      this.drawBeamLines();
      this.restoreStyle();
    }
  }]);

  return Beam;
}(_element.Element);

/***/ }),
/* 16 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveTie = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class implements varies types of ties between contiguous notes. The
// ties include: regular ties, hammer ons, pull offs, and slides.

var StaveTie = exports.StaveTie = function (_Element) {
  _inherits(StaveTie, _Element);

  function StaveTie(notes, text) {
    _classCallCheck(this, StaveTie);

    var _this = _possibleConstructorReturn(this, (StaveTie.__proto__ || Object.getPrototypeOf(StaveTie)).call(this));
    /**
     * Notes is a struct that has:
     *
     *  {
     *    first_note: Note,
     *    last_note: Note,
     *    first_indices: [n1, n2, n3],
     *    last_indices: [n1, n2, n3]
     *  }
     *
     **/


    _this.setAttribute('type', 'StaveTie');
    _this.notes = notes;
    _this.context = null;
    _this.text = text;
    _this.direction = null;

    _this.render_options = {
      cp1: 8, // Curve control point 1
      cp2: 12, // Curve control point 2
      text_shift_x: 0,
      first_x_shift: 0,
      last_x_shift: 0,
      y_shift: 7,
      tie_spacing: 0,
      font: { family: 'Arial', size: 10, style: '' }
    };

    _this.font = _this.render_options.font;
    _this.setNotes(notes);
    return _this;
  }

  _createClass(StaveTie, [{
    key: 'setFont',
    value: function setFont(font) {
      this.font = font;return this;
    }
  }, {
    key: 'setDirection',
    value: function setDirection(direction) {
      this.direction = direction;return this;
    }

    /**
     * Set the notes to attach this tie to.
     *
     * @param {!Object} notes The notes to tie up.
     */

  }, {
    key: 'setNotes',
    value: function setNotes(notes) {
      if (!notes.first_note && !notes.last_note) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'Tie needs to have either first_note or last_note set.');
      }

      if (!notes.first_indices) notes.first_indices = [0];
      if (!notes.last_indices) notes.last_indices = [0];

      if (notes.first_indices.length !== notes.last_indices.length) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'Tied notes must have similar index sizes');
      }

      // Success. Lets grab 'em notes.
      this.first_note = notes.first_note;
      this.first_indices = notes.first_indices;
      this.last_note = notes.last_note;
      this.last_indices = notes.last_indices;
      return this;
    }

    /**
     * @return {boolean} Returns true if this is a partial bar.
     */

  }, {
    key: 'isPartial',
    value: function isPartial() {
      return !this.first_note || !this.last_note;
    }
  }, {
    key: 'renderTie',
    value: function renderTie(params) {
      if (params.first_ys.length === 0 || params.last_ys.length === 0) {
        throw new _vex.Vex.RERR('BadArguments', 'No Y-values to render');
      }

      var ctx = this.context;
      var cp1 = this.render_options.cp1;
      var cp2 = this.render_options.cp2;

      if (Math.abs(params.last_x_px - params.first_x_px) < 10) {
        cp1 = 2;cp2 = 8;
      }

      var first_x_shift = this.render_options.first_x_shift;
      var last_x_shift = this.render_options.last_x_shift;
      var y_shift = this.render_options.y_shift * params.direction;

      for (var i = 0; i < this.first_indices.length; ++i) {
        var cp_x = (params.last_x_px + last_x_shift + (params.first_x_px + first_x_shift)) / 2;
        var first_y_px = params.first_ys[this.first_indices[i]] + y_shift;
        var last_y_px = params.last_ys[this.last_indices[i]] + y_shift;

        if (isNaN(first_y_px) || isNaN(last_y_px)) {
          throw new _vex.Vex.RERR('BadArguments', 'Bad indices for tie rendering.');
        }

        var top_cp_y = (first_y_px + last_y_px) / 2 + cp1 * params.direction;
        var bottom_cp_y = (first_y_px + last_y_px) / 2 + cp2 * params.direction;

        ctx.beginPath();
        ctx.moveTo(params.first_x_px + first_x_shift, first_y_px);
        ctx.quadraticCurveTo(cp_x, top_cp_y, params.last_x_px + last_x_shift, last_y_px);
        ctx.quadraticCurveTo(cp_x, bottom_cp_y, params.first_x_px + first_x_shift, first_y_px);
        ctx.closePath();
        ctx.fill();
      }
    }
  }, {
    key: 'renderText',
    value: function renderText(first_x_px, last_x_px) {
      if (!this.text) return;
      var center_x = (first_x_px + last_x_px) / 2;
      center_x -= this.context.measureText(this.text).width / 2;

      this.context.save();
      this.context.setFont(this.font.family, this.font.size, this.font.style);
      this.context.fillText(this.text, center_x + this.render_options.text_shift_x, (this.first_note || this.last_note).getStave().getYForTopText() - 1);
      this.context.restore();
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      var first_note = this.first_note;
      var last_note = this.last_note;

      var first_x_px = void 0;
      var last_x_px = void 0;
      var first_ys = void 0;
      var last_ys = void 0;
      var stem_direction = void 0;
      if (first_note) {
        first_x_px = first_note.getTieRightX() + this.render_options.tie_spacing;
        stem_direction = first_note.getStemDirection();
        first_ys = first_note.getYs();
      } else {
        first_x_px = last_note.getStave().getTieStartX();
        first_ys = last_note.getYs();
        this.first_indices = this.last_indices;
      }

      if (last_note) {
        last_x_px = last_note.getTieLeftX() + this.render_options.tie_spacing;
        stem_direction = last_note.getStemDirection();
        last_ys = last_note.getYs();
      } else {
        last_x_px = first_note.getStave().getTieEndX();
        last_ys = first_note.getYs();
        this.last_indices = this.first_indices;
      }

      if (this.direction) {
        stem_direction = this.direction;
      }

      this.renderTie({
        first_x_px: first_x_px,
        last_x_px: last_x_px,
        first_ys: first_ys,
        last_ys: last_ys,
        direction: stem_direction
      });

      this.renderText(first_x_px, last_x_px);
      return true;
    }
  }]);

  return StaveTie;
}(_element.Element);

/***/ }),
/* 17 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Articulation = undefined;

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

var _glyph = __webpack_require__(2);

var _stem = __webpack_require__(9);

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Larry Kuhns.
//
// ## Description
//
// This file implements articulations and accents as modifiers that can be
// attached to notes. The complete list of articulations is available in
// `tables.js` under `Vex.Flow.articulationCodes`.
//
// See `tests/articulation_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.Articulation.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Articulation.DEBUG) _vex.Vex.L('Vex.Flow.Articulation', args);
}

var _Modifier$Position = _modifier.Modifier.Position,
    ABOVE = _Modifier$Position.ABOVE,
    BELOW = _Modifier$Position.BELOW;


var roundToNearestHalf = function roundToNearestHalf(mathFn, value) {
  return mathFn(value / 0.5) * 0.5;
};

// This includes both staff and ledger lines
var isWithinLines = function isWithinLines(line, position) {
  return position === ABOVE ? line <= 5 : line >= 1;
};

var getRoundingFunction = function getRoundingFunction(line, position) {
  if (isWithinLines(line, position)) {
    if (position === ABOVE) {
      return Math.ceil;
    } else {
      return Math.floor;
    }
  } else {
    return Math.round;
  }
};

var snapLineToStaff = function snapLineToStaff(canSitBetweenLines, line, position, offsetDirection) {
  // Initially, snap to nearest staff line or space
  var snappedLine = roundToNearestHalf(getRoundingFunction(line, position), line);
  var canSnapToStaffSpace = canSitBetweenLines && isWithinLines(snappedLine, position);
  var onStaffLine = snappedLine % 1 === 0;

  if (canSnapToStaffSpace && onStaffLine) {
    var HALF_STAFF_SPACE = 0.5;
    return snappedLine + HALF_STAFF_SPACE * -offsetDirection;
  } else {
    return snappedLine;
  }
};

var getTopY = function getTopY(note, textLine) {
  var stave = note.getStave();
  var stemDirection = note.getStemDirection();

  var _note$getStemExtents = note.getStemExtents(),
      stemTipY = _note$getStemExtents.topY,
      stemBaseY = _note$getStemExtents.baseY;

  if (note.getCategory() === 'stavenotes') {
    if (note.hasStem()) {
      if (stemDirection === _stem.Stem.UP) {
        return stemTipY;
      } else {
        return stemBaseY;
      }
    } else {
      return Math.min.apply(Math, _toConsumableArray(note.getYs()));
    }
  } else if (note.getCategory() === 'tabnotes') {
    if (note.hasStem()) {
      if (stemDirection === _stem.Stem.UP) {
        return stemTipY;
      } else {
        return stave.getYForTopText(textLine);
      }
    } else {
      return stave.getYForTopText(textLine);
    }
  } else {
    throw new _vex.Vex.RERR('UnknownCategory', 'Only can get the top and bottom ys of stavenotes and tabnotes');
  }
};

var getBottomY = function getBottomY(note, textLine) {
  var stave = note.getStave();
  var stemDirection = note.getStemDirection();

  var _note$getStemExtents2 = note.getStemExtents(),
      stemTipY = _note$getStemExtents2.topY,
      stemBaseY = _note$getStemExtents2.baseY;

  if (note.getCategory() === 'stavenotes') {
    if (note.hasStem()) {
      if (stemDirection === _stem.Stem.UP) {
        return stemBaseY;
      } else {
        return stemTipY;
      }
    } else {
      return Math.max.apply(Math, _toConsumableArray(note.getYs()));
    }
  } else if (note.getCategory() === 'tabnotes') {
    if (note.hasStem()) {
      if (stemDirection === _stem.Stem.UP) {
        return stave.getYForBottomText(textLine);
      } else {
        return stemTipY;
      }
    } else {
      return stave.getYForBottomText(textLine);
    }
  } else {
    throw new _vex.Vex.RERR('UnknownCategory', 'Only can get the top and bottom ys of stavenotes and tabnotes');
  }
};

// Gets the initial offset of the articulation from the y value of the starting position.
// This is required because the top/bottom text positions already have spacing applied to
// provide a "visually pleasent" default position. However the y values provided from
// the stavenote's top/bottom do *not* have any pre-applied spacing. This function
// normalizes this asymmetry.
var getInitialOffset = function getInitialOffset(note, position) {
  var isOnStemTip = position === ABOVE && note.getStemDirection() === _stem.Stem.UP || position === BELOW && note.getStemDirection() === _stem.Stem.DOWN;

  if (note.getCategory() === 'stavenotes') {
    if (note.hasStem() && isOnStemTip) {
      return 0.5;
    } else {
      // this amount is larger than the stem-tip offset because we start from
      // the center of the notehead
      return 1;
    }
  } else {
    if (note.hasStem() && isOnStemTip) {
      return 1;
    } else {
      return 0;
    }
  }
};

var Articulation = exports.Articulation = function (_Modifier) {
  _inherits(Articulation, _Modifier);

  _createClass(Articulation, null, [{
    key: 'format',


    // FIXME:
    // Most of the complex formatting logic (ie: snapping to space) is
    // actually done in .render(). But that logic belongs in this method.
    //
    // Unfortunately, this isn't possible because, by this point, stem lengths
    // have not yet been finalized. Finalized stem lengths are required to determine the
    // initial position of any stem-side articulation.
    //
    // This indicates that all objects should have their stave set before being
    // formatted. It can't be an optional if you want accurate vertical positioning.
    // Consistently positioned articulations that play nice with other modifiers
    // won't be possible until we stop relying on render-time formatting.
    //
    // Ideally, when this function has completed, the vertical articulation positions
    // should be ready to render without further adjustment. But the current state
    // is far from this ideal.
    value: function format(articulations, state) {
      if (!articulations || articulations.length === 0) return false;

      var isAbove = function isAbove(artic) {
        return artic.getPosition() === ABOVE;
      };
      var isBelow = function isBelow(artic) {
        return artic.getPosition() === BELOW;
      };
      var margin = 0.5;
      var getIncrement = function getIncrement(articulation, line, position) {
        return roundToNearestHalf(getRoundingFunction(line, position), articulation.glyph.getMetrics().height / 10 + margin);
      };

      articulations.filter(isAbove).forEach(function (articulation) {
        articulation.setTextLine(state.top_text_line);
        state.top_text_line += getIncrement(articulation, state.top_text_line, ABOVE);
      });

      articulations.filter(isBelow).forEach(function (articulation) {
        articulation.setTextLine(state.text_line);
        state.text_line += getIncrement(articulation, state.text_line, BELOW);
      });

      var width = articulations.map(function (articulation) {
        return articulation.getWidth();
      }).reduce(function (maxWidth, articWidth) {
        return Math.max(articWidth, maxWidth);
      });

      state.left_shift += width / 2;
      state.right_shift += width / 2;
      return true;
    }
  }, {
    key: 'easyScoreHook',
    value: function easyScoreHook(_ref, note, builder) {
      var articulations = _ref.articulations;

      if (!articulations) return;

      var articNameToCode = {
        staccato: 'a.',
        tenuto: 'a-'
      };

      articulations.split(',').map(function (articString) {
        return articString.trim().split('.');
      }).map(function (_ref2) {
        var _ref3 = _slicedToArray(_ref2, 2),
            name = _ref3[0],
            position = _ref3[1];

        var artic = { type: articNameToCode[name] };
        if (position) artic.position = _modifier.Modifier.PositionString[position];
        return builder.getFactory().Articulation(artic);
      }).map(function (artic) {
        return note.addModifier(0, artic);
      });
    }

    // Create a new articulation of type `type`, which is an entry in
    // `Vex.Flow.articulationCodes` in `tables.js`.

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'articulations';
    }
  }, {
    key: 'INITIAL_OFFSET',
    get: function get() {
      return -0.5;
    }
  }]);

  function Articulation(type) {
    _classCallCheck(this, Articulation);

    var _this = _possibleConstructorReturn(this, (Articulation.__proto__ || Object.getPrototypeOf(Articulation)).call(this));

    _this.setAttribute('type', 'Articulation');

    _this.note = null;
    _this.index = null;
    _this.type = type;
    _this.position = BELOW;
    _this.render_options = {
      font_scale: 38
    };

    _this.articulation = _tables.Flow.articulationCodes(_this.type);
    if (!_this.articulation) {
      throw new _vex.Vex.RERR('ArgumentError', 'Articulation not found: ' + _this.type);
    }

    _this.glyph = new _glyph.Glyph(_this.articulation.code, _this.render_options.font_scale);

    _this.setWidth(_this.glyph.getMetrics().width);
    return _this;
  }

  _createClass(Articulation, [{
    key: 'getCategory',
    value: function getCategory() {
      return Articulation.CATEGORY;
    }

    // Render articulation in position next to note.

  }, {
    key: 'draw',
    value: function draw() {
      var _ABOVE$BELOW$position;

      var note = this.note,
          index = this.index,
          position = this.position,
          glyph = this.glyph,
          canSitBetweenLines = this.articulation.between_lines,
          textLine = this.text_line,
          ctx = this.context;


      this.checkContext();

      if (!note || index == null) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw Articulation without a note and index.");
      }

      this.setRendered();

      var stave = note.getStave();
      var staffSpace = stave.getSpacingBetweenLines();
      var isTab = note.getCategory() === 'tabnotes';

      // Articulations are centered over/under the note head.

      var _note$getModifierStar = note.getModifierStartXY(position, index),
          x = _note$getModifierStar.x;

      var shouldSitOutsideStaff = !canSitBetweenLines || isTab;

      var initialOffset = getInitialOffset(note, position);

      var y = (_ABOVE$BELOW$position = {}, _defineProperty(_ABOVE$BELOW$position, ABOVE, function () {
        glyph.setOrigin(0.5, 1);
        var y = getTopY(note, textLine) - (textLine + initialOffset) * staffSpace;
        return shouldSitOutsideStaff ? Math.min(stave.getYForTopText(Articulation.INITIAL_OFFSET), y) : y;
      }), _defineProperty(_ABOVE$BELOW$position, BELOW, function () {
        glyph.setOrigin(0.5, 0);
        var y = getBottomY(note, textLine) + (textLine + initialOffset) * staffSpace;
        return shouldSitOutsideStaff ? Math.max(stave.getYForBottomText(Articulation.INITIAL_OFFSET), y) : y;
      }), _ABOVE$BELOW$position)[position]();

      if (!isTab) {
        var offsetDirection = position === ABOVE ? -1 : +1;
        var noteLine = isTab ? note.positions[index].str : note.getKeyProps()[index].line;
        var distanceFromNote = (note.getYs()[index] - y) / staffSpace;
        var articLine = distanceFromNote + noteLine;
        var snappedLine = snapLineToStaff(canSitBetweenLines, articLine, position, offsetDirection);

        if (isWithinLines(snappedLine, position)) glyph.setOrigin(0.5, 0.5);

        y += Math.abs(snappedLine - articLine) * staffSpace * offsetDirection;
      }

      L('Rendering articulation at (x: ' + x + ', y: ' + y + ')');

      glyph.render(ctx, x, y);
    }
  }]);

  return Articulation;
}(_modifier.Modifier);

/***/ }),
/* 18 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Tuplet = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _formatter = __webpack_require__(11);

var _glyph = __webpack_require__(2);

var _stem = __webpack_require__(9);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

/**
 * ## Description
 *
 * Create a new tuplet from the specified notes. The notes must
 * be part of the same voice. If they are of different rhythmic
 * values, then options.num_notes must be set.
 *
 * @constructor
 * @param {Array.<Vex.Flow.StaveNote>} A set of notes: staveNotes,
 *   notes, etc... any class that inherits stemmableNote at some
 *   point in its prototype chain.
 * @param options: object {
 *
 *   num_notes: fit this many notes into...
 *   notes_occupied: ...the space of this many notes
 *
 *       Together, these two properties make up the tuplet ratio
 *     in the form of num_notes : notes_occupied.
 *       num_notes defaults to the number of notes passed in, so
 *     it is important that if you omit this property, all of
 *     the notes passed should be of the same note value.
 *       notes_occupied defaults to 2 -- so you should almost
 *     certainly pass this parameter for anything other than
 *     a basic triplet.
 *
 *   location:
 *     default 1, which is above the notes: ┌─── 3 ───┐
 *      -1 is below the notes └─── 3 ───┘
 *
 *   bracketed: boolean, draw a bracket around the tuplet number
 *     when true: ┌─── 3 ───┐   when false: 3
 *     defaults to true if notes are not beamed, false otherwise
 *
 *   ratioed: boolean
 *     when true: ┌─── 7:8 ───┐, when false: ┌─── 7 ───┐
 *     defaults to true if the difference between num_notes and
 *     notes_occupied is greater than 1.
 *
 *   y_offset: int, default 0
 *     manually offset a tuplet, for instance to avoid collisions
 *     with articulations, etc...
 * }
 */

var Tuplet = exports.Tuplet = function (_Element) {
  _inherits(Tuplet, _Element);

  _createClass(Tuplet, null, [{
    key: 'LOCATION_TOP',
    get: function get() {
      return 1;
    }
  }, {
    key: 'LOCATION_BOTTOM',
    get: function get() {
      return -1;
    }
  }, {
    key: 'NESTING_OFFSET',
    get: function get() {
      return 15;
    }
  }]);

  function Tuplet(notes, options) {
    _classCallCheck(this, Tuplet);

    var _this = _possibleConstructorReturn(this, (Tuplet.__proto__ || Object.getPrototypeOf(Tuplet)).call(this));

    _this.setAttribute('type', 'Tuplet');
    if (!notes || !notes.length) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'No notes provided for tuplet.');
    }

    _this.options = _vex.Vex.Merge({}, options);
    _this.notes = notes;
    _this.num_notes = 'num_notes' in _this.options ? _this.options.num_notes : notes.length;

    // We accept beats_occupied, but warn that it's deprecated:
    // the preferred property name is now notes_occupied.
    if (_this.options.beats_occupied) {
      _this.beatsOccupiedDeprecationWarning();
    }
    _this.notes_occupied = _this.options.notes_occupied || _this.options.beats_occupied || 2;
    if ('bracketed' in _this.options) {
      _this.bracketed = _this.options.bracketed;
    } else {
      _this.bracketed = notes.some(function (note) {
        return note.beam === null;
      });
    }

    _this.ratioed = 'ratioed' in _this.options ? _this.options.ratioed : Math.abs(_this.notes_occupied - _this.num_notes) > 1;
    _this.point = 28;
    _this.y_pos = 16;
    _this.x_pos = 100;
    _this.width = 200;
    _this.location = _this.options.location || Tuplet.LOCATION_TOP;

    _formatter.Formatter.AlignRestsToNotes(notes, true, true);
    _this.resolveGlyphs();
    _this.attach();
    return _this;
  }

  _createClass(Tuplet, [{
    key: 'attach',
    value: function attach() {
      for (var i = 0; i < this.notes.length; i++) {
        var note = this.notes[i];
        note.setTuplet(this);
      }
    }
  }, {
    key: 'detach',
    value: function detach() {
      for (var i = 0; i < this.notes.length; i++) {
        var note = this.notes[i];
        note.resetTuplet(this);
      }
    }

    /**
     * Set whether or not the bracket is drawn.
     */

  }, {
    key: 'setBracketed',
    value: function setBracketed(bracketed) {
      this.bracketed = !!bracketed;
      return this;
    }

    /**
     * Set whether or not the ratio is shown.
     */

  }, {
    key: 'setRatioed',
    value: function setRatioed(ratioed) {
      this.ratioed = !!ratioed;
      return this;
    }

    /**
     * Set the tuplet to be displayed either on the top or bottom of the stave
     */

  }, {
    key: 'setTupletLocation',
    value: function setTupletLocation(location) {
      if (!location) {
        location = Tuplet.LOCATION_TOP;
      } else if (location !== Tuplet.LOCATION_TOP && location !== Tuplet.LOCATION_BOTTOM) {
        throw new _vex.Vex.RERR('BadArgument', 'Invalid tuplet location: ' + location);
      }

      this.location = location;
      return this;
    }
  }, {
    key: 'getNotes',
    value: function getNotes() {
      return this.notes;
    }
  }, {
    key: 'getNoteCount',
    value: function getNoteCount() {
      return this.num_notes;
    }
  }, {
    key: 'beatsOccupiedDeprecationWarning',
    value: function beatsOccupiedDeprecationWarning() {
      var msg = ['beats_occupied has been deprecated as an ', 'option for tuplets. Please use notes_occupied ', 'instead. Calls to getBeatsOccupied and ', 'setBeatsOccupied should now be routed to ', 'getNotesOccupied and setNotesOccupied instead'].join('');

      if (console && console.warn) {
        // eslint-disable-line no-console
        console.warn(msg); // eslint-disable-line no-console
      } else if (console) {
        console.log(msg); // eslint-disable-line no-console
      }
    }
  }, {
    key: 'getBeatsOccupied',
    value: function getBeatsOccupied() {
      this.beatsOccupiedDeprecationWarning();
      return this.getNotesOccupied();
    }
  }, {
    key: 'setBeatsOccupied',
    value: function setBeatsOccupied(beats) {
      this.beatsOccupiedDeprecationWarning();
      return this.setNotesOccupied(beats);
    }
  }, {
    key: 'getNotesOccupied',
    value: function getNotesOccupied() {
      return this.notes_occupied;
    }
  }, {
    key: 'setNotesOccupied',
    value: function setNotesOccupied(notes) {
      this.detach();
      this.notes_occupied = notes;
      this.resolveGlyphs();
      this.attach();
    }
  }, {
    key: 'resolveGlyphs',
    value: function resolveGlyphs() {
      this.num_glyphs = [];
      var n = this.num_notes;
      while (n >= 1) {
        this.num_glyphs.push(new _glyph.Glyph('v' + n % 10, this.point));
        n = parseInt(n / 10, 10);
      }

      this.denom_glyphs = [];
      n = this.notes_occupied;
      while (n >= 1) {
        this.denom_glyphs.push(new _glyph.Glyph('v' + n % 10, this.point));
        n = parseInt(n / 10, 10);
      }
    }

    // determine how many tuplets are nested within this tuplet
    // on the same side (above/below), to calculate a y
    // offset for this tuplet:

  }, {
    key: 'getNestedTupletCount',
    value: function getNestedTupletCount() {
      var location = this.location;
      var first_note = this.notes[0];
      var maxTupletCount = countTuplets(first_note, location);
      var minTupletCount = countTuplets(first_note, location);

      // Count the tuplets that are on the same side (above/below)
      // as this tuplet:
      function countTuplets(note, location) {
        return note.tupletStack.filter(function (tuplet) {
          return tuplet.location === location;
        }).length;
      }

      this.notes.forEach(function (note) {
        var tupletCount = countTuplets(note, location);
        maxTupletCount = tupletCount > maxTupletCount ? tupletCount : maxTupletCount;
        minTupletCount = tupletCount < minTupletCount ? tupletCount : minTupletCount;
      });

      return maxTupletCount - minTupletCount;
    }

    // determine the y position of the tuplet:

  }, {
    key: 'getYPosition',
    value: function getYPosition() {
      // offset the tuplet for any nested tuplets between
      // it and the notes:
      var nested_tuplet_y_offset = this.getNestedTupletCount() * Tuplet.NESTING_OFFSET * -this.location;

      // offset the tuplet for any manual y_offset:
      var y_offset = this.options.y_offset || 0;

      // now iterate through the notes and find our highest
      // or lowest locations, to form a base y_pos
      var first_note = this.notes[0];
      var y_pos = void 0;
      if (this.location === Tuplet.LOCATION_TOP) {
        y_pos = first_note.getStave().getYForLine(0) - 15;
        // y_pos = first_note.getStemExtents().topY - 10;

        for (var i = 0; i < this.notes.length; ++i) {
          var top_y = this.notes[i].getStemDirection() === _stem.Stem.UP ? this.notes[i].getStemExtents().topY - 10 : this.notes[i].getStemExtents().baseY - 20;

          if (top_y < y_pos) {
            y_pos = top_y;
          }
        }
      } else {
        y_pos = first_note.getStave().getYForLine(4) + 20;

        for (var _i = 0; _i < this.notes.length; ++_i) {
          var bottom_y = this.notes[_i].getStemDirection() === _stem.Stem.UP ? this.notes[_i].getStemExtents().baseY + 20 : this.notes[_i].getStemExtents().topY + 10;
          if (bottom_y > y_pos) {
            y_pos = bottom_y;
          }
        }
      }

      return y_pos + nested_tuplet_y_offset + y_offset;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var _this2 = this;

      this.checkContext();
      this.setRendered();

      // determine x value of left bound of tuplet
      var first_note = this.notes[0];
      var last_note = this.notes[this.notes.length - 1];

      if (!this.bracketed) {
        this.x_pos = first_note.getStemX();
        this.width = last_note.getStemX() - this.x_pos;
      } else {
        this.x_pos = first_note.getTieLeftX() - 5;
        this.width = last_note.getTieRightX() - this.x_pos + 5;
      }

      // determine y value for tuplet
      this.y_pos = this.getYPosition();

      var addGlyphWidth = function addGlyphWidth(width, glyph) {
        return width + glyph.getMetrics().width;
      };

      // calculate total width of tuplet notation
      var width = this.num_glyphs.reduce(addGlyphWidth, 0);
      if (this.ratioed) {
        width = this.denom_glyphs.reduce(addGlyphWidth, width);
        width += this.point * 0.32;
      }

      var notation_center_x = this.x_pos + this.width / 2;
      var notation_start_x = notation_center_x - width / 2;

      // draw bracket if the tuplet is not beamed
      if (this.bracketed) {
        var line_width = this.width / 2 - width / 2 - 5;

        // only draw the bracket if it has positive length
        if (line_width > 0) {
          this.context.fillRect(this.x_pos, this.y_pos, line_width, 1);
          this.context.fillRect(this.x_pos + this.width / 2 + width / 2 + 5, this.y_pos, line_width, 1);
          this.context.fillRect(this.x_pos, this.y_pos + (this.location === Tuplet.LOCATION_BOTTOM), 1, this.location * 10);
          this.context.fillRect(this.x_pos + this.width, this.y_pos + (this.location === Tuplet.LOCATION_BOTTOM), 1, this.location * 10);
        }
      }

      // draw numerator glyphs
      var x_offset = 0;
      this.num_glyphs.forEach(function (glyph) {
        glyph.render(_this2.context, notation_start_x + x_offset, _this2.y_pos + _this2.point / 3 - 2);
        x_offset += glyph.getMetrics().width;
      });

      // display colon and denominator if the ratio is to be shown
      if (this.ratioed) {
        var colon_x = notation_start_x + x_offset + this.point * 0.16;
        var colon_radius = this.point * 0.06;
        this.context.beginPath();
        this.context.arc(colon_x, this.y_pos - this.point * 0.08, colon_radius, 0, Math.PI * 2, true);
        this.context.closePath();
        this.context.fill();
        this.context.beginPath();
        this.context.arc(colon_x, this.y_pos + this.point * 0.12, colon_radius, 0, Math.PI * 2, true);
        this.context.closePath();
        this.context.fill();
        x_offset += this.point * 0.32;
        this.denom_glyphs.forEach(function (glyph) {
          glyph.render(_this2.context, notation_start_x + x_offset, _this2.y_pos + _this2.point / 3 - 2);
          x_offset += glyph.getMetrics().width;
        });
      }
    }
  }]);

  return Tuplet;
}(_element.Element);

/***/ }),
/* 19 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveConnector = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _tables = __webpack_require__(1);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

function drawBoldDoubleLine(ctx, type, topX, topY, botY) {
  if (type !== StaveConnector.type.BOLD_DOUBLE_LEFT && type !== StaveConnector.type.BOLD_DOUBLE_RIGHT) {
    throw new _vex.Vex.RERR('InvalidConnector', 'A REPEAT_BEGIN or REPEAT_END type must be provided.');
  }

  var x_shift = 3;
  var variableWidth = 3.5; // Width for avoiding anti-aliasing width issues
  var thickLineOffset = 2; // For aesthetics

  if (type === StaveConnector.type.BOLD_DOUBLE_RIGHT) {
    x_shift = -5; // Flips the side of the thin line
    variableWidth = 3;
  }

  // Thin line
  ctx.fillRect(topX + x_shift, topY, 1, botY - topY);
  // Thick line
  ctx.fillRect(topX - thickLineOffset, topY, variableWidth, botY - topY);
}

var StaveConnector = exports.StaveConnector = function (_Element) {
  _inherits(StaveConnector, _Element);

  _createClass(StaveConnector, null, [{
    key: 'type',

    // SINGLE_LEFT and SINGLE are the same value for compatibility
    // with older versions of vexflow which didn't have right sided
    // stave connectors
    get: function get() {
      return {
        SINGLE_RIGHT: 0,
        SINGLE_LEFT: 1,
        SINGLE: 1,
        DOUBLE: 2,
        BRACE: 3,
        BRACKET: 4,
        BOLD_DOUBLE_LEFT: 5,
        BOLD_DOUBLE_RIGHT: 6,
        THIN_DOUBLE: 7,
        NONE: 8
      };
    }
  }, {
    key: 'typeString',
    get: function get() {
      return {
        singleRight: StaveConnector.type.SINGLE_RIGHT,
        singleLeft: StaveConnector.type.SINGLE_LEFT,
        single: StaveConnector.type.SINGLE,
        double: StaveConnector.type.DOUBLE,
        brace: StaveConnector.type.BRACE,
        bracket: StaveConnector.type.BRACKET,
        boldDoubleLeft: StaveConnector.type.BOLD_DOUBLE_LEFT,
        boldDoubleRight: StaveConnector.type.BOLD_DOUBLE_RIGHT,
        thinDouble: StaveConnector.type.THIN_DOUBLE,
        none: StaveConnector.type.NONE
      };
    }
  }]);

  function StaveConnector(top_stave, bottom_stave) {
    _classCallCheck(this, StaveConnector);

    var _this = _possibleConstructorReturn(this, (StaveConnector.__proto__ || Object.getPrototypeOf(StaveConnector)).call(this));

    _this.setAttribute('type', 'StaveConnector');

    _this.thickness = _tables.Flow.STAVE_LINE_THICKNESS;
    _this.width = 3;
    _this.top_stave = top_stave;
    _this.bottom_stave = bottom_stave;
    _this.type = StaveConnector.type.DOUBLE;
    _this.font = {
      family: 'times',
      size: 16,
      weight: 'normal'
    };
    // 1. Offset Bold Double Left to align with offset Repeat Begin bars
    // 2. Offset BRACE type not to overlap with another StaveConnector
    _this.x_shift = 0;
    _this.texts = [];
    return _this;
  }

  _createClass(StaveConnector, [{
    key: 'setType',
    value: function setType(type) {
      type = typeof type === 'string' ? StaveConnector.typeString[type] : type;

      if (type >= StaveConnector.type.SINGLE_RIGHT && type <= StaveConnector.type.NONE) {
        this.type = type;
      }
      return this;
    }
  }, {
    key: 'setText',
    value: function setText(text, options) {
      this.texts.push({
        content: text,
        options: _vex.Vex.Merge({ shift_x: 0, shift_y: 0 }, options)
      });
      return this;
    }
  }, {
    key: 'setFont',
    value: function setFont(font) {
      _vex.Vex.Merge(this.font, font);
    }
  }, {
    key: 'setXShift',
    value: function setXShift(x_shift) {
      if (typeof x_shift !== 'number') {
        throw _vex.Vex.RERR('InvalidType', 'x_shift must be a Number');
      }

      this.x_shift = x_shift;
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var ctx = this.checkContext();
      this.setRendered();

      var topY = this.top_stave.getYForLine(0);
      var botY = this.bottom_stave.getYForLine(this.bottom_stave.getNumLines() - 1) + this.thickness;
      var width = this.width;
      var topX = this.top_stave.getX();

      var isRightSidedConnector = this.type === StaveConnector.type.SINGLE_RIGHT || this.type === StaveConnector.type.BOLD_DOUBLE_RIGHT || this.type === StaveConnector.type.THIN_DOUBLE;

      if (isRightSidedConnector) {
        topX = this.top_stave.getX() + this.top_stave.width;
      }

      var attachment_height = botY - topY;
      switch (this.type) {
        case StaveConnector.type.SINGLE:
          width = 1;
          break;
        case StaveConnector.type.SINGLE_LEFT:
          width = 1;
          break;
        case StaveConnector.type.SINGLE_RIGHT:
          width = 1;
          break;
        case StaveConnector.type.DOUBLE:
          topX -= this.width + 2;
          break;
        case StaveConnector.type.BRACE:
          {
            width = 12;
            // May need additional code to draw brace
            var x1 = this.top_stave.getX() - 2 + this.x_shift;
            var y1 = topY;
            var x3 = x1;
            var y3 = botY;
            var x2 = x1 - width;
            var y2 = y1 + attachment_height / 2.0;
            var cpx1 = x2 - 0.90 * width;
            var cpy1 = y1 + 0.2 * attachment_height;
            var cpx2 = x1 + 1.10 * width;
            var cpy2 = y2 - 0.135 * attachment_height;
            var cpx3 = cpx2;
            var cpy3 = y2 + 0.135 * attachment_height;
            var cpx4 = cpx1;
            var cpy4 = y3 - 0.2 * attachment_height;
            var cpx5 = x2 - width;
            var cpy5 = cpy4;
            var cpx6 = x1 + 0.40 * width;
            var cpy6 = y2 + 0.135 * attachment_height;
            var cpx7 = cpx6;
            var cpy7 = y2 - 0.135 * attachment_height;
            var cpx8 = cpx5;
            var cpy8 = cpy1;
            ctx.beginPath();
            ctx.moveTo(x1, y1);
            ctx.bezierCurveTo(cpx1, cpy1, cpx2, cpy2, x2, y2);
            ctx.bezierCurveTo(cpx3, cpy3, cpx4, cpy4, x3, y3);
            ctx.bezierCurveTo(cpx5, cpy5, cpx6, cpy6, x2, y2);
            ctx.bezierCurveTo(cpx7, cpy7, cpx8, cpy8, x1, y1);
            ctx.fill();
            ctx.stroke();
            break;
          }case StaveConnector.type.BRACKET:
          topY -= 4;
          botY += 4;
          attachment_height = botY - topY;
          _glyph.Glyph.renderGlyph(ctx, topX - 5, topY - 3, 40, 'v1b', true);
          _glyph.Glyph.renderGlyph(ctx, topX - 5, botY + 3, 40, 'v10', true);
          topX -= this.width + 2;
          break;
        case StaveConnector.type.BOLD_DOUBLE_LEFT:
          drawBoldDoubleLine(ctx, this.type, topX + this.x_shift, topY, botY);
          break;
        case StaveConnector.type.BOLD_DOUBLE_RIGHT:
          drawBoldDoubleLine(ctx, this.type, topX, topY, botY);
          break;
        case StaveConnector.type.THIN_DOUBLE:
          width = 1;
          break;
        case StaveConnector.type.NONE:
          break;
        default:
          throw new _vex.Vex.RERR('InvalidType', 'The provided StaveConnector.type (' + this.type + ') is invalid');
      }

      if (this.type !== StaveConnector.type.BRACE && this.type !== StaveConnector.type.BOLD_DOUBLE_LEFT && this.type !== StaveConnector.type.BOLD_DOUBLE_RIGHT && this.type !== StaveConnector.type.NONE) {
        ctx.fillRect(topX, topY, width, attachment_height);
      }

      // If the connector is a thin double barline, draw the paralell line
      if (this.type === StaveConnector.type.THIN_DOUBLE) {
        ctx.fillRect(topX - 3, topY, width, attachment_height);
      }

      ctx.save();
      ctx.lineWidth = 2;
      ctx.setFont(this.font.family, this.font.size, this.font.weight);
      // Add stave connector text
      for (var i = 0; i < this.texts.length; i++) {
        var text = this.texts[i];
        var text_width = ctx.measureText('' + text.content).width;
        var x = this.top_stave.getX() - text_width - 24 + text.options.shift_x;
        var y = (this.top_stave.getYForLine(0) + this.bottom_stave.getBottomLineY()) / 2 + text.options.shift_y;

        ctx.fillText('' + text.content, x, y + 4);
      }
      ctx.restore();
    }
  }]);

  return StaveConnector;
}(_element.Element);

/***/ }),
/* 20 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StemmableNote = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _stem = __webpack_require__(9);

var _glyph = __webpack_require__(2);

var _note = __webpack_require__(6);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// `StemmableNote` is an abstract interface for notes with optional stems.
// Examples of stemmable notes are `StaveNote` and `TabNote`

var StemmableNote = exports.StemmableNote = function (_Note) {
  _inherits(StemmableNote, _Note);

  function StemmableNote(note_struct) {
    _classCallCheck(this, StemmableNote);

    var _this = _possibleConstructorReturn(this, (StemmableNote.__proto__ || Object.getPrototypeOf(StemmableNote)).call(this, note_struct));

    _this.setAttribute('type', 'StemmableNote');

    _this.stem = null;
    _this.stemExtensionOverride = null;
    _this.beam = null;
    return _this;
  }

  // Get and set the note's `Stem`


  _createClass(StemmableNote, [{
    key: 'getStem',
    value: function getStem() {
      return this.stem;
    }
  }, {
    key: 'setStem',
    value: function setStem(stem) {
      this.stem = stem;return this;
    }

    // Builds and sets a new stem

  }, {
    key: 'buildStem',
    value: function buildStem() {
      var stem = new _stem.Stem();
      this.setStem(stem);
      return this;
    }
  }, {
    key: 'buildFlag',
    value: function buildFlag() {
      var glyph = this.glyph,
          beam = this.beam;

      var shouldRenderFlag = beam === null;

      if (glyph && glyph.flag && shouldRenderFlag) {
        var flagCode = this.getStemDirection() === _stem.Stem.DOWN ? glyph.code_flag_downstem : glyph.code_flag_upstem;

        this.flag = new _glyph.Glyph(flagCode, this.render_options.glyph_font_scale);
      }
    }

    // Get the full length of stem

  }, {
    key: 'getStemLength',
    value: function getStemLength() {
      return _stem.Stem.HEIGHT + this.getStemExtension();
    }

    // Get the number of beams for this duration

  }, {
    key: 'getBeamCount',
    value: function getBeamCount() {
      var glyph = this.getGlyph();

      if (glyph) {
        return glyph.beam_count;
      } else {
        return 0;
      }
    }

    // Get the minimum length of stem

  }, {
    key: 'getStemMinumumLength',
    value: function getStemMinumumLength() {
      var frac = _tables.Flow.durationToFraction(this.duration);
      var length = frac.value() <= 1 ? 0 : 20;
      // if note is flagged, cannot shorten beam
      switch (this.duration) {
        case '8':
          if (this.beam == null) length = 35;
          break;
        case '16':
          length = this.beam == null ? 35 : 25;
          break;
        case '32':
          length = this.beam == null ? 45 : 35;
          break;
        case '64':
          length = this.beam == null ? 50 : 40;
          break;
        case '128':
          length = this.beam == null ? 55 : 45;
          break;
        default:
          break;
      }
      return length;
    }

    // Get/set the direction of the stem

  }, {
    key: 'getStemDirection',
    value: function getStemDirection() {
      return this.stem_direction;
    }
  }, {
    key: 'setStemDirection',
    value: function setStemDirection(direction) {
      if (!direction) direction = _stem.Stem.UP;
      if (direction !== _stem.Stem.UP && direction !== _stem.Stem.DOWN) {
        throw new _vex.Vex.RERR('BadArgument', 'Invalid stem direction: ' + direction);
      }

      this.stem_direction = direction;
      if (this.stem) {
        this.stem.setDirection(direction);
        this.stem.setExtension(this.getStemExtension());
      }

      this.reset();
      if (this.flag) {
        this.buildFlag();
      }

      this.beam = null;
      if (this.preFormatted) {
        this.preFormat();
      }
      return this;
    }

    // Get the `x` coordinate of the stem

  }, {
    key: 'getStemX',
    value: function getStemX() {
      var x_begin = this.getAbsoluteX() + this.x_shift;
      var x_end = this.getAbsoluteX() + this.x_shift + this.getGlyphWidth();
      var stem_x = this.stem_direction === _stem.Stem.DOWN ? x_begin : x_end;
      return stem_x;
    }

    // Get the `x` coordinate for the center of the glyph.
    // Used for `TabNote` stems and stemlets over rests

  }, {
    key: 'getCenterGlyphX',
    value: function getCenterGlyphX() {
      return this.getAbsoluteX() + this.x_shift + this.getGlyphWidth() / 2;
    }

    // Get the stem extension for the current duration

  }, {
    key: 'getStemExtension',
    value: function getStemExtension() {
      var glyph = this.getGlyph();

      if (this.stemExtensionOverride != null) {
        return this.stemExtensionOverride;
      }

      if (glyph) {
        return this.getStemDirection() === 1 ? glyph.stem_up_extension : glyph.stem_down_extension;
      }

      return 0;
    }

    // Set the stem length to a specific. Will override the default length.

  }, {
    key: 'setStemLength',
    value: function setStemLength(height) {
      this.stemExtensionOverride = height - _stem.Stem.HEIGHT;
      return this;
    }

    // Get the top and bottom `y` values of the stem.

  }, {
    key: 'getStemExtents',
    value: function getStemExtents() {
      return this.stem.getExtents();
    }

    // Sets the current note's beam

  }, {
    key: 'setBeam',
    value: function setBeam(beam) {
      this.beam = beam;return this;
    }

    // Get the `y` value for the top/bottom modifiers at a specific `textLine`

  }, {
    key: 'getYForTopText',
    value: function getYForTopText(textLine) {
      var extents = this.getStemExtents();
      if (this.hasStem()) {
        return Math.min(this.stave.getYForTopText(textLine), extents.topY - this.render_options.annotation_spacing * (textLine + 1));
      } else {
        return this.stave.getYForTopText(textLine);
      }
    }
  }, {
    key: 'getYForBottomText',
    value: function getYForBottomText(textLine) {
      var extents = this.getStemExtents();
      if (this.hasStem()) {
        return Math.max(this.stave.getYForTopText(textLine), extents.baseY + this.render_options.annotation_spacing * textLine);
      } else {
        return this.stave.getYForBottomText(textLine);
      }
    }
  }, {
    key: 'hasFlag',
    value: function hasFlag() {
      return _tables.Flow.durationToGlyph(this.duration).flag && !this.beam;
    }

    // Post format the note

  }, {
    key: 'postFormat',
    value: function postFormat() {
      if (this.beam) this.beam.postFormat();

      this.postFormatted = true;

      return this;
    }

    // Render the stem onto the canvas

  }, {
    key: 'drawStem',
    value: function drawStem(stem_struct) {
      this.checkContext();
      this.setRendered();

      this.setStem(new _stem.Stem(stem_struct));
      this.stem.setContext(this.context).draw();
    }
  }]);

  return StemmableNote;
}(_note.Note);

/***/ }),
/* 21 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Dot = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _modifier = __webpack_require__(4);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // VexFlow - Music Engraving for HTML5
// Copyright Mohit Muthanna 2010
//
// This class implements dot modifiers for notes.

var Dot = exports.Dot = function (_Modifier) {
  _inherits(Dot, _Modifier);

  _createClass(Dot, null, [{
    key: 'format',


    // Arrange dots inside a ModifierContext.
    value: function format(dots, state) {
      var right_shift = state.right_shift;
      var dot_spacing = 1;

      if (!dots || dots.length === 0) return false;

      var dot_list = [];
      for (var i = 0; i < dots.length; ++i) {
        var dot = dots[i];
        var note = dot.getNote();

        var props = void 0;
        var shift = void 0;
        // Only StaveNote has .getKeyProps()
        if (typeof note.getKeyProps === 'function') {
          props = note.getKeyProps()[dot.getIndex()];
          shift = props.displaced ? note.getExtraRightPx() : 0;
        } else {
          // Else it's a TabNote
          props = { line: 0.5 }; // Shim key props for dot placement
          shift = 0;
        }

        dot_list.push({ line: props.line, shift: shift, note: note, dot: dot });
      }

      // Sort dots by line number.
      dot_list.sort(function (a, b) {
        return b.line - a.line;
      });

      var dot_shift = right_shift;
      var x_width = 0;
      var last_line = null;
      var last_note = null;
      var prev_dotted_space = null;
      var half_shiftY = 0;

      for (var _i = 0; _i < dot_list.length; ++_i) {
        var _dot_list$_i = dot_list[_i],
            _dot = _dot_list$_i.dot,
            _note = _dot_list$_i.note,
            _shift = _dot_list$_i.shift,
            line = _dot_list$_i.line;

        // Reset the position of the dot every line.

        if (line !== last_line || _note !== last_note) {
          dot_shift = _shift;
        }

        if (!_note.isRest() && line !== last_line) {
          if (Math.abs(line % 1) === 0.5) {
            // note is on a space, so no dot shift
            half_shiftY = 0;
          } else {
            // note is on a line, so shift dot to space above the line
            half_shiftY = 0.5;
            if (last_note != null && !last_note.isRest() && last_line - line === 0.5) {
              // previous note on a space, so shift dot to space below the line
              half_shiftY = -0.5;
            } else if (line + half_shiftY === prev_dotted_space) {
              // previous space is dotted, so shift dot to space below the line
              half_shiftY = -0.5;
            }
          }
        }

        // convert half_shiftY to a multiplier for dots.draw()
        if (_note.isRest()) {
          _dot.dot_shiftY += -half_shiftY;
        } else {
          _dot.dot_shiftY = -half_shiftY;
        }
        prev_dotted_space = line + half_shiftY;

        _dot.setXShift(dot_shift);
        dot_shift += _dot.getWidth() + dot_spacing; // spacing
        x_width = dot_shift > x_width ? dot_shift : x_width;
        last_line = line;
        last_note = _note;
      }

      // Update state.
      state.right_shift += x_width;
      return true;
    }

    /**
     * @constructor
     */

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'dots';
    }
  }]);

  function Dot() {
    _classCallCheck(this, Dot);

    var _this = _possibleConstructorReturn(this, (Dot.__proto__ || Object.getPrototypeOf(Dot)).call(this));

    _this.setAttribute('type', 'Dot');

    _this.note = null;
    _this.index = null;
    _this.position = _modifier.Modifier.Position.RIGHT;

    _this.radius = 2;
    _this.setWidth(5);
    _this.dot_shiftY = 0;
    return _this;
  }

  _createClass(Dot, [{
    key: 'getCategory',
    value: function getCategory() {
      return Dot.CATEGORY;
    }
  }, {
    key: 'setNote',
    value: function setNote(note) {
      this.note = note;

      if (this.note.getCategory() === 'gracenotes') {
        this.radius *= 0.50;
        this.setWidth(3);
      }
    }
  }, {
    key: 'setDotShiftY',
    value: function setDotShiftY(y) {
      this.dot_shiftY = y;return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      if (!this.note || this.index === null) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw dot without a note and index.");
      }

      var lineSpace = this.note.stave.options.spacing_between_lines_px;

      var start = this.note.getModifierStartXY(this.position, this.index);

      // Set the starting y coordinate to the base of the stem for TabNotes
      if (this.note.getCategory() === 'tabnotes') {
        start.y = this.note.getStemExtents().baseY;
      }

      var x = start.x + this.x_shift + this.width - this.radius;
      var y = start.y + this.y_shift + this.dot_shiftY * lineSpace;
      var ctx = this.context;

      ctx.beginPath();
      ctx.arc(x, y, this.radius, 0, Math.PI * 2, false);
      ctx.fill();
    }
  }]);

  return Dot;
}(_modifier.Modifier);

/***/ }),
/* 22 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ModifierContext = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This class implements various types of modifiers to notes (e.g. bends,
// fingering positions etc.)

var _vex = __webpack_require__(0);

var _stavenote = __webpack_require__(5);

var _dot = __webpack_require__(21);

var _frethandfinger = __webpack_require__(23);

var _accidental = __webpack_require__(24);

var _notesubgroup = __webpack_require__(26);

var _gracenotegroup = __webpack_require__(27);

var _strokes = __webpack_require__(43);

var _stringnumber = __webpack_require__(29);

var _articulation = __webpack_require__(17);

var _ornament = __webpack_require__(44);

var _annotation = __webpack_require__(30);

var _bend = __webpack_require__(31);

var _vibrato = __webpack_require__(32);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// To enable logging for this class. Set `Vex.Flow.ModifierContext.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (ModifierContext.DEBUG) _vex.Vex.L('Vex.Flow.ModifierContext', args);
}

var ModifierContext = exports.ModifierContext = function () {
  function ModifierContext() {
    _classCallCheck(this, ModifierContext);

    // Current modifiers
    this.modifiers = {};

    // Formatting data.
    this.preFormatted = false;
    this.postFormatted = false;
    this.width = 0;
    this.spacing = 0;
    this.state = {
      left_shift: 0,
      right_shift: 0,
      text_line: 0,
      top_text_line: 0
    };

    // Add new modifiers to this array. The ordering is significant -- lower
    // modifiers are formatted and rendered before higher ones.
    this.PREFORMAT = [_stavenote.StaveNote, _dot.Dot, _frethandfinger.FretHandFinger, _accidental.Accidental, _gracenotegroup.GraceNoteGroup, _notesubgroup.NoteSubGroup, _strokes.Stroke, _stringnumber.StringNumber, _articulation.Articulation, _ornament.Ornament, _annotation.Annotation, _bend.Bend, _vibrato.Vibrato];

    // If post-formatting is required for an element, add it to this array.
    this.POSTFORMAT = [_stavenote.StaveNote];
  }

  _createClass(ModifierContext, [{
    key: 'addModifier',
    value: function addModifier(modifier) {
      var type = modifier.getCategory();
      if (!this.modifiers[type]) this.modifiers[type] = [];
      this.modifiers[type].push(modifier);
      modifier.setModifierContext(this);
      this.preFormatted = false;
      return this;
    }
  }, {
    key: 'getModifiers',
    value: function getModifiers(type) {
      return this.modifiers[type];
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'getExtraLeftPx',
    value: function getExtraLeftPx() {
      return this.state.left_shift;
    }
  }, {
    key: 'getExtraRightPx',
    value: function getExtraRightPx() {
      return this.state.right_shift;
    }
  }, {
    key: 'getState',
    value: function getState() {
      return this.state;
    }
  }, {
    key: 'getMetrics',
    value: function getMetrics() {
      if (!this.formatted) {
        throw new _vex.Vex.RERR('UnformattedModifier', 'Unformatted modifier has no metrics.');
      }

      return {
        width: this.state.left_shift + this.state.right_shift + this.spacing,
        spacing: this.spacing,
        extra_left_px: this.state.left_shift,
        extra_right_px: this.state.right_shift
      };
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      var _this = this;

      if (this.preFormatted) return;
      this.PREFORMAT.forEach(function (modifier) {
        L('Preformatting ModifierContext: ', modifier.CATEGORY);
        modifier.format(_this.getModifiers(modifier.CATEGORY), _this.state, _this);
      });

      // Update width of this modifier context
      this.width = this.state.left_shift + this.state.right_shift;
      this.preFormatted = true;
    }
  }, {
    key: 'postFormat',
    value: function postFormat() {
      var _this2 = this;

      if (this.postFormatted) return;
      this.POSTFORMAT.forEach(function (modifier) {
        L('Postformatting ModifierContext: ', modifier.CATEGORY);
        modifier.postFormat(_this2.getModifiers(modifier.CATEGORY), _this2);
      });
    }
  }]);

  return ModifierContext;
}();

/***/ }),
/* 23 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.FretHandFinger = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _modifier = __webpack_require__(4);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // VexFlow - Music Engraving for HTML5
// Copyright Mohit Muthanna 2010
// Author Larry Kuhns 2013
// Class to draws string numbers into the notation.

/**
 * @constructor
 */
var FretHandFinger = exports.FretHandFinger = function (_Modifier) {
  _inherits(FretHandFinger, _Modifier);

  _createClass(FretHandFinger, null, [{
    key: 'format',


    // Arrange fingerings inside a ModifierContext.
    value: function format(nums, state) {
      var left_shift = state.left_shift,
          right_shift = state.right_shift;

      var num_spacing = 1;

      if (!nums || nums.length === 0) return false;

      var nums_list = [];
      var prev_note = null;
      var shiftLeft = 0;
      var shiftRight = 0;

      for (var i = 0; i < nums.length; ++i) {
        var num = nums[i];
        var note = num.getNote();
        var pos = num.getPosition();
        var props = note.getKeyProps()[num.getIndex()];
        if (note !== prev_note) {
          for (var n = 0; n < note.keys.length; ++n) {
            var props_tmp = note.getKeyProps()[n];
            if (left_shift === 0) {
              shiftLeft = props_tmp.displaced ? note.getExtraLeftPx() : shiftLeft;
            }
            if (right_shift === 0) {
              shiftRight = props_tmp.displaced ? note.getExtraRightPx() : shiftRight;
            }
          }
          prev_note = note;
        }

        nums_list.push({
          note: note,
          num: num,
          pos: pos,
          line: props.line,
          shiftL: shiftLeft,
          shiftR: shiftRight
        });
      }

      // Sort fingernumbers by line number.
      nums_list.sort(function (a, b) {
        return b.line - a.line;
      });

      var numShiftL = 0;
      var numShiftR = 0;
      var xWidthL = 0;
      var xWidthR = 0;
      var lastLine = null;
      var lastNote = null;

      for (var _i = 0; _i < nums_list.length; ++_i) {
        var num_shift = 0;
        var _nums_list$_i = nums_list[_i],
            _note = _nums_list$_i.note,
            _pos = _nums_list$_i.pos,
            _num = _nums_list$_i.num,
            line = _nums_list$_i.line,
            shiftL = _nums_list$_i.shiftL,
            shiftR = _nums_list$_i.shiftR;

        // Reset the position of the string number every line.

        if (line !== lastLine || _note !== lastNote) {
          numShiftL = left_shift + shiftL;
          numShiftR = right_shift + shiftR;
        }

        var numWidth = _num.getWidth() + num_spacing;
        if (_pos === _modifier.Modifier.Position.LEFT) {
          _num.setXShift(left_shift + numShiftL);
          num_shift = left_shift + numWidth; // spacing
          xWidthL = num_shift > xWidthL ? num_shift : xWidthL;
        } else if (_pos === _modifier.Modifier.Position.RIGHT) {
          _num.setXShift(numShiftR);
          num_shift = shiftRight + numWidth; // spacing
          xWidthR = num_shift > xWidthR ? num_shift : xWidthR;
        }
        lastLine = line;
        lastNote = _note;
      }

      state.left_shift += xWidthL;
      state.right_shift += xWidthR;

      return true;
    }
  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'frethandfinger';
    }
  }]);

  function FretHandFinger(number) {
    _classCallCheck(this, FretHandFinger);

    var _this = _possibleConstructorReturn(this, (FretHandFinger.__proto__ || Object.getPrototypeOf(FretHandFinger)).call(this));

    _this.setAttribute('type', 'FretHandFinger');

    _this.note = null;
    _this.index = null;
    _this.finger = number;
    _this.width = 7;
    _this.position = _modifier.Modifier.Position.LEFT; // Default position above stem or note head
    _this.x_shift = 0;
    _this.y_shift = 0;
    _this.x_offset = 0; // Horizontal offset from default
    _this.y_offset = 0; // Vertical offset from default
    _this.font = {
      family: 'sans-serif',
      size: 9,
      weight: 'bold'
    };
    return _this;
  }

  _createClass(FretHandFinger, [{
    key: 'getCategory',
    value: function getCategory() {
      return FretHandFinger.CATEGORY;
    }
  }, {
    key: 'setFretHandFinger',
    value: function setFretHandFinger(number) {
      this.finger = number;return this;
    }
  }, {
    key: 'setOffsetX',
    value: function setOffsetX(x) {
      this.x_offset = x;return this;
    }
  }, {
    key: 'setOffsetY',
    value: function setOffsetY(y) {
      this.y_offset = y;return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();

      if (!this.note || this.index == null) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw string number without a note and index.");
      }

      this.setRendered();
      var ctx = this.context;
      var start = this.note.getModifierStartXY(this.position, this.index);
      var dot_x = start.x + this.x_shift + this.x_offset;
      var dot_y = start.y + this.y_shift + this.y_offset + 5;

      switch (this.position) {
        case _modifier.Modifier.Position.ABOVE:
          dot_x -= 4;
          dot_y -= 12;
          break;
        case _modifier.Modifier.Position.BELOW:
          dot_x -= 2;
          dot_y += 10;
          break;
        case _modifier.Modifier.Position.LEFT:
          dot_x -= this.width;
          break;
        case _modifier.Modifier.Position.RIGHT:
          dot_x += 1;
          break;
        default:
          throw new _vex.Vex.RERR('InvalidPostion', 'The position ' + this.position + ' does not exist');
      }

      ctx.save();
      ctx.setFont(this.font.family, this.font.size, this.font.weight);
      ctx.fillText('' + this.finger, dot_x, dot_y);
      ctx.restore();
    }
  }]);

  return FretHandFinger;
}(_modifier.Modifier);

/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Accidental = undefined;

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _fraction = __webpack_require__(8);

var _tables = __webpack_require__(1);

var _music = __webpack_require__(25);

var _modifier = __webpack_require__(4);

var _glyph = __webpack_require__(2);

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// @author Mohit Cheppudira
// @author Greg Ristow (modifications)
//
// ## Description
//
// This file implements accidentals as modifiers that can be attached to
// notes. Support is included for both western and microtonal accidentals.
//
// See `tests/accidental_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.Accidental.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Accidental.DEBUG) _vex.Vex.L('Vex.Flow.Accidental', args);
}

var getGlyphWidth = function getGlyphWidth(glyph) {
  return glyph.getMetrics().width;
};

// An `Accidental` inherits from `Modifier`, and is formatted within a
// `ModifierContext`.

var Accidental = exports.Accidental = function (_Modifier) {
  _inherits(Accidental, _Modifier);

  _createClass(Accidental, null, [{
    key: 'format',


    // Arrange accidentals inside a ModifierContext.
    value: function format(accidentals, state) {
      var _this2 = this;

      var noteheadAccidentalPadding = 1;
      var leftShift = state.left_shift + noteheadAccidentalPadding;
      var accidentalSpacing = 3;

      // If there are no accidentals, we needn't format their positions
      if (!accidentals || accidentals.length === 0) return;

      var accList = [];
      var prevNote = null;
      var shiftL = 0;

      // First determine the accidentals' Y positions from the note.keys
      var propsTemp = void 0;
      for (var i = 0; i < accidentals.length; ++i) {
        var acc = accidentals[i];
        var note = acc.getNote();
        var stave = note.getStave();
        var props = note.getKeyProps()[acc.getIndex()];
        if (note !== prevNote) {
          // Iterate through all notes to get the displaced pixels
          for (var n = 0; n < note.keys.length; ++n) {
            propsTemp = note.getKeyProps()[n];
            shiftL = propsTemp.displaced ? note.getExtraLeftPx() : shiftL;
          }
          prevNote = note;
        }
        if (stave !== null) {
          var lineSpace = stave.options.spacing_between_lines_px;
          var y = stave.getYForLine(props.line);
          var accLine = Math.round(y / lineSpace * 2) / 2;
          accList.push({ y: y, line: accLine, shift: shiftL, acc: acc, lineSpace: lineSpace });
        } else {
          accList.push({ line: props.line, shift: shiftL, acc: acc });
        }
      }

      // Sort accidentals by line number.
      accList.sort(function (a, b) {
        return b.line - a.line;
      });

      // FIXME: Confusing name. Each object in this array has a property called `line`.
      // So if this is a list of lines, you end up with: `line.line` which is very awkward.
      var lineList = [];

      // amount by which all accidentals must be shifted right or left for
      // stem flipping, notehead shifting concerns.
      var accShift = 0;
      var previousLine = null;

      // Create an array of unique line numbers (lineList) from accList
      for (var _i = 0; _i < accList.length; _i++) {
        var _acc = accList[_i];

        // if this is the first line, or a new line, add a lineList
        if (previousLine === null || previousLine !== _acc.line) {
          lineList.push({
            line: _acc.line,
            flatLine: true,
            dblSharpLine: true,
            numAcc: 0,
            width: 0
          });
        }
        // if this accidental is not a flat, the accidental needs 3.0 lines lower
        // clearance instead of 2.5 lines for b or bb.
        // FIXME: Naming could use work. acc.acc is very awkward
        if (_acc.acc.type !== 'b' && _acc.acc.type !== 'bb') {
          lineList[lineList.length - 1].flatLine = false;
        }

        // if this accidental is not a double sharp, the accidental needs 3.0 lines above
        if (_acc.acc.type !== '##') {
          lineList[lineList.length - 1].dblSharpLine = false;
        }

        // Track how many accidentals are on this line:
        lineList[lineList.length - 1].numAcc++;

        // Track the total x_offset needed for this line which will be needed
        // for formatting lines w/ multiple accidentals:

        // width = accidental width + universal spacing between accidentals
        lineList[lineList.length - 1].width += _acc.acc.getWidth() + accidentalSpacing;

        // if this accShift is larger, use it to keep first column accidentals in the same line
        accShift = _acc.shift > accShift ? _acc.shift : accShift;

        previousLine = _acc.line;
      }

      // ### Place Accidentals in Columns
      //
      // Default to a classic triangular layout (middle accidental farthest left),
      // but follow exceptions as outlined in G. Read's _Music Notation_ and
      // Elaine Gould's _Behind Bars_.
      //
      // Additionally, this implements different vertical collision rules for
      // flats (only need 2.5 lines clearance below) and double sharps (only
      // need 2.5 lines of clearance above or below).
      //
      // Classic layouts and exception patterns are found in the 'tables.js'
      // in 'Vex.Flow.accidentalColumnsTable'
      //
      // Beyond 6 vertical accidentals, default to the parallel ascending lines approach,
      // using as few columns as possible for the verticle structure.
      //
      // TODO (?): Allow column to be specified for an accidental at run-time?

      var totalColumns = 0;

      // establish the boundaries for a group of notes with clashing accidentals:

      var _loop = function _loop(_i3) {
        var noFurtherConflicts = false;
        var groupStart = _i3;
        var groupEnd = _i3;

        while (groupEnd + 1 < lineList.length && !noFurtherConflicts) {
          // if this note conflicts with the next:
          if (_this2.checkCollision(lineList[groupEnd], lineList[groupEnd + 1])) {
            // include the next note in the group:
            groupEnd++;
          } else {
            noFurtherConflicts = true;
          }
        }

        // Gets an a line from the `lineList`, relative to the current group
        var getGroupLine = function getGroupLine(index) {
          return lineList[groupStart + index];
        };
        var getGroupLines = function getGroupLines(indexes) {
          return indexes.map(getGroupLine);
        };
        var lineDifference = function lineDifference(indexA, indexB) {
          var _getGroupLines$map = getGroupLines([indexA, indexB]).map(function (item) {
            return item.line;
          }),
              _getGroupLines$map2 = _slicedToArray(_getGroupLines$map, 2),
              a = _getGroupLines$map2[0],
              b = _getGroupLines$map2[1];

          return a - b;
        };

        var notColliding = function notColliding() {
          for (var _len2 = arguments.length, indexPairs = Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
            indexPairs[_key2] = arguments[_key2];
          }

          return indexPairs.map(getGroupLines).every(function (lines) {
            return !_this2.checkCollision.apply(_this2, _toConsumableArray(lines));
          });
        };

        // Set columns for the lines in this group:
        var groupLength = groupEnd - groupStart + 1;

        // Set the accidental column for each line of the group
        var endCase = _this2.checkCollision(lineList[groupStart], lineList[groupEnd]) ? 'a' : 'b';

        switch (groupLength) {
          case 3:
            if (endCase === 'a' && lineDifference(1, 2) === 0.5 && lineDifference(0, 1) !== 0.5) {
              endCase = 'second_on_bottom';
            }
            break;
          case 4:
            if (notColliding([0, 2], [1, 3])) {
              endCase = 'spaced_out_tetrachord';
            }
            break;
          case 5:
            if (endCase === 'b' && notColliding([1, 3])) {
              endCase = 'spaced_out_pentachord';
              if (notColliding([0, 2], [2, 4])) {
                endCase = 'very_spaced_out_pentachord';
              }
            }
            break;
          case 6:
            if (notColliding([0, 3], [1, 4], [2, 5])) {
              endCase = 'spaced_out_hexachord';
            }
            if (notColliding([0, 2], [2, 4], [1, 3], [3, 5])) {
              endCase = 'very_spaced_out_hexachord';
            }
            break;
          default:
            break;
        }

        var groupMember = void 0;
        var column = void 0;
        // If the group contains more than seven members, use ascending parallel lines
        // of accidentals, using as few columns as possible while avoiding collisions.
        if (groupLength >= 7) {
          // First, determine how many columns to use:
          var patternLength = 2;
          var collisionDetected = true;
          while (collisionDetected === true) {
            collisionDetected = false;
            for (var line = 0; line + patternLength < lineList.length; line++) {
              if (_this2.checkCollision(lineList[line], lineList[line + patternLength])) {
                collisionDetected = true;
                patternLength++;
                break;
              }
            }
          }
          // Then, assign a column to each line of accidentals
          for (groupMember = _i3; groupMember <= groupEnd; groupMember++) {
            column = (groupMember - _i3) % patternLength + 1;
            lineList[groupMember].column = column;
            totalColumns = totalColumns > column ? totalColumns : column;
          }

          // Otherwise, if the group contains fewer than seven members, use the layouts from
          // the accidentalsColumnsTable housed in tables.js.
        } else {
          for (groupMember = _i3; groupMember <= groupEnd; groupMember++) {
            column = _tables.Flow.accidentalColumnsTable[groupLength][endCase][groupMember - _i3];
            lineList[groupMember].column = column;
            totalColumns = totalColumns > column ? totalColumns : column;
          }
        }

        // Increment i to the last note that was set, so that if a lower set of notes
        // does not conflict at all with this group, it can have its own classic shape.
        _i3 = groupEnd;
        _i2 = _i3;
      };

      for (var _i2 = 0; _i2 < lineList.length; _i2++) {
        _loop(_i2);
      }

      // ### Convert Columns to x_offsets
      //
      // This keeps columns aligned, even if they have different accidentals within them
      // which sometimes results in a larger x_offset than is an accidental might need
      // to preserve the symmetry of the accidental shape.
      //
      // Neither A.C. Vinci nor G. Read address this, and it typically only happens in
      // music with complex chord clusters.
      //
      // TODO (?): Optionally allow closer compression of accidentals, instead of forcing
      // parallel columns.

      // track each column's max width, which will be used as initial shift of later columns:
      var columnWidths = [];
      var columnXOffsets = [];
      for (var _i4 = 0; _i4 <= totalColumns; _i4++) {
        columnWidths[_i4] = 0;
        columnXOffsets[_i4] = 0;
      }

      columnWidths[0] = accShift + leftShift;
      columnXOffsets[0] = accShift + leftShift;

      // Fill columnWidths with widest needed x-space;
      // this is what keeps the columns parallel.
      lineList.forEach(function (line) {
        if (line.width > columnWidths[line.column]) columnWidths[line.column] = line.width;
      });

      for (var _i5 = 1; _i5 < columnWidths.length; _i5++) {
        // this column's offset = this column's width + previous column's offset
        columnXOffsets[_i5] = columnWidths[_i5] + columnXOffsets[_i5 - 1];
      }

      var totalShift = columnXOffsets[columnXOffsets.length - 1];
      // Set the xShift for each accidental according to column offsets:
      var accCount = 0;
      lineList.forEach(function (line) {
        var lineWidth = 0;
        var lastAccOnLine = accCount + line.numAcc;
        // handle all of the accidentals on a given line:
        for (accCount; accCount < lastAccOnLine; accCount++) {
          var xShift = columnXOffsets[line.column - 1] + lineWidth;
          accList[accCount].acc.setXShift(xShift);
          // keep track of the width of accidentals we've added so far, so that when
          // we loop, we add space for them.
          lineWidth += accList[accCount].acc.getWidth() + accidentalSpacing;
          L('Line, accCount, shift: ', line.line, accCount, xShift);
        }
      });

      // update the overall layout with the full width of the accidental shapes:
      state.left_shift += totalShift;
    }

    // Helper function to determine whether two lines of accidentals collide vertically

  }, {
    key: 'checkCollision',
    value: function checkCollision(line1, line2) {
      var clearance = line2.line - line1.line;
      var clearanceRequired = 3;
      // But less clearance is required for certain accidentals: b, bb and ##.
      if (clearance > 0) {
        // then line 2 is on top
        clearanceRequired = line2.flatLine || line2.dblSharpLine ? 2.5 : 3.0;
        if (line1.dblSharpLine) clearance -= 0.5;
      } else {
        // line 1 is on top
        clearanceRequired = line1.flatLine || line1.dblSharpLine ? 2.5 : 3.0;
        if (line2.dblSharpLine) clearance -= 0.5;
      }
      var collision = Math.abs(clearance) < clearanceRequired;
      L('Line_1, Line_2, Collision: ', line1.line, line2.line, collision);
      return collision;
    }

    // Use this method to automatically apply accidentals to a set of `voices`.
    // The accidentals will be remembered between all the voices provided.
    // Optionally, you can also provide an initial `keySignature`.

  }, {
    key: 'applyAccidentals',
    value: function applyAccidentals(voices, keySignature) {
      var tickPositions = [];
      var tickNoteMap = {};

      // Sort the tickables in each voice by their tick position in the voice
      voices.forEach(function (voice) {
        var tickPosition = new _fraction.Fraction(0, 1);
        var notes = voice.getTickables();
        notes.forEach(function (note) {
          if (note.shouldIgnoreTicks()) return;

          var notesAtPosition = tickNoteMap[tickPosition.value()];

          if (!notesAtPosition) {
            tickPositions.push(tickPosition.value());
            tickNoteMap[tickPosition.value()] = [note];
          } else {
            notesAtPosition.push(note);
          }

          tickPosition.add(note.getTicks());
        });
      });

      var music = new _music.Music();

      // Default key signature is C major
      if (!keySignature) keySignature = 'C';

      // Get the scale map, which represents the current state of each pitch
      var scaleMap = music.createScaleMap(keySignature);

      tickPositions.forEach(function (tick) {
        var notes = tickNoteMap[tick];

        // Array to store all pitches that modified accidental states
        // at this tick position
        var modifiedPitches = [];

        var processNote = function processNote(note) {
          if (note.isRest() || note.shouldIgnoreTicks()) return;

          // Go through each key and determine if an accidental should be
          // applied
          note.keys.forEach(function (keyString, keyIndex) {
            var key = music.getNoteParts(keyString.split('/')[0]);

            // Force a natural for every key without an accidental
            var accidentalString = key.accidental || 'n';
            var pitch = key.root + accidentalString;

            // Determine if the current pitch has the same accidental
            // as the scale state
            var sameAccidental = scaleMap[key.root] === pitch;

            // Determine if an identical pitch in the chord already
            // modified the accidental state
            var previouslyModified = modifiedPitches.indexOf(pitch) > -1;

            // Add the accidental to the StaveNote
            if (!sameAccidental || sameAccidental && previouslyModified) {
              // Modify the scale map so that the root pitch has an
              // updated state
              scaleMap[key.root] = pitch;

              // Create the accidental
              var accidental = new Accidental(accidentalString);

              // Attach the accidental to the StaveNote
              note.addAccidental(keyIndex, accidental);

              // Add the pitch to list of pitches that modified accidentals
              modifiedPitches.push(pitch);
            }
          });

          // process grace notes
          note.getModifiers().forEach(function (modifier) {
            if (modifier.getCategory() === 'gracenotegroups') {
              modifier.getGraceNotes().forEach(processNote);
            }
          });
        };

        notes.forEach(processNote);
      });
    }

    // Create accidental. `type` can be a value from the
    // `Vex.Flow.accidentalCodes.accidentals` table in `tables.js`. For
    // example: `#`, `##`, `b`, `n`, etc.

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'accidentals';
    }
  }]);

  function Accidental() {
    var type = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;

    _classCallCheck(this, Accidental);

    var _this = _possibleConstructorReturn(this, (Accidental.__proto__ || Object.getPrototypeOf(Accidental)).call(this));

    _this.setAttribute('type', 'Accidental');

    L('New accidental: ', type);

    _this.note = null;
    // The `index` points to a specific note in a chord.
    _this.index = null;
    _this.type = type;
    _this.position = _modifier.Modifier.Position.LEFT;

    _this.render_options = {
      // Font size for glyphs
      font_scale: 38,

      // Length of stroke across heads above or below the stave.
      stroke_px: 3,

      // Padding between accidental and parentheses on each side
      parenLeftPadding: 2,
      parenRightPadding: 2
    };

    _this.accidental = _tables.Flow.accidentalCodes(_this.type);
    if (!_this.accidental) {
      throw new _vex.Vex.RERR('ArgumentError', 'Unknown accidental type: ' + type);
    }

    // Cautionary accidentals have parentheses around them
    _this.cautionary = false;
    _this.parenLeft = null;
    _this.parenRight = null;

    _this.reset();
    return _this;
  }

  _createClass(Accidental, [{
    key: 'reset',
    value: function reset() {
      var fontScale = this.render_options.font_scale;
      this.glyph = new _glyph.Glyph(this.accidental.code, fontScale);
      this.glyph.setOriginX(1.0);

      if (this.cautionary) {
        this.parenLeft = new _glyph.Glyph(_tables.Flow.accidentalCodes('{').code, fontScale);
        this.parenRight = new _glyph.Glyph(_tables.Flow.accidentalCodes('}').code, fontScale);
        this.parenLeft.setOriginX(1.0);
        this.parenRight.setOriginX(1.0);
      }
    }
  }, {
    key: 'getCategory',
    value: function getCategory() {
      return Accidental.CATEGORY;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      var parenWidth = this.cautionary ? getGlyphWidth(this.parenLeft) + getGlyphWidth(this.parenRight) + this.render_options.parenLeftPadding + this.render_options.parenRightPadding : 0;

      return getGlyphWidth(this.glyph) + parenWidth;
    }

    // Attach this accidental to `note`, which must be a `StaveNote`.

  }, {
    key: 'setNote',
    value: function setNote(note) {
      if (!note) {
        throw new _vex.Vex.RERR('ArgumentError', 'Bad note value: ' + note);
      }

      this.note = note;

      // Accidentals attached to grace notes are rendered smaller.
      if (this.note.getCategory() === 'gracenotes') {
        this.render_options.font_scale = 25;
        this.reset();
      }
    }

    // If called, draws parenthesis around accidental.

  }, {
    key: 'setAsCautionary',
    value: function setAsCautionary() {
      this.cautionary = true;
      this.render_options.font_scale = 28;
      this.reset();
      return this;
    }

    // Render accidental onto canvas.

  }, {
    key: 'draw',
    value: function draw() {
      var context = this.context,
          type = this.type,
          position = this.position,
          note = this.note,
          index = this.index,
          cautionary = this.cautionary,
          x_shift = this.x_shift,
          y_shift = this.y_shift,
          glyph = this.glyph,
          parenLeft = this.parenLeft,
          parenRight = this.parenRight,
          _render_options = this.render_options,
          parenLeftPadding = _render_options.parenLeftPadding,
          parenRightPadding = _render_options.parenRightPadding;


      this.checkContext();

      if (!(note && index != null)) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw accidental without a note and index.");
      }

      // Figure out the start `x` and `y` coordinates for note and index.
      var start = note.getModifierStartXY(position, index);
      var accX = start.x + x_shift;
      var accY = start.y + y_shift;
      L('Rendering: ', type, accX, accY);

      if (!cautionary) {
        glyph.render(context, accX, accY);
      } else {
        // Render the accidental in parentheses.
        parenRight.render(context, accX, accY);
        accX -= getGlyphWidth(parenRight);
        accX -= parenRightPadding;
        accX -= this.accidental.parenRightPaddingAdjustment;
        glyph.render(context, accX, accY);
        accX -= getGlyphWidth(glyph);
        accX -= parenLeftPadding;
        parenLeft.render(context, accX, accY);
      }

      this.setRendered();
    }
  }]);

  return Accidental;
}(_modifier.Modifier);

/***/ }),
/* 25 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Music = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class implements some standard music theory routines.

var _vex = __webpack_require__(0);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Music = exports.Music = function () {
  function Music() {
    _classCallCheck(this, Music);
  }

  _createClass(Music, [{
    key: 'isValidNoteValue',
    value: function isValidNoteValue(note) {
      if (note == null || note < 0 || note >= Music.NUM_TONES) {
        return false;
      }
      return true;
    }
  }, {
    key: 'isValidIntervalValue',
    value: function isValidIntervalValue(interval) {
      return this.isValidNoteValue(interval);
    }
  }, {
    key: 'getNoteParts',
    value: function getNoteParts(noteString) {
      if (!noteString || noteString.length < 1) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid note name: ' + noteString);
      }

      if (noteString.length > 3) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid note name: ' + noteString);
      }

      var note = noteString.toLowerCase();

      var regex = /^([cdefgab])(b|bb|n|#|##)?$/;
      var match = regex.exec(note);

      if (match != null) {
        var root = match[1];
        var accidental = match[2];

        return {
          root: root,
          accidental: accidental
        };
      } else {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid note name: ' + noteString);
      }
    }
  }, {
    key: 'getKeyParts',
    value: function getKeyParts(keyString) {
      if (!keyString || keyString.length < 1) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid key: ' + keyString);
      }

      var key = keyString.toLowerCase();

      // Support Major, Minor, Melodic Minor, and Harmonic Minor key types.
      var regex = /^([cdefgab])(b|#)?(mel|harm|m|M)?$/;
      var match = regex.exec(key);

      if (match != null) {
        var root = match[1];
        var accidental = match[2];
        var type = match[3];

        // Unspecified type implies major
        if (!type) type = 'M';

        return {
          root: root,
          accidental: accidental,
          type: type
        };
      } else {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid key: ' + keyString);
      }
    }
  }, {
    key: 'getNoteValue',
    value: function getNoteValue(noteString) {
      var value = Music.noteValues[noteString];
      if (value == null) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid note name: ' + noteString);
      }

      return value.int_val;
    }
  }, {
    key: 'getIntervalValue',
    value: function getIntervalValue(intervalString) {
      var value = Music.intervals[intervalString];
      if (value == null) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid interval name: ' + intervalString);
      }

      return value;
    }
  }, {
    key: 'getCanonicalNoteName',
    value: function getCanonicalNoteName(noteValue) {
      if (!this.isValidNoteValue(noteValue)) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid note value: ' + noteValue);
      }

      return Music.canonical_notes[noteValue];
    }
  }, {
    key: 'getCanonicalIntervalName',
    value: function getCanonicalIntervalName(intervalValue) {
      if (!this.isValidIntervalValue(intervalValue)) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid interval value: ' + intervalValue);
      }

      return Music.diatonic_intervals[intervalValue];
    }

    /* Given a note, interval, and interval direction, product the
     * relative note.
     */

  }, {
    key: 'getRelativeNoteValue',
    value: function getRelativeNoteValue(noteValue, intervalValue, direction) {
      if (direction == null) direction = 1;

      if (direction !== 1 && direction !== -1) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid direction: ' + direction);
      }

      var sum = (noteValue + direction * intervalValue) % Music.NUM_TONES;
      if (sum < 0) sum += Music.NUM_TONES;

      return sum;
    }
  }, {
    key: 'getRelativeNoteName',
    value: function getRelativeNoteName(root, noteValue) {
      var parts = this.getNoteParts(root);
      var rootValue = this.getNoteValue(parts.root);
      var interval = noteValue - rootValue;

      if (Math.abs(interval) > Music.NUM_TONES - 3) {
        var multiplier = 1;
        if (interval > 0) multiplier = -1;

        // Possibly wrap around. (Add +1 for modulo operator)
        var reverse_interval = (noteValue + 1 + (rootValue + 1)) % Music.NUM_TONES * multiplier;

        if (Math.abs(reverse_interval) > 2) {
          throw new _vex.Vex.RERR('BadArguments', 'Notes not related: ' + root + ', ' + noteValue + ')');
        } else {
          interval = reverse_interval;
        }
      }

      if (Math.abs(interval) > 2) {
        throw new _vex.Vex.RERR('BadArguments', 'Notes not related: ' + root + ', ' + noteValue + ')');
      }

      var relativeNoteName = parts.root;
      if (interval > 0) {
        for (var i = 1; i <= interval; ++i) {
          relativeNoteName += '#';
        }
      } else if (interval < 0) {
        for (var _i = -1; _i >= interval; --_i) {
          relativeNoteName += 'b';
        }
      }

      return relativeNoteName;
    }

    /* Return scale tones, given intervals. Each successive interval is
     * relative to the previous one, e.g., Major Scale:
     *
     *   TTSTTTS = [2,2,1,2,2,2,1]
     *
     * When used with key = 0, returns C scale (which is isomorphic to
     * interval list).
     */

  }, {
    key: 'getScaleTones',
    value: function getScaleTones(key, intervals) {
      var tones = [key];

      var nextNote = key;
      for (var i = 0; i < intervals.length; i += 1) {
        nextNote = this.getRelativeNoteValue(nextNote, intervals[i]);
        if (nextNote !== key) tones.push(nextNote);
      }

      return tones;
    }

    /* Returns the interval of a note, given a diatonic scale.
     *
     * E.g., Given the scale C, and the note E, returns M3
     */

  }, {
    key: 'getIntervalBetween',
    value: function getIntervalBetween(note1, note2, direction) {
      if (direction == null) direction = 1;

      if (direction !== 1 && direction !== -1) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid direction: ' + direction);
      }

      if (!this.isValidNoteValue(note1) || !this.isValidNoteValue(note2)) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid notes: ' + note1 + ', ' + note2);
      }

      var difference = direction === 1 ? note2 - note1 : note1 - note2;

      if (difference < 0) difference += Music.NUM_TONES;

      return difference;
    }

    // Create a scale map that represents the pitch state for a
    // `keySignature`. For example, passing a `G` to `keySignature` would
    // return a scale map with every note naturalized except for `F` which
    // has an `F#` state.

  }, {
    key: 'createScaleMap',
    value: function createScaleMap(keySignature) {
      var keySigParts = this.getKeyParts(keySignature);
      var scaleName = Music.scaleTypes[keySigParts.type];

      var keySigString = keySigParts.root;
      if (keySigParts.accidental) keySigString += keySigParts.accidental;

      if (!scaleName) throw new _vex.Vex.RERR('BadArguments', 'Unsupported key type: ' + keySignature);

      var scale = this.getScaleTones(this.getNoteValue(keySigString), scaleName);
      var noteLocation = Music.root_indices[keySigParts.root];

      var scaleMap = {};
      for (var i = 0; i < Music.roots.length; ++i) {
        var index = (noteLocation + i) % Music.roots.length;
        var rootName = Music.roots[index];
        var noteName = this.getRelativeNoteName(rootName, scale[i]);

        if (noteName.length === 1) {
          noteName += 'n';
        }

        scaleMap[rootName] = noteName;
      }

      return scaleMap;
    }
  }], [{
    key: 'NUM_TONES',
    get: function get() {
      return 12;
    }
  }, {
    key: 'roots',
    get: function get() {
      return ['c', 'd', 'e', 'f', 'g', 'a', 'b'];
    }
  }, {
    key: 'root_values',
    get: function get() {
      return [0, 2, 4, 5, 7, 9, 11];
    }
  }, {
    key: 'root_indices',
    get: function get() {
      return {
        'c': 0,
        'd': 1,
        'e': 2,
        'f': 3,
        'g': 4,
        'a': 5,
        'b': 6
      };
    }
  }, {
    key: 'canonical_notes',
    get: function get() {
      return ['c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#', 'a', 'a#', 'b'];
    }
  }, {
    key: 'diatonic_intervals',
    get: function get() {
      return ['unison', 'm2', 'M2', 'm3', 'M3', 'p4', 'dim5', 'p5', 'm6', 'M6', 'b7', 'M7', 'octave'];
    }
  }, {
    key: 'diatonic_accidentals',
    get: function get() {
      return {
        'unison': { note: 0, accidental: 0 },
        'm2': { note: 1, accidental: -1 },
        'M2': { note: 1, accidental: 0 },
        'm3': { note: 2, accidental: -1 },
        'M3': { note: 2, accidental: 0 },
        'p4': { note: 3, accidental: 0 },
        'dim5': { note: 4, accidental: -1 },
        'p5': { note: 4, accidental: 0 },
        'm6': { note: 5, accidental: -1 },
        'M6': { note: 5, accidental: 0 },
        'b7': { note: 6, accidental: -1 },
        'M7': { note: 6, accidental: 0 },
        'octave': { note: 7, accidental: 0 }
      };
    }
  }, {
    key: 'intervals',
    get: function get() {
      return {
        'u': 0, 'unison': 0,
        'm2': 1, 'b2': 1, 'min2': 1, 'S': 1, 'H': 1,
        '2': 2, 'M2': 2, 'maj2': 2, 'T': 2, 'W': 2,
        'm3': 3, 'b3': 3, 'min3': 3,
        'M3': 4, '3': 4, 'maj3': 4,
        '4': 5, 'p4': 5,
        '#4': 6, 'b5': 6, 'aug4': 6, 'dim5': 6,
        '5': 7, 'p5': 7,
        '#5': 8, 'b6': 8, 'aug5': 8,
        '6': 9, 'M6': 9, 'maj6': 9,
        'b7': 10, 'm7': 10, 'min7': 10, 'dom7': 10,
        'M7': 11, 'maj7': 11,
        '8': 12, 'octave': 12
      };
    }
  }, {
    key: 'scales',
    get: function get() {
      return {
        major: [2, 2, 1, 2, 2, 2, 1],
        dorian: [2, 1, 2, 2, 2, 1, 2],
        mixolydian: [2, 2, 1, 2, 2, 1, 2],
        minor: [2, 1, 2, 2, 1, 2, 2]
      };
    }
  }, {
    key: 'scaleTypes',
    get: function get() {
      return {
        'M': Music.scales.major,
        'm': Music.scales.minor
      };
    }
  }, {
    key: 'accidentals',
    get: function get() {
      return ['bb', 'b', 'n', '#', '##'];
    }
  }, {
    key: 'noteValues',
    get: function get() {
      return {
        'c': { root_index: 0, int_val: 0 },
        'cn': { root_index: 0, int_val: 0 },
        'c#': { root_index: 0, int_val: 1 },
        'c##': { root_index: 0, int_val: 2 },
        'cb': { root_index: 0, int_val: 11 },
        'cbb': { root_index: 0, int_val: 10 },
        'd': { root_index: 1, int_val: 2 },
        'dn': { root_index: 1, int_val: 2 },
        'd#': { root_index: 1, int_val: 3 },
        'd##': { root_index: 1, int_val: 4 },
        'db': { root_index: 1, int_val: 1 },
        'dbb': { root_index: 1, int_val: 0 },
        'e': { root_index: 2, int_val: 4 },
        'en': { root_index: 2, int_val: 4 },
        'e#': { root_index: 2, int_val: 5 },
        'e##': { root_index: 2, int_val: 6 },
        'eb': { root_index: 2, int_val: 3 },
        'ebb': { root_index: 2, int_val: 2 },
        'f': { root_index: 3, int_val: 5 },
        'fn': { root_index: 3, int_val: 5 },
        'f#': { root_index: 3, int_val: 6 },
        'f##': { root_index: 3, int_val: 7 },
        'fb': { root_index: 3, int_val: 4 },
        'fbb': { root_index: 3, int_val: 3 },
        'g': { root_index: 4, int_val: 7 },
        'gn': { root_index: 4, int_val: 7 },
        'g#': { root_index: 4, int_val: 8 },
        'g##': { root_index: 4, int_val: 9 },
        'gb': { root_index: 4, int_val: 6 },
        'gbb': { root_index: 4, int_val: 5 },
        'a': { root_index: 5, int_val: 9 },
        'an': { root_index: 5, int_val: 9 },
        'a#': { root_index: 5, int_val: 10 },
        'a##': { root_index: 5, int_val: 11 },
        'ab': { root_index: 5, int_val: 8 },
        'abb': { root_index: 5, int_val: 7 },
        'b': { root_index: 6, int_val: 11 },
        'bn': { root_index: 6, int_val: 11 },
        'b#': { root_index: 6, int_val: 0 },
        'b##': { root_index: 6, int_val: 1 },
        'bb': { root_index: 6, int_val: 10 },
        'bbb': { root_index: 6, int_val: 9 }
      };
    }
  }]);

  return Music;
}();

/***/ }),
/* 26 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoteSubGroup = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

var _formatter = __webpack_require__(11);

var _voice = __webpack_require__(12);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Taehoon Moon 2016
//
// ## Description
//
// This file implements `NoteSubGroup` which is used to format and
// render notes as a `Modifier`
// ex) ClefNote, TimeSigNote and BarNote.

var NoteSubGroup = exports.NoteSubGroup = function (_Modifier) {
  _inherits(NoteSubGroup, _Modifier);

  _createClass(NoteSubGroup, null, [{
    key: 'format',


    // Arrange groups inside a `ModifierContext`
    value: function format(groups, state) {
      if (!groups || groups.length === 0) return false;

      var width = 0;
      for (var i = 0; i < groups.length; ++i) {
        var group = groups[i];
        group.preFormat();
        width += group.getWidth();
      }

      state.left_shift += width;
      return true;
    }
  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'notesubgroup';
    }
  }]);

  function NoteSubGroup(subNotes) {
    var _ret;

    _classCallCheck(this, NoteSubGroup);

    var _this = _possibleConstructorReturn(this, (NoteSubGroup.__proto__ || Object.getPrototypeOf(NoteSubGroup)).call(this));

    _this.setAttribute('type', 'NoteSubGroup');

    _this.note = null;
    _this.index = null;
    _this.position = _modifier.Modifier.Position.LEFT;
    _this.subNotes = subNotes;
    _this.subNotes.forEach(function (subNote) {
      subNote.ignore_ticks = false;
    });
    _this.width = 0;
    _this.preFormatted = false;

    _this.formatter = new _formatter.Formatter();
    _this.voice = new _voice.Voice({
      num_beats: 4,
      beat_value: 4,
      resolution: _tables.Flow.RESOLUTION
    }).setStrict(false);

    _this.voice.addTickables(_this.subNotes);

    return _ret = _this, _possibleConstructorReturn(_this, _ret);
  }

  _createClass(NoteSubGroup, [{
    key: 'getCategory',
    value: function getCategory() {
      return NoteSubGroup.CATEGORY;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return;

      this.formatter.joinVoices([this.voice]).format([this.voice], 0);
      this.setWidth(this.formatter.getMinTotalWidth());
      this.preFormatted = true;
    }
  }, {
    key: 'setNote',
    value: function setNote(note) {
      this.note = note;
    }
  }, {
    key: 'setWidth',
    value: function setWidth(width) {
      this.width = width;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var _this2 = this;

      this.checkContext();

      var note = this.getNote();

      if (!(note && this.index !== null)) {
        throw new _vex.Vex.RuntimeError('NoAttachedNote', "Can't draw notes without a parent note and parent note index.");
      }

      this.setRendered();
      this.alignSubNotesWithNote(this.subNotes, note); // Modifier function

      // Draw notes
      this.subNotes.forEach(function (subNote) {
        return subNote.setContext(_this2.context).draw();
      });
    }
  }]);

  return NoteSubGroup;
}(_modifier.Modifier);

/***/ }),
/* 27 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.GraceNoteGroup = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

var _formatter = __webpack_require__(11);

var _voice = __webpack_require__(12);

var _beam = __webpack_require__(15);

var _stavetie = __webpack_require__(16);

var _tabtie = __webpack_require__(28);

var _stavenote = __webpack_require__(5);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements `GraceNoteGroup` which is used to format and
// render grace notes.

// To enable logging for this class. Set `Vex.Flow.GraceNoteGroup.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (GraceNoteGroup.DEBUG) _vex.Vex.L('Vex.Flow.GraceNoteGroup', args);
}

var GraceNoteGroup = exports.GraceNoteGroup = function (_Modifier) {
  _inherits(GraceNoteGroup, _Modifier);

  _createClass(GraceNoteGroup, null, [{
    key: 'format',


    // Arrange groups inside a `ModifierContext`
    value: function format(gracenote_groups, state) {
      var group_spacing_stave = 4;
      var group_spacing_tab = 0;

      if (!gracenote_groups || gracenote_groups.length === 0) return false;

      var group_list = [];
      var prev_note = null;
      var shiftL = 0;

      for (var i = 0; i < gracenote_groups.length; ++i) {
        var gracenote_group = gracenote_groups[i];
        var note = gracenote_group.getNote();
        var is_stavenote = note.getCategory() === _stavenote.StaveNote.CATEGORY;
        var spacing = is_stavenote ? group_spacing_stave : group_spacing_tab;

        if (is_stavenote && note !== prev_note) {
          // Iterate through all notes to get the displaced pixels
          for (var n = 0; n < note.keys.length; ++n) {
            var props_tmp = note.getKeyProps()[n];
            shiftL = props_tmp.displaced ? note.getExtraLeftPx() : shiftL;
          }
          prev_note = note;
        }

        group_list.push({ shift: shiftL, gracenote_group: gracenote_group, spacing: spacing });
      }

      // If first note left shift in case it is displaced
      var group_shift = group_list[0].shift;
      var formatWidth = void 0;
      for (var _i = 0; _i < group_list.length; ++_i) {
        var _gracenote_group = group_list[_i].gracenote_group;
        _gracenote_group.preFormat();
        formatWidth = _gracenote_group.getWidth() + group_list[_i].spacing;
        group_shift = Math.max(formatWidth, group_shift);
      }

      for (var _i2 = 0; _i2 < group_list.length; ++_i2) {
        var _gracenote_group2 = group_list[_i2].gracenote_group;
        formatWidth = _gracenote_group2.getWidth() + group_list[_i2].spacing;
        _gracenote_group2.setSpacingFromNextModifier(group_shift - Math.min(formatWidth, group_shift));
      }

      state.left_shift += group_shift;
      return true;
    }

    // ## Prototype Methods
    //
    // `GraceNoteGroup` inherits from `Modifier` and is placed inside a
    // `ModifierContext`.

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'gracenotegroups';
    }
  }]);

  function GraceNoteGroup(grace_notes, show_slur) {
    var _ret;

    _classCallCheck(this, GraceNoteGroup);

    var _this = _possibleConstructorReturn(this, (GraceNoteGroup.__proto__ || Object.getPrototypeOf(GraceNoteGroup)).call(this));

    _this.setAttribute('type', 'GraceNoteGroup');

    _this.note = null;
    _this.index = null;
    _this.position = _modifier.Modifier.Position.LEFT;
    _this.grace_notes = grace_notes;
    _this.width = 0;

    _this.preFormatted = false;

    _this.show_slur = show_slur;
    _this.slur = null;

    _this.formatter = new _formatter.Formatter();
    _this.voice = new _voice.Voice({
      num_beats: 4,
      beat_value: 4,
      resolution: _tables.Flow.RESOLUTION
    }).setStrict(false);

    _this.render_options = {
      slur_y_shift: 0
    };

    _this.voice.addTickables(_this.grace_notes);

    return _ret = _this, _possibleConstructorReturn(_this, _ret);
  }

  _createClass(GraceNoteGroup, [{
    key: 'getCategory',
    value: function getCategory() {
      return GraceNoteGroup.CATEGORY;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return;

      this.formatter.joinVoices([this.voice]).format([this.voice], 0);
      this.setWidth(this.formatter.getMinTotalWidth());
      this.preFormatted = true;
    }
  }, {
    key: 'beamNotes',
    value: function beamNotes() {
      if (this.grace_notes.length > 1) {
        var beam = new _beam.Beam(this.grace_notes);

        beam.render_options.beam_width = 3;
        beam.render_options.partial_beam_length = 4;

        this.beam = beam;
      }

      return this;
    }
  }, {
    key: 'setNote',
    value: function setNote(note) {
      this.note = note;
    }
  }, {
    key: 'setWidth',
    value: function setWidth(width) {
      this.width = width;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'getGraceNotes',
    value: function getGraceNotes() {
      return this.grace_notes;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var _this2 = this;

      this.checkContext();

      var note = this.getNote();

      L('Drawing grace note group for:', note);

      if (!(note && this.index !== null)) {
        throw new _vex.Vex.RuntimeError('NoAttachedNote', "Can't draw grace note without a parent note and parent note index.");
      }

      this.setRendered();
      this.alignSubNotesWithNote(this.getGraceNotes(), note); // Modifier function

      // Draw notes
      this.grace_notes.forEach(function (graceNote) {
        graceNote.setContext(_this2.context).draw();
      });

      // Draw beam
      if (this.beam) {
        this.beam.setContext(this.context).draw();
      }

      if (this.show_slur) {
        // Create and draw slur
        var is_stavenote = this.getNote().getCategory() === _stavenote.StaveNote.CATEGORY;
        var TieClass = is_stavenote ? _stavetie.StaveTie : _tabtie.TabTie;

        this.slur = new TieClass({
          last_note: this.grace_notes[0],
          first_note: note,
          first_indices: [0],
          last_indices: [0]
        });

        this.slur.render_options.cp2 = 12;
        this.slur.render_options.y_shift = (is_stavenote ? 7 : 5) + this.render_options.slur_y_shift;
        this.slur.setContext(this.context).draw();
      }
    }
  }]);

  return GraceNoteGroup;
}(_modifier.Modifier);

/***/ }),
/* 28 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TabTie = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _stavetie = __webpack_require__(16);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // / [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class implements varies types of ties between contiguous notes. The
// ties include: regular ties, hammer ons, pull offs, and slides.

var TabTie = exports.TabTie = function (_StaveTie) {
  _inherits(TabTie, _StaveTie);

  _createClass(TabTie, null, [{
    key: 'createHammeron',
    value: function createHammeron(notes) {
      return new TabTie(notes, 'H');
    }
  }, {
    key: 'createPulloff',
    value: function createPulloff(notes) {
      return new TabTie(notes, 'P');
    }
  }]);

  function TabTie(notes, text) {
    _classCallCheck(this, TabTie);

    var _this = _possibleConstructorReturn(this, (TabTie.__proto__ || Object.getPrototypeOf(TabTie)).call(this, notes, text));
    /**
     * Notes is a struct that has:
     *
     *  {
     *    first_note: Note,
     *    last_note: Note,
     *    first_indices: [n1, n2, n3],
     *    last_indices: [n1, n2, n3]
     *  }
     *
     **/


    _this.setAttribute('type', 'TabTie');

    _this.render_options.cp1 = 9;
    _this.render_options.cp2 = 11;
    _this.render_options.y_shift = 3;

    _this.setNotes(notes);
    return _this;
  }

  _createClass(TabTie, [{
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      var first_note = this.first_note;
      var last_note = this.last_note;
      var first_x_px = void 0;
      var last_x_px = void 0;
      var first_ys = void 0;
      var last_ys = void 0;

      if (first_note) {
        first_x_px = first_note.getTieRightX() + this.render_options.tie_spacing;
        first_ys = first_note.getYs();
      } else {
        first_x_px = last_note.getStave().getTieStartX();
        first_ys = last_note.getYs();
        this.first_indices = this.last_indices;
      }

      if (last_note) {
        last_x_px = last_note.getTieLeftX() + this.render_options.tie_spacing;
        last_ys = last_note.getYs();
      } else {
        last_x_px = first_note.getStave().getTieEndX();
        last_ys = first_note.getYs();
        this.last_indices = this.first_indices;
      }

      this.renderTie({
        first_x_px: first_x_px,
        last_x_px: last_x_px,
        first_ys: first_ys,
        last_ys: last_ys,
        direction: -1 // Tab tie's are always face up.
      });

      this.renderText(first_x_px, last_x_px);
      return true;
    }
  }]);

  return TabTie;
}(_stavetie.StaveTie);

/***/ }),
/* 29 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StringNumber = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _modifier = __webpack_require__(4);

var _renderer = __webpack_require__(14);

var _stavenote = __webpack_require__(5);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Larry Kuhns
//
// ## Description
// This file implements the `StringNumber` class which renders string
// number annotations beside notes.

var StringNumber = exports.StringNumber = function (_Modifier) {
  _inherits(StringNumber, _Modifier);

  _createClass(StringNumber, null, [{
    key: 'format',


    // ## Static Methods
    // Arrange string numbers inside a `ModifierContext`
    value: function format(nums, state) {
      var left_shift = state.left_shift;
      var right_shift = state.right_shift;
      var num_spacing = 1;

      if (!nums || nums.length === 0) return this;

      var nums_list = [];
      var prev_note = null;
      var shift_left = 0;
      var shift_right = 0;

      var i = void 0;
      var num = void 0;
      var note = void 0;
      var pos = void 0;
      var props_tmp = void 0;
      for (i = 0; i < nums.length; ++i) {
        num = nums[i];
        note = num.getNote();

        for (i = 0; i < nums.length; ++i) {
          num = nums[i];
          note = num.getNote();
          pos = num.getPosition();
          var props = note.getKeyProps()[num.getIndex()];

          if (note !== prev_note) {
            for (var n = 0; n < note.keys.length; ++n) {
              props_tmp = note.getKeyProps()[n];
              if (left_shift === 0) {
                shift_left = props_tmp.displaced ? note.getExtraLeftPx() : shift_left;
              }
              if (right_shift === 0) {
                shift_right = props_tmp.displaced ? note.getExtraRightPx() : shift_right;
              }
            }
            prev_note = note;
          }

          nums_list.push({
            pos: pos,
            note: note,
            num: num,
            line: props.line,
            shiftL: shift_left,
            shiftR: shift_right
          });
        }
      }

      // Sort string numbers by line number.
      nums_list.sort(function (a, b) {
        return b.line - a.line;
      });

      // TODO: This variable never gets assigned to anything. Is that a bug or can this be removed?
      var num_shiftL = 0; // eslint-disable-line
      var num_shiftR = 0;
      var x_widthL = 0;
      var x_widthR = 0;
      var last_line = null;
      var last_note = null;
      for (i = 0; i < nums_list.length; ++i) {
        var num_shift = 0;
        note = nums_list[i].note;
        pos = nums_list[i].pos;
        num = nums_list[i].num;
        var line = nums_list[i].line;
        var shiftL = nums_list[i].shiftL;
        var shiftR = nums_list[i].shiftR;

        // Reset the position of the string number every line.
        if (line !== last_line || note !== last_note) {
          num_shiftL = left_shift + shiftL;
          num_shiftR = right_shift + shiftR;
        }

        var num_width = num.getWidth() + num_spacing;
        if (pos === _modifier.Modifier.Position.LEFT) {
          num.setXShift(left_shift);
          num_shift = shift_left + num_width; // spacing
          x_widthL = num_shift > x_widthL ? num_shift : x_widthL;
        } else if (pos === _modifier.Modifier.Position.RIGHT) {
          num.setXShift(num_shiftR);
          num_shift += num_width; // spacing
          x_widthR = num_shift > x_widthR ? num_shift : x_widthR;
        }
        last_line = line;
        last_note = note;
      }

      state.left_shift += x_widthL;
      state.right_shift += x_widthR;
      return true;
    }
  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'stringnumber';
    }
  }]);

  function StringNumber(number) {
    _classCallCheck(this, StringNumber);

    var _this = _possibleConstructorReturn(this, (StringNumber.__proto__ || Object.getPrototypeOf(StringNumber)).call(this));

    _this.setAttribute('type', 'StringNumber');

    _this.note = null;
    _this.last_note = null;
    _this.index = null;
    _this.string_number = number;
    _this.setWidth(20); // ???
    _this.position = _modifier.Modifier.Position.ABOVE; // Default position above stem or note head
    _this.x_shift = 0;
    _this.y_shift = 0;
    _this.x_offset = 0; // Horizontal offset from default
    _this.y_offset = 0; // Vertical offset from default
    _this.dashed = true; // true - draw dashed extension  false - no extension
    _this.leg = _renderer.Renderer.LineEndType.NONE; // draw upward/downward leg at the of extension line
    _this.radius = 8;
    _this.font = {
      family: 'sans-serif',
      size: 10,
      weight: 'bold'
    };
    return _this;
  }

  _createClass(StringNumber, [{
    key: 'getCategory',
    value: function getCategory() {
      return StringNumber.CATEGORY;
    }
  }, {
    key: 'getNote',
    value: function getNote() {
      return this.note;
    }
  }, {
    key: 'setNote',
    value: function setNote(note) {
      this.note = note;return this;
    }
  }, {
    key: 'getIndex',
    value: function getIndex() {
      return this.index;
    }
  }, {
    key: 'setIndex',
    value: function setIndex(index) {
      this.index = index;return this;
    }
  }, {
    key: 'setLineEndType',
    value: function setLineEndType(leg) {
      if (leg >= _renderer.Renderer.LineEndType.NONE && leg <= _renderer.Renderer.LineEndType.DOWN) {
        this.leg = leg;
      }
      return this;
    }
  }, {
    key: 'setStringNumber',
    value: function setStringNumber(number) {
      this.string_number = number;return this;
    }
  }, {
    key: 'setOffsetX',
    value: function setOffsetX(x) {
      this.x_offset = x;return this;
    }
  }, {
    key: 'setOffsetY',
    value: function setOffsetY(y) {
      this.y_offset = y;return this;
    }
  }, {
    key: 'setLastNote',
    value: function setLastNote(note) {
      this.last_note = note;return this;
    }
  }, {
    key: 'setDashed',
    value: function setDashed(dashed) {
      this.dashed = dashed;return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var ctx = this.checkContext();
      if (!(this.note && this.index != null)) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw string number without a note and index.");
      }
      this.setRendered();

      var line_space = this.note.stave.options.spacing_between_lines_px;

      var start = this.note.getModifierStartXY(this.position, this.index);
      var dot_x = start.x + this.x_shift + this.x_offset;
      var dot_y = start.y + this.y_shift + this.y_offset;

      switch (this.position) {
        case _modifier.Modifier.Position.ABOVE:
        case _modifier.Modifier.Position.BELOW:
          {
            var stem_ext = this.note.getStemExtents();
            var top = stem_ext.topY;
            var bottom = stem_ext.baseY + 2;

            if (this.note.stem_direction === _stavenote.StaveNote.STEM_DOWN) {
              top = stem_ext.baseY;
              bottom = stem_ext.topY - 2;
            }

            if (this.position === _modifier.Modifier.Position.ABOVE) {
              dot_y = this.note.hasStem() ? top - line_space * 1.75 : start.y - line_space * 1.75;
            } else {
              dot_y = this.note.hasStem() ? bottom + line_space * 1.5 : start.y + line_space * 1.75;
            }

            dot_y += this.y_shift + this.y_offset;

            break;
          }case _modifier.Modifier.Position.LEFT:
          dot_x -= this.radius / 2 + 5;
          break;
        case _modifier.Modifier.Position.RIGHT:
          dot_x += this.radius / 2 + 6;
          break;
        default:
          throw new _vex.Vex.RERR('InvalidPosition', 'The position ' + this.position + ' is invalid');
      }

      ctx.save();
      ctx.beginPath();
      ctx.arc(dot_x, dot_y, this.radius, 0, Math.PI * 2, false);
      ctx.lineWidth = 1.5;
      ctx.stroke();
      ctx.setFont(this.font.family, this.font.size, this.font.weight);
      var x = dot_x - ctx.measureText(this.string_number).width / 2;
      ctx.fillText('' + this.string_number, x, dot_y + 4.5);

      if (this.last_note != null) {
        var end = this.last_note.getStemX() - this.note.getX() + 5;
        ctx.strokeStyle = '#000000';
        ctx.lineCap = 'round';
        ctx.lineWidth = 0.6;
        if (this.dashed) {
          _renderer.Renderer.drawDashedLine(ctx, dot_x + 10, dot_y, dot_x + end, dot_y, [3, 3]);
        } else {
          _renderer.Renderer.drawDashedLine(ctx, dot_x + 10, dot_y, dot_x + end, dot_y, [3, 0]);
        }

        var len = void 0;
        var pattern = void 0;
        switch (this.leg) {
          case _renderer.Renderer.LineEndType.UP:
            len = -10;
            pattern = this.dashed ? [3, 3] : [3, 0];
            _renderer.Renderer.drawDashedLine(ctx, dot_x + end, dot_y, dot_x + end, dot_y + len, pattern);
            break;
          case _renderer.Renderer.LineEndType.DOWN:
            len = 10;
            pattern = this.dashed ? [3, 3] : [3, 0];
            _renderer.Renderer.drawDashedLine(ctx, dot_x + end, dot_y, dot_x + end, dot_y + len, pattern);
            break;
          default:
            break;
        }
      }

      ctx.restore();
    }
  }]);

  return StringNumber;
}(_modifier.Modifier);

/***/ }),
/* 30 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Annotation = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements text annotations as modifiers that can be attached to
// notes.
//
// See `tests/annotation_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.Annotation.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Annotation.DEBUG) _vex.Vex.L('Vex.Flow.Annotation', args);
}

var Annotation = exports.Annotation = function (_Modifier) {
  _inherits(Annotation, _Modifier);

  _createClass(Annotation, null, [{
    key: 'format',


    // Arrange annotations within a `ModifierContext`
    value: function format(annotations, state) {
      if (!annotations || annotations.length === 0) return false;

      var width = 0;
      for (var i = 0; i < annotations.length; ++i) {
        var annotation = annotations[i];
        width = Math.max(annotation.getWidth(), width);
        if (annotation.getPosition() === _modifier.Modifier.Position.ABOVE) {
          annotation.setTextLine(state.top_text_line);
          state.top_text_line++;
        } else {
          annotation.setTextLine(state.text_line);
          state.text_line++;
        }
      }

      state.left_shift += width / 2;
      state.right_shift += width / 2;
      return true;
    }

    // ## Prototype Methods
    //
    // Annotations inherit from `Modifier` and is positioned correctly when
    // in a `ModifierContext`.
    // Create a new `Annotation` with the string `text`.

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'annotations';
    }

    // Text annotations can be positioned and justified relative to the note.

  }, {
    key: 'Justify',
    get: function get() {
      return {
        LEFT: 1,
        CENTER: 2,
        RIGHT: 3,
        CENTER_STEM: 4
      };
    }
  }, {
    key: 'JustifyString',
    get: function get() {
      return {
        left: Annotation.Justify.LEFT,
        right: Annotation.Justify.RIGHT,
        center: Annotation.Justify.CENTER,
        centerStem: Annotation.Justify.CENTER_STEM
      };
    }
  }, {
    key: 'VerticalJustify',
    get: function get() {
      return {
        TOP: 1,
        CENTER: 2,
        BOTTOM: 3,
        CENTER_STEM: 4
      };
    }
  }, {
    key: 'VerticalJustifyString',
    get: function get() {
      return {
        above: Annotation.VerticalJustify.TOP,
        top: Annotation.VerticalJustify.TOP,
        below: Annotation.VerticalJustify.BOTTOM,
        bottom: Annotation.VerticalJustify.BOTTOM,
        center: Annotation.VerticalJustify.CENTER,
        centerStem: Annotation.VerticalJustify.CENTER_STEM
      };
    }
  }]);

  function Annotation(text) {
    _classCallCheck(this, Annotation);

    var _this = _possibleConstructorReturn(this, (Annotation.__proto__ || Object.getPrototypeOf(Annotation)).call(this));

    _this.setAttribute('type', 'Annotation');

    _this.note = null;
    _this.index = null;
    _this.text = text;
    _this.justification = Annotation.Justify.CENTER;
    _this.vert_justification = Annotation.VerticalJustify.TOP;
    _this.font = {
      family: 'Arial',
      size: 10,
      weight: ''
    };

    // The default width is calculated from the text.
    _this.setWidth(_tables.Flow.textWidth(text));
    return _this;
  }

  _createClass(Annotation, [{
    key: 'getCategory',
    value: function getCategory() {
      return Annotation.CATEGORY;
    }

    // Set font family, size, and weight. E.g., `Arial`, `10pt`, `Bold`.

  }, {
    key: 'setFont',
    value: function setFont(family, size, weight) {
      this.font = { family: family, size: size, weight: weight };
      return this;
    }

    // Set vertical position of text (above or below stave). `just` must be
    // a value in `Annotation.VerticalJustify`.

  }, {
    key: 'setVerticalJustification',
    value: function setVerticalJustification(just) {
      this.vert_justification = typeof just === 'string' ? Annotation.VerticalJustifyString[just] : just;
      return this;
    }

    // Get and set horizontal justification. `justification` is a value in
    // `Annotation.Justify`.

  }, {
    key: 'getJustification',
    value: function getJustification() {
      return this.justification;
    }
  }, {
    key: 'setJustification',
    value: function setJustification(just) {
      this.justification = typeof just === 'string' ? Annotation.JustifyString[just] : just;
      return this;
    }

    // Render text beside the note.

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();

      if (!this.note) {
        throw new _vex.Vex.RERR('NoNoteForAnnotation', "Can't draw text annotation without an attached note.");
      }

      this.setRendered();
      var start = this.note.getModifierStartXY(_modifier.Modifier.Position.ABOVE, this.index);

      // We're changing context parameters. Save current state.
      this.context.save();
      this.context.setFont(this.font.family, this.font.size, this.font.weight);
      var text_width = this.context.measureText(this.text).width;

      // Estimate text height to be the same as the width of an 'm'.
      //
      // This is a hack to work around the inability to measure text height
      // in HTML5 Canvas (and SVG).
      var text_height = this.context.measureText('m').width;
      var x = void 0;
      var y = void 0;

      if (this.justification === Annotation.Justify.LEFT) {
        x = start.x;
      } else if (this.justification === Annotation.Justify.RIGHT) {
        x = start.x - text_width;
      } else if (this.justification === Annotation.Justify.CENTER) {
        x = start.x - text_width / 2;
      } else /* CENTER_STEM */{
          x = this.note.getStemX() - text_width / 2;
        }

      var stem_ext = void 0;
      var spacing = void 0;
      var has_stem = this.note.hasStem();
      var stave = this.note.getStave();

      // The position of the text varies based on whether or not the note
      // has a stem.
      if (has_stem) {
        stem_ext = this.note.getStem().getExtents();
        spacing = stave.getSpacingBetweenLines();
      }

      if (this.vert_justification === Annotation.VerticalJustify.BOTTOM) {
        // HACK: We need to compensate for the text's height since its origin
        // is bottom-right.
        y = stave.getYForBottomText(this.text_line + _tables.Flow.TEXT_HEIGHT_OFFSET_HACK);
        if (has_stem) {
          var stem_base = this.note.getStemDirection() === 1 ? stem_ext.baseY : stem_ext.topY;
          y = Math.max(y, stem_base + spacing * (this.text_line + 2));
        }
      } else if (this.vert_justification === Annotation.VerticalJustify.CENTER) {
        var yt = this.note.getYForTopText(this.text_line) - 1;
        var yb = stave.getYForBottomText(this.text_line);
        y = yt + (yb - yt) / 2 + text_height / 2;
      } else if (this.vert_justification === Annotation.VerticalJustify.TOP) {
        y = Math.min(stave.getYForTopText(this.text_line), this.note.getYs()[0] - 10);
        if (has_stem) {
          y = Math.min(y, stem_ext.topY - 5 - spacing * this.text_line);
        }
      } else /* CENTER_STEM */{
          var extents = this.note.getStemExtents();
          y = extents.topY + (extents.baseY - extents.topY) / 2 + text_height / 2;
        }

      L('Rendering annotation: ', this.text, x, y);
      this.context.fillText(this.text, x, y);
      this.context.restore();
    }
  }]);

  return Annotation;
}(_modifier.Modifier);

/***/ }),
/* 31 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Bend = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements tablature bends.

/**
   @param text Text for bend ("Full", "Half", etc.) (DEPRECATED)
   @param release If true, render a release. (DEPRECATED)
   @param phrase If set, ignore "text" and "release", and use the more
                 sophisticated phrase specified.

   Example of a phrase:

     [{
       type: UP,
       text: "whole"
       width: 8;
     },
     {
       type: DOWN,
       text: "whole"
       width: 8;
     },
     {
       type: UP,
       text: "half"
       width: 8;
     },
     {
       type: UP,
       text: "whole"
       width: 8;
     },
     {
       type: DOWN,
       text: "1 1/2"
       width: 8;
     }]
 */
var Bend = exports.Bend = function (_Modifier) {
  _inherits(Bend, _Modifier);

  _createClass(Bend, null, [{
    key: 'format',


    // ## Static Methods
    // Arrange bends in `ModifierContext`
    value: function format(bends, state) {
      if (!bends || bends.length === 0) return false;

      var last_width = 0;
      // Bends are always on top.
      var text_line = state.top_text_line;

      // Format Bends
      for (var i = 0; i < bends.length; ++i) {
        var bend = bends[i];
        bend.setXShift(last_width);
        last_width = bend.getWidth();
        bend.setTextLine(text_line);
      }

      state.right_shift += last_width;
      state.top_text_line += 1;
      return true;
    }

    // ## Prototype Methods

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'bends';
    }
  }, {
    key: 'UP',
    get: function get() {
      return 0;
    }
  }, {
    key: 'DOWN',
    get: function get() {
      return 1;
    }
  }]);

  function Bend(text, release, phrase) {
    _classCallCheck(this, Bend);

    var _this = _possibleConstructorReturn(this, (Bend.__proto__ || Object.getPrototypeOf(Bend)).call(this));

    _this.setAttribute('type', 'Bend');

    _this.text = text;
    _this.x_shift = 0;
    _this.release = release || false;
    _this.font = '10pt Arial';
    _this.render_options = {
      line_width: 1.5,
      line_style: '#777777',
      bend_width: 8,
      release_width: 8
    };

    if (phrase) {
      _this.phrase = phrase;
    } else {
      // Backward compatibility
      _this.phrase = [{ type: Bend.UP, text: _this.text }];
      if (_this.release) _this.phrase.push({ type: Bend.DOWN, text: '' });
    }

    _this.updateWidth();
    return _this;
  }

  _createClass(Bend, [{
    key: 'getCategory',
    value: function getCategory() {
      return Bend.CATEGORY;
    }
  }, {
    key: 'setXShift',
    value: function setXShift(value) {
      this.x_shift = value;
      this.updateWidth();
    }
  }, {
    key: 'setFont',
    value: function setFont(font) {
      this.font = font;return this;
    }
  }, {
    key: 'getText',
    value: function getText() {
      return this.text;
    }
  }, {
    key: 'updateWidth',
    value: function updateWidth() {
      var that = this;

      function measure_text(text) {
        var text_width = void 0;
        if (that.context) {
          text_width = that.context.measureText(text).width;
        } else {
          text_width = _tables.Flow.textWidth(text);
        }

        return text_width;
      }

      var total_width = 0;
      for (var i = 0; i < this.phrase.length; ++i) {
        var bend = this.phrase[i];
        if ('width' in bend) {
          total_width += bend.width;
        } else {
          var additional_width = bend.type === Bend.UP ? this.render_options.bend_width : this.render_options.release_width;

          bend.width = _vex.Vex.Max(additional_width, measure_text(bend.text)) + 3;
          bend.draw_width = bend.width / 2;
          total_width += bend.width;
        }
      }

      this.setWidth(total_width + this.x_shift);
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      if (!(this.note && this.index != null)) {
        throw new _vex.Vex.RERR('NoNoteForBend', "Can't draw bend without a note or index.");
      }

      this.setRendered();

      var start = this.note.getModifierStartXY(_modifier.Modifier.Position.RIGHT, this.index);
      start.x += 3;
      start.y += 0.5;
      var x_shift = this.x_shift;

      var ctx = this.context;
      var bend_height = this.note.getStave().getYForTopText(this.text_line) + 3;
      var annotation_y = this.note.getStave().getYForTopText(this.text_line) - 1;
      var that = this;

      function renderBend(x, y, width, height) {
        var cp_x = x + width;
        var cp_y = y;

        ctx.save();
        ctx.beginPath();
        ctx.setLineWidth(that.render_options.line_width);
        ctx.setStrokeStyle(that.render_options.line_style);
        ctx.setFillStyle(that.render_options.line_style);
        ctx.moveTo(x, y);
        ctx.quadraticCurveTo(cp_x, cp_y, x + width, height);
        ctx.stroke();
        ctx.restore();
      }

      function renderRelease(x, y, width, height) {
        ctx.save();
        ctx.beginPath();
        ctx.setLineWidth(that.render_options.line_width);
        ctx.setStrokeStyle(that.render_options.line_style);
        ctx.setFillStyle(that.render_options.line_style);
        ctx.moveTo(x, height);
        ctx.quadraticCurveTo(x + width, height, x + width, y);
        ctx.stroke();
        ctx.restore();
      }

      function renderArrowHead(x, y, direction) {
        var width = 4;
        var dir = direction || 1;

        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.lineTo(x - width, y + width * dir);
        ctx.lineTo(x + width, y + width * dir);
        ctx.closePath();
        ctx.fill();
      }

      function renderText(x, text) {
        ctx.save();
        ctx.setRawFont(that.font);
        var render_x = x - ctx.measureText(text).width / 2;
        ctx.fillText(text, render_x, annotation_y);
        ctx.restore();
      }

      var last_bend = null;
      var last_drawn_width = 0;
      for (var i = 0; i < this.phrase.length; ++i) {
        var bend = this.phrase[i];
        if (i === 0) bend.draw_width += x_shift;

        last_drawn_width = bend.draw_width + (last_bend ? last_bend.draw_width : 0) - (i === 1 ? x_shift : 0);
        if (bend.type === Bend.UP) {
          if (last_bend && last_bend.type === Bend.UP) {
            renderArrowHead(start.x, bend_height);
          }

          renderBend(start.x, start.y, last_drawn_width, bend_height);
        }

        if (bend.type === Bend.DOWN) {
          if (last_bend && last_bend.type === Bend.UP) {
            renderRelease(start.x, start.y, last_drawn_width, bend_height);
          }

          if (last_bend && last_bend.type === Bend.DOWN) {
            renderArrowHead(start.x, start.y, -1);
            renderRelease(start.x, start.y, last_drawn_width, bend_height);
          }

          if (last_bend === null) {
            last_drawn_width = bend.draw_width;
            renderRelease(start.x, start.y, last_drawn_width, bend_height);
          }
        }

        renderText(start.x + last_drawn_width, bend.text);
        last_bend = bend;
        last_bend.x = start.x;

        start.x += last_drawn_width;
      }

      // Final arrowhead and text
      if (last_bend.type === Bend.UP) {
        renderArrowHead(last_bend.x + last_drawn_width, bend_height);
      } else if (last_bend.type === Bend.DOWN) {
        renderArrowHead(last_bend.x + last_drawn_width, start.y, -1);
      }
    }
  }]);

  return Bend;
}(_modifier.Modifier);

/***/ }),
/* 32 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Vibrato = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _modifier = __webpack_require__(4);

var _bend = __webpack_require__(31);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class implements vibratos.

var Vibrato = exports.Vibrato = function (_Modifier) {
  _inherits(Vibrato, _Modifier);

  _createClass(Vibrato, null, [{
    key: 'format',


    // ## Static Methods
    // Arrange vibratos inside a `ModifierContext`.
    value: function format(vibratos, state, context) {
      if (!vibratos || vibratos.length === 0) return false;

      // Vibratos are always on top.
      var text_line = state.top_text_line;
      var width = 0;
      var shift = state.right_shift - 7;

      // If there's a bend, drop the text line
      var bends = context.getModifiers(_bend.Bend.CATEGORY);
      if (bends && bends.length > 0) {
        text_line--;
      }

      // Format Vibratos
      for (var i = 0; i < vibratos.length; ++i) {
        var vibrato = vibratos[i];
        vibrato.setXShift(shift);
        vibrato.setTextLine(text_line);
        width += vibrato.getWidth();
        shift += width;
      }

      state.right_shift += width;
      state.top_text_line += 1;
      return true;
    }

    // ## Prototype Methods

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'vibratos';
    }
  }]);

  function Vibrato() {
    _classCallCheck(this, Vibrato);

    var _this = _possibleConstructorReturn(this, (Vibrato.__proto__ || Object.getPrototypeOf(Vibrato)).call(this));

    _this.setAttribute('type', 'Vibrato');

    _this.position = _modifier.Modifier.Position.RIGHT;
    _this.render_options = {
      harsh: false,
      vibrato_width: 20,
      wave_height: 6,
      wave_width: 4,
      wave_girth: 2
    };

    _this.setVibratoWidth(_this.render_options.vibrato_width);
    return _this;
  }

  _createClass(Vibrato, [{
    key: 'getCategory',
    value: function getCategory() {
      return Vibrato.CATEGORY;
    }
  }, {
    key: 'setHarsh',
    value: function setHarsh(harsh) {
      this.render_options.harsh = harsh;return this;
    }
  }, {
    key: 'setVibratoWidth',
    value: function setVibratoWidth(width) {
      this.render_options.vibrato_width = width;
      this.setWidth(width);
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var ctx = this.checkContext();

      if (!this.note) {
        throw new _vex.Vex.RERR('NoNoteForVibrato', "Can't draw vibrato without an attached note.");
      }

      this.setRendered();
      var start = this.note.getModifierStartXY(_modifier.Modifier.Position.RIGHT, this.index);

      var vx = start.x + this.x_shift;
      var vy = this.note.getYForTopText(this.text_line) + 2;

      Vibrato.renderVibrato(ctx, vx, vy, this.render_options);
    }

    // Static rendering method that can be called from
    // other classes (e.g. VibratoBracket)

  }], [{
    key: 'renderVibrato',
    value: function renderVibrato(ctx, x, y, opts) {
      var harsh = opts.harsh,
          vibrato_width = opts.vibrato_width,
          wave_width = opts.wave_width,
          wave_girth = opts.wave_girth,
          wave_height = opts.wave_height;

      var num_waves = vibrato_width / wave_width;

      ctx.beginPath();

      var i = void 0;
      if (harsh) {
        ctx.moveTo(x, y + wave_girth + 1);
        for (i = 0; i < num_waves / 2; ++i) {
          ctx.lineTo(x + wave_width, y - wave_height / 2);
          x += wave_width;
          ctx.lineTo(x + wave_width, y + wave_height / 2);
          x += wave_width;
        }
        for (i = 0; i < num_waves / 2; ++i) {
          ctx.lineTo(x - wave_width, y - wave_height / 2 + wave_girth + 1);
          x -= wave_width;
          ctx.lineTo(x - wave_width, y + wave_height / 2 + wave_girth + 1);
          x -= wave_width;
        }
        ctx.fill();
      } else {
        ctx.moveTo(x, y + wave_girth);
        for (i = 0; i < num_waves / 2; ++i) {
          ctx.quadraticCurveTo(x + wave_width / 2, y - wave_height / 2, x + wave_width, y);
          x += wave_width;
          ctx.quadraticCurveTo(x + wave_width / 2, y + wave_height / 2, x + wave_width, y);
          x += wave_width;
        }

        for (i = 0; i < num_waves / 2; ++i) {
          ctx.quadraticCurveTo(x - wave_width / 2, y + wave_height / 2 + wave_girth, x - wave_width, y + wave_girth);
          x -= wave_width;
          ctx.quadraticCurveTo(x - wave_width / 2, y - wave_height / 2 + wave_girth, x - wave_width, y + wave_girth);
          x -= wave_width;
        }
        ctx.fill();
      }
    }
  }]);

  return Vibrato;
}(_modifier.Modifier);

/***/ }),
/* 33 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Stave = undefined;

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _tables = __webpack_require__(1);

var _stavebarline = __webpack_require__(34);

var _stavemodifier = __webpack_require__(7);

var _staverepetition = __webpack_require__(45);

var _stavesection = __webpack_require__(69);

var _stavetempo = __webpack_require__(70);

var _stavetext = __webpack_require__(71);

var _boundingbox = __webpack_require__(10);

var _clef = __webpack_require__(36);

var _keysignature = __webpack_require__(46);

var _timesignature = __webpack_require__(37);

var _stavevolta = __webpack_require__(47);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

var Stave = exports.Stave = function (_Element) {
  _inherits(Stave, _Element);

  function Stave(x, y, width, options) {
    _classCallCheck(this, Stave);

    var _this = _possibleConstructorReturn(this, (Stave.__proto__ || Object.getPrototypeOf(Stave)).call(this));

    _this.setAttribute('type', 'Stave');

    _this.x = x;
    _this.y = y;
    _this.width = width;
    _this.formatted = false;
    _this.start_x = x + 5;
    _this.end_x = x + width;
    _this.modifiers = []; // stave modifiers (clef, key, time, barlines, coda, segno, etc.)
    _this.measure = 0;
    _this.clef = 'treble';
    _this.font = {
      family: 'sans-serif',
      size: 8,
      weight: ''
    };
    _this.options = {
      vertical_bar_width: 10, // Width around vertical bar end-marker
      glyph_spacing_px: 10,
      num_lines: 5,
      fill_style: '#999999',
      left_bar: true, // draw vertical bar on left
      right_bar: true, // draw vertical bar on right
      spacing_between_lines_px: 10, // in pixels
      space_above_staff_ln: 4, // in staff lines
      space_below_staff_ln: 4, // in staff lines
      top_text_position: 1 // in staff lines
    };
    _this.bounds = { x: _this.x, y: _this.y, w: _this.width, h: 0 };
    _vex.Vex.Merge(_this.options, options);

    _this.resetLines();

    var BARTYPE = _stavebarline.Barline.type;
    // beg bar
    _this.addModifier(new _stavebarline.Barline(_this.options.left_bar ? BARTYPE.SINGLE : BARTYPE.NONE));
    // end bar
    _this.addEndModifier(new _stavebarline.Barline(_this.options.right_bar ? BARTYPE.SINGLE : BARTYPE.NONE));
    return _this;
  }

  _createClass(Stave, [{
    key: 'space',
    value: function space(spacing) {
      return this.options.spacing_between_lines_px * spacing;
    }
  }, {
    key: 'resetLines',
    value: function resetLines() {
      this.options.line_config = [];
      for (var i = 0; i < this.options.num_lines; i++) {
        this.options.line_config.push({ visible: true });
      }
      this.height = (this.options.num_lines + this.options.space_above_staff_ln) * this.options.spacing_between_lines_px;
      this.options.bottom_text_position = this.options.num_lines;
    }
  }, {
    key: 'getOptions',
    value: function getOptions() {
      return this.options;
    }
  }, {
    key: 'setNoteStartX',
    value: function setNoteStartX(x) {
      if (!this.formatted) this.format();

      this.start_x = x;
      return this;
    }
  }, {
    key: 'getNoteStartX',
    value: function getNoteStartX() {
      if (!this.formatted) this.format();

      return this.start_x;
    }
  }, {
    key: 'getNoteEndX',
    value: function getNoteEndX() {
      if (!this.formatted) this.format();

      return this.end_x;
    }
  }, {
    key: 'getTieStartX',
    value: function getTieStartX() {
      return this.start_x;
    }
  }, {
    key: 'getTieEndX',
    value: function getTieEndX() {
      return this.x + this.width;
    }
  }, {
    key: 'getX',
    value: function getX() {
      return this.x;
    }
  }, {
    key: 'getNumLines',
    value: function getNumLines() {
      return this.options.num_lines;
    }
  }, {
    key: 'setNumLines',
    value: function setNumLines(lines) {
      this.options.num_lines = parseInt(lines, 10);
      this.resetLines();
      return this;
    }
  }, {
    key: 'setY',
    value: function setY(y) {
      this.y = y;return this;
    }
  }, {
    key: 'getTopLineTopY',
    value: function getTopLineTopY() {
      return this.getYForLine(0) - _tables.Flow.STAVE_LINE_THICKNESS / 2;
    }
  }, {
    key: 'getBottomLineBottomY',
    value: function getBottomLineBottomY() {
      return this.getYForLine(this.getNumLines() - 1) + _tables.Flow.STAVE_LINE_THICKNESS / 2;
    }
  }, {
    key: 'setX',
    value: function setX(x) {
      var shift = x - this.x;
      this.formatted = false;
      this.x = x;
      this.start_x += shift;
      this.end_x += shift;
      for (var i = 0; i < this.modifiers.length; i++) {
        var mod = this.modifiers[i];
        if (mod.x !== undefined) {
          mod.x += shift;
        }
      }
      return this;
    }
  }, {
    key: 'setWidth',
    value: function setWidth(width) {
      this.formatted = false;
      this.width = width;
      this.end_x = this.x + width;

      // reset the x position of the end barline (TODO(0xfe): This makes no sense)
      // this.modifiers[1].setX(this.end_x);
      return this;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'getStyle',
    value: function getStyle() {
      return _extends({
        fillStyle: this.options.fill_style,
        strokeStyle: this.options.fill_style, // yes, this is correct for legacy compatibility
        lineWidth: _tables.Flow.STAVE_LINE_THICKNESS
      }, this.style || {});
    }
  }, {
    key: 'setMeasure',
    value: function setMeasure(measure) {
      this.measure = measure;return this;
    }

    /**
     * Gets the pixels to shift from the beginning of the stave
     * following the modifier at the provided index
     * @param  {Number} index The index from which to determine the shift
     * @return {Number}       The amount of pixels shifted
     */

  }, {
    key: 'getModifierXShift',
    value: function getModifierXShift() {
      var index = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;

      if (typeof index !== 'number') {
        throw new _vex.Vex.RERR('InvalidIndex', 'Must be of number type');
      }

      if (!this.formatted) this.format();

      if (this.getModifiers(_stavemodifier.StaveModifier.Position.BEGIN).length === 1) {
        return 0;
      }

      var start_x = this.start_x - this.x;
      var begBarline = this.modifiers[0];
      if (begBarline.getType() === _stavebarline.Barline.type.REPEAT_BEGIN && start_x > begBarline.getWidth()) {
        start_x -= begBarline.getWidth();
      }

      return start_x;
    }

    // Coda & Segno Symbol functions

  }, {
    key: 'setRepetitionTypeLeft',
    value: function setRepetitionTypeLeft(type, y) {
      this.modifiers.push(new _staverepetition.Repetition(type, this.x, y));
      return this;
    }
  }, {
    key: 'setRepetitionTypeRight',
    value: function setRepetitionTypeRight(type, y) {
      this.modifiers.push(new _staverepetition.Repetition(type, this.x, y));
      return this;
    }

    // Volta functions

  }, {
    key: 'setVoltaType',
    value: function setVoltaType(type, number_t, y) {
      this.modifiers.push(new _stavevolta.Volta(type, number_t, this.x, y));
      return this;
    }

    // Section functions

  }, {
    key: 'setSection',
    value: function setSection(section, y) {
      this.modifiers.push(new _stavesection.StaveSection(section, this.x, y));
      return this;
    }

    // Tempo functions

  }, {
    key: 'setTempo',
    value: function setTempo(tempo, y) {
      this.modifiers.push(new _stavetempo.StaveTempo(tempo, this.x, y));
      return this;
    }

    // Text functions

  }, {
    key: 'setText',
    value: function setText(text, position, options) {
      this.modifiers.push(new _stavetext.StaveText(text, position, options));
      return this;
    }
  }, {
    key: 'getHeight',
    value: function getHeight() {
      return this.height;
    }
  }, {
    key: 'getSpacingBetweenLines',
    value: function getSpacingBetweenLines() {
      return this.options.spacing_between_lines_px;
    }
  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      return new _boundingbox.BoundingBox(this.x, this.y, this.width, this.getBottomY() - this.y);
    }
  }, {
    key: 'getBottomY',
    value: function getBottomY() {
      var options = this.options;
      var spacing = options.spacing_between_lines_px;
      var score_bottom = this.getYForLine(options.num_lines) + options.space_below_staff_ln * spacing;

      return score_bottom;
    }
  }, {
    key: 'getBottomLineY',
    value: function getBottomLineY() {
      return this.getYForLine(this.options.num_lines);
    }

    // This returns the y for the *center* of a staff line

  }, {
    key: 'getYForLine',
    value: function getYForLine(line) {
      var options = this.options;
      var spacing = options.spacing_between_lines_px;
      var headroom = options.space_above_staff_ln;

      var y = this.y + line * spacing + headroom * spacing;

      return y;
    }
  }, {
    key: 'getLineForY',
    value: function getLineForY(y) {
      // Does the reverse of getYForLine - somewhat dumb and just calls
      // getYForLine until the right value is reaches

      var options = this.options;
      var spacing = options.spacing_between_lines_px;
      var headroom = options.space_above_staff_ln;
      return (y - this.y) / spacing - headroom;
    }
  }, {
    key: 'getYForTopText',
    value: function getYForTopText(line) {
      var l = line || 0;
      return this.getYForLine(-l - this.options.top_text_position);
    }
  }, {
    key: 'getYForBottomText',
    value: function getYForBottomText(line) {
      var l = line || 0;
      return this.getYForLine(this.options.bottom_text_position + l);
    }
  }, {
    key: 'getYForNote',
    value: function getYForNote(line) {
      var options = this.options;
      var spacing = options.spacing_between_lines_px;
      var headroom = options.space_above_staff_ln;
      var y = this.y + headroom * spacing + 5 * spacing - line * spacing;

      return y;
    }
  }, {
    key: 'getYForGlyphs',
    value: function getYForGlyphs() {
      return this.getYForLine(3);
    }
  }, {
    key: 'addModifier',
    value: function addModifier(modifier, position) {
      if (position !== undefined) {
        modifier.setPosition(position);
      }

      modifier.setStave(this);
      this.formatted = false;
      this.modifiers.push(modifier);
      return this;
    }
  }, {
    key: 'addEndModifier',
    value: function addEndModifier(modifier) {
      this.addModifier(modifier, _stavemodifier.StaveModifier.Position.END);
      return this;
    }

    // Bar Line functions

  }, {
    key: 'setBegBarType',
    value: function setBegBarType(type) {
      // Only valid bar types at beginning of stave is none, single or begin repeat
      var _Barline$type = _stavebarline.Barline.type,
          SINGLE = _Barline$type.SINGLE,
          REPEAT_BEGIN = _Barline$type.REPEAT_BEGIN,
          NONE = _Barline$type.NONE;

      if (type === SINGLE || type === REPEAT_BEGIN || type === NONE) {
        this.modifiers[0].setType(type);
        this.formatted = false;
      }
      return this;
    }
  }, {
    key: 'setEndBarType',
    value: function setEndBarType(type) {
      // Repeat end not valid at end of stave
      if (type !== _stavebarline.Barline.type.REPEAT_BEGIN) {
        this.modifiers[1].setType(type);
        this.formatted = false;
      }
      return this;
    }
  }, {
    key: 'setClef',
    value: function setClef(clefSpec, size, annotation, position) {
      if (position === undefined) {
        position = _stavemodifier.StaveModifier.Position.BEGIN;
      }

      this.clef = clefSpec;
      var clefs = this.getModifiers(position, _clef.Clef.CATEGORY);
      if (clefs.length === 0) {
        this.addClef(clefSpec, size, annotation, position);
      } else {
        clefs[0].setType(clefSpec, size, annotation);
      }

      return this;
    }
  }, {
    key: 'setEndClef',
    value: function setEndClef(clefSpec, size, annotation) {
      this.setClef(clefSpec, size, annotation, _stavemodifier.StaveModifier.Position.END);
      return this;
    }
  }, {
    key: 'setKeySignature',
    value: function setKeySignature(keySpec, cancelKeySpec, position) {
      if (position === undefined) {
        position = _stavemodifier.StaveModifier.Position.BEGIN;
      }

      var keySignatures = this.getModifiers(position, _keysignature.KeySignature.CATEGORY);
      if (keySignatures.length === 0) {
        this.addKeySignature(keySpec, cancelKeySpec, position);
      } else {
        keySignatures[0].setKeySig(keySpec, cancelKeySpec);
      }

      return this;
    }
  }, {
    key: 'setEndKeySignature',
    value: function setEndKeySignature(keySpec, cancelKeySpec) {
      this.setKeySignature(keySpec, cancelKeySpec, _stavemodifier.StaveModifier.Position.END);
      return this;
    }
  }, {
    key: 'setTimeSignature',
    value: function setTimeSignature(timeSpec, customPadding, position) {
      if (position === undefined) {
        position = _stavemodifier.StaveModifier.Position.BEGIN;
      }

      var timeSignatures = this.getModifiers(position, _timesignature.TimeSignature.CATEGORY);
      if (timeSignatures.length === 0) {
        this.addTimeSignature(timeSpec, customPadding, position);
      } else {
        timeSignatures[0].setTimeSig(timeSpec);
      }

      return this;
    }
  }, {
    key: 'setEndTimeSignature',
    value: function setEndTimeSignature(timeSpec, customPadding) {
      this.setTimeSignature(timeSpec, customPadding, _stavemodifier.StaveModifier.Position.END);
      return this;
    }
  }, {
    key: 'addKeySignature',
    value: function addKeySignature(keySpec, cancelKeySpec, position) {
      this.addModifier(new _keysignature.KeySignature(keySpec, cancelKeySpec), position);
      return this;
    }
  }, {
    key: 'addClef',
    value: function addClef(clef, size, annotation, position) {
      if (position === undefined || position === _stavemodifier.StaveModifier.Position.BEGIN) {
        this.clef = clef;
      }

      this.addModifier(new _clef.Clef(clef, size, annotation), position);
      return this;
    }
  }, {
    key: 'addEndClef',
    value: function addEndClef(clef, size, annotation) {
      this.addClef(clef, size, annotation, _stavemodifier.StaveModifier.Position.END);
      return this;
    }
  }, {
    key: 'addTimeSignature',
    value: function addTimeSignature(timeSpec, customPadding, position) {
      this.addModifier(new _timesignature.TimeSignature(timeSpec, customPadding), position);
      return this;
    }
  }, {
    key: 'addEndTimeSignature',
    value: function addEndTimeSignature(timeSpec, customPadding) {
      this.addTimeSignature(timeSpec, customPadding, _stavemodifier.StaveModifier.Position.END);
      return this;
    }

    // Deprecated

  }, {
    key: 'addTrebleGlyph',
    value: function addTrebleGlyph() {
      this.addClef('treble');
      return this;
    }
  }, {
    key: 'getModifiers',
    value: function getModifiers(position, category) {
      if (position === undefined) return this.modifiers;

      return this.modifiers.filter(function (modifier) {
        return position === modifier.getPosition() && (category === undefined || category === modifier.getCategory());
      });
    }
  }, {
    key: 'sortByCategory',
    value: function sortByCategory(items, order) {
      for (var i = items.length - 1; i >= 0; i--) {
        for (var j = 0; j < i; j++) {
          if (order[items[j].getCategory()] > order[items[j + 1].getCategory()]) {
            var temp = items[j];
            items[j] = items[j + 1];
            items[j + 1] = temp;
          }
        }
      }
    }
  }, {
    key: 'format',
    value: function format() {
      var begBarline = this.modifiers[0];
      var endBarline = this.modifiers[1];

      var begModifiers = this.getModifiers(_stavemodifier.StaveModifier.Position.BEGIN);
      var endModifiers = this.getModifiers(_stavemodifier.StaveModifier.Position.END);

      this.sortByCategory(begModifiers, {
        barlines: 0, clefs: 1, keysignatures: 2, timesignatures: 3
      });

      this.sortByCategory(endModifiers, {
        timesignatures: 0, keysignatures: 1, barlines: 2, clefs: 3
      });

      if (begModifiers.length > 1 && begBarline.getType() === _stavebarline.Barline.type.REPEAT_BEGIN) {
        begModifiers.push(begModifiers.splice(0, 1)[0]);
        begModifiers.splice(0, 0, new _stavebarline.Barline(_stavebarline.Barline.type.SINGLE));
      }

      if (endModifiers.indexOf(endBarline) > 0) {
        endModifiers.splice(0, 0, new _stavebarline.Barline(_stavebarline.Barline.type.NONE));
      }

      var width = void 0;
      var padding = void 0;
      var modifier = void 0;
      var offset = 0;
      var x = this.x;
      for (var i = 0; i < begModifiers.length; i++) {
        modifier = begModifiers[i];
        padding = modifier.getPadding(i + offset);
        width = modifier.getWidth();

        x += padding;
        modifier.setX(x);
        x += width;

        if (padding + width === 0) offset--;
      }

      this.start_x = x;
      x = this.x + this.width;

      for (var _i = 0; _i < endModifiers.length; _i++) {
        modifier = endModifiers[_i];
        x -= modifier.getPadding(_i);
        if (_i !== 0) {
          x -= modifier.getWidth();
        }

        modifier.setX(x);

        if (_i === 0) {
          x -= modifier.getWidth();
        }
      }

      this.end_x = endModifiers.length === 1 ? this.x + this.width : x;
      this.formatted = true;
    }

    /**
     * All drawing functions below need the context to be set.
     */

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      if (!this.formatted) this.format();

      var num_lines = this.options.num_lines;
      var width = this.width;
      var x = this.x;
      var y = void 0;

      // Render lines
      for (var line = 0; line < num_lines; line++) {
        y = this.getYForLine(line);

        this.applyStyle();
        if (this.options.line_config[line].visible) {
          this.context.beginPath();
          this.context.moveTo(x, y);
          this.context.lineTo(x + width, y);
          this.context.stroke();
        }
        this.restoreStyle();
      }

      // Draw the modifiers (bar lines, coda, segno, repeat brackets, etc.)
      for (var i = 0; i < this.modifiers.length; i++) {
        // Only draw modifier if it has a draw function
        if (typeof this.modifiers[i].draw === 'function') {
          this.modifiers[i].draw(this, this.getModifierXShift(i));
        }
      }

      // Render measure numbers
      if (this.measure > 0) {
        this.context.save();
        this.context.setFont(this.font.family, this.font.size, this.font.weight);
        var text_width = this.context.measureText('' + this.measure).width;
        y = this.getYForTopText(0) + 3;
        this.context.fillText('' + this.measure, this.x - text_width / 2, y);
        this.context.restore();
      }

      return this;
    }

    // Draw Simple barlines for backward compatability
    // Do not delete - draws the beginning bar of the stave

  }, {
    key: 'drawVertical',
    value: function drawVertical(x, isDouble) {
      this.drawVerticalFixed(this.x + x, isDouble);
    }
  }, {
    key: 'drawVerticalFixed',
    value: function drawVerticalFixed(x, isDouble) {
      this.checkContext();

      var top_line = this.getYForLine(0);
      var bottom_line = this.getYForLine(this.options.num_lines - 1);
      if (isDouble) {
        this.context.fillRect(x - 3, top_line, 1, bottom_line - top_line + 1);
      }
      this.context.fillRect(x, top_line, 1, bottom_line - top_line + 1);
    }
  }, {
    key: 'drawVerticalBar',
    value: function drawVerticalBar(x) {
      this.drawVerticalBarFixed(this.x + x, false);
    }
  }, {
    key: 'drawVerticalBarFixed',
    value: function drawVerticalBarFixed(x) {
      this.checkContext();

      var top_line = this.getYForLine(0);
      var bottom_line = this.getYForLine(this.options.num_lines - 1);
      this.context.fillRect(x, top_line, 1, bottom_line - top_line + 1);
    }

    /**
     * Get the current configuration for the Stave.
     * @return {Array} An array of configuration objects.
     */

  }, {
    key: 'getConfigForLines',
    value: function getConfigForLines() {
      return this.options.line_config;
    }

    /**
     * Configure properties of the lines in the Stave
     * @param line_number The index of the line to configure.
     * @param line_config An configuration object for the specified line.
     * @throws Vex.RERR "StaveConfigError" When the specified line number is out of
     *   range of the number of lines specified in the constructor.
     */

  }, {
    key: 'setConfigForLine',
    value: function setConfigForLine(line_number, line_config) {
      if (line_number >= this.options.num_lines || line_number < 0) {
        throw new _vex.Vex.RERR('StaveConfigError', 'The line number must be within the range of the number of lines in the Stave.');
      }

      if (line_config.visible === undefined) {
        throw new _vex.Vex.RERR('StaveConfigError', "The line configuration object is missing the 'visible' property.");
      }

      if (typeof line_config.visible !== 'boolean') {
        throw new _vex.Vex.RERR('StaveConfigError', "The line configuration objects 'visible' property must be true or false.");
      }

      this.options.line_config[line_number] = line_config;

      return this;
    }

    /**
     * Set the staff line configuration array for all of the lines at once.
     * @param lines_configuration An array of line configuration objects.  These objects
     *   are of the same format as the single one passed in to setLineConfiguration().
     *   The caller can set null for any line config entry if it is desired that the default be used
     * @throws Vex.RERR "StaveConfigError" When the lines_configuration array does not have
     *   exactly the same number of elements as the num_lines configuration object set in
     *   the constructor.
     */

  }, {
    key: 'setConfigForLines',
    value: function setConfigForLines(lines_configuration) {
      if (lines_configuration.length !== this.options.num_lines) {
        throw new _vex.Vex.RERR('StaveConfigError', 'The length of the lines configuration array must match the number of lines in the Stave');
      }

      // Make sure the defaults are present in case an incomplete set of
      //  configuration options were supplied.
      for (var line_config in lines_configuration) {
        // Allow 'null' to be used if the caller just wants the default for a particular node.
        if (!lines_configuration[line_config]) {
          lines_configuration[line_config] = this.options.line_config[line_config];
        }
        _vex.Vex.Merge(this.options.line_config[line_config], lines_configuration[line_config]);
      }

      this.options.line_config = lines_configuration;

      return this;
    }
  }]);

  return Stave;
}(_element.Element);

/***/ }),
/* 34 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Barline = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _tables = __webpack_require__(1);

var _stavemodifier = __webpack_require__(7);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// Author Larry Kuhns 2011

var Barline = exports.Barline = function (_StaveModifier) {
  _inherits(Barline, _StaveModifier);

  _createClass(Barline, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'barlines';
    }
  }, {
    key: 'type',
    get: function get() {
      return {
        SINGLE: 1,
        DOUBLE: 2,
        END: 3,
        REPEAT_BEGIN: 4,
        REPEAT_END: 5,
        REPEAT_BOTH: 6,
        NONE: 7
      };
    }
  }, {
    key: 'typeString',
    get: function get() {
      return {
        single: Barline.type.SINGLE,
        double: Barline.type.DOUBLE,
        end: Barline.type.END,
        repeatBegin: Barline.type.REPEAT_BEGIN,
        repeatEnd: Barline.type.REPEAT_END,
        repeatBoth: Barline.type.REPEAT_BOTH,
        none: Barline.type.NONE
      };
    }

    /**
     * @constructor
     */

  }]);

  function Barline(type) {
    _classCallCheck(this, Barline);

    var _this = _possibleConstructorReturn(this, (Barline.__proto__ || Object.getPrototypeOf(Barline)).call(this));

    _this.setAttribute('type', 'Barline');
    _this.thickness = _tables.Flow.STAVE_LINE_THICKNESS;

    var TYPE = Barline.type;
    _this.widths = {};
    _this.widths[TYPE.SINGLE] = 5;
    _this.widths[TYPE.DOUBLE] = 5;
    _this.widths[TYPE.END] = 5;
    _this.widths[TYPE.REPEAT_BEGIN] = 5;
    _this.widths[TYPE.REPEAT_END] = 5;
    _this.widths[TYPE.REPEAT_BOTH] = 5;
    _this.widths[TYPE.NONE] = 5;

    _this.paddings = {};
    _this.paddings[TYPE.SINGLE] = 0;
    _this.paddings[TYPE.DOUBLE] = 0;
    _this.paddings[TYPE.END] = 0;
    _this.paddings[TYPE.REPEAT_BEGIN] = 15;
    _this.paddings[TYPE.REPEAT_END] = 15;
    _this.paddings[TYPE.REPEAT_BOTH] = 15;
    _this.paddings[TYPE.NONE] = 0;

    _this.setPosition(_stavemodifier.StaveModifier.Position.BEGIN);
    _this.setType(type);
    return _this;
  }

  _createClass(Barline, [{
    key: 'getCategory',
    value: function getCategory() {
      return Barline.CATEGORY;
    }
  }, {
    key: 'getType',
    value: function getType() {
      return this.type;
    }
  }, {
    key: 'setType',
    value: function setType(type) {
      this.type = typeof type === 'string' ? Barline.typeString[type] : type;

      this.setWidth(this.widths[this.type]);
      this.setPadding(this.paddings[this.type]);
      return this;
    }

    // Draw barlines

  }, {
    key: 'draw',
    value: function draw(stave) {
      stave.checkContext();
      this.setRendered();

      switch (this.type) {
        case Barline.type.SINGLE:
          this.drawVerticalBar(stave, this.x, false);
          break;
        case Barline.type.DOUBLE:
          this.drawVerticalBar(stave, this.x, true);
          break;
        case Barline.type.END:
          this.drawVerticalEndBar(stave, this.x);
          break;
        case Barline.type.REPEAT_BEGIN:
          // If the barline is shifted over (in front of clef/time/key)
          // Draw vertical bar at the beginning.
          this.drawRepeatBar(stave, this.x, true);
          if (stave.getX() !== this.x) {
            this.drawVerticalBar(stave, stave.getX());
          }

          break;
        case Barline.type.REPEAT_END:
          this.drawRepeatBar(stave, this.x, false);
          break;
        case Barline.type.REPEAT_BOTH:
          this.drawRepeatBar(stave, this.x, false);
          this.drawRepeatBar(stave, this.x, true);
          break;
        default:
          // Default is NONE, so nothing to draw
          break;
      }
    }
  }, {
    key: 'drawVerticalBar',
    value: function drawVerticalBar(stave, x, double_bar) {
      stave.checkContext();
      var topY = stave.getTopLineTopY();
      var botY = stave.getBottomLineBottomY();
      if (double_bar) {
        stave.context.fillRect(x - 3, topY, 1, botY - topY);
      }
      stave.context.fillRect(x, topY, 1, botY - topY);
    }
  }, {
    key: 'drawVerticalEndBar',
    value: function drawVerticalEndBar(stave, x) {
      stave.checkContext();
      var topY = stave.getTopLineTopY();
      var botY = stave.getBottomLineBottomY();
      stave.context.fillRect(x - 5, topY, 1, botY - topY);
      stave.context.fillRect(x - 2, topY, 3, botY - topY);
    }
  }, {
    key: 'drawRepeatBar',
    value: function drawRepeatBar(stave, x, begin) {
      stave.checkContext();

      var topY = stave.getTopLineTopY();
      var botY = stave.getBottomLineBottomY();
      var x_shift = 3;

      if (!begin) {
        x_shift = -5;
      }

      stave.context.fillRect(x + x_shift, topY, 1, botY - topY);
      stave.context.fillRect(x - 2, topY, 3, botY - topY);

      var dot_radius = 2;

      // Shift dots left or right
      if (begin) {
        x_shift += 4;
      } else {
        x_shift -= 4;
      }

      var dot_x = x + x_shift + dot_radius / 2;

      // calculate the y offset based on number of stave lines
      var y_offset = (stave.getNumLines() - 1) * stave.getSpacingBetweenLines();
      y_offset = y_offset / 2 - stave.getSpacingBetweenLines() / 2;
      var dot_y = topY + y_offset + dot_radius / 2;

      // draw the top repeat dot
      stave.context.beginPath();
      stave.context.arc(dot_x, dot_y, dot_radius, 0, Math.PI * 2, false);
      stave.context.fill();

      // draw the bottom repeat dot
      dot_y += stave.getSpacingBetweenLines();
      stave.context.beginPath();
      stave.context.arc(dot_x, dot_y, dot_radius, 0, Math.PI * 2, false);
      stave.context.fill();
    }
  }]);

  return Barline;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 35 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TextNote = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _note = __webpack_require__(6);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// `TextNote` is a notation element that is positioned in time. Generally
// meant for objects that sit above/below the staff and inline with each other.
// Examples of this would be such as dynamics, lyrics, chord changes, etc.

var TextNote = exports.TextNote = function (_Note) {
  _inherits(TextNote, _Note);

  _createClass(TextNote, null, [{
    key: 'Justification',
    get: function get() {
      return {
        LEFT: 1,
        CENTER: 2,
        RIGHT: 3
      };
    }

    // Glyph data

  }, {
    key: 'GLYPHS',
    get: function get() {
      return {
        'segno': {
          code: 'v8c',
          point: 40,
          x_shift: 0,
          y_shift: -10
          // width: 10 // optional
        },
        'tr': {
          code: 'v1f',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'mordent_upper': {
          code: 'v1e',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'mordent_lower': {
          code: 'v45',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'f': {
          code: 'vba',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'p': {
          code: 'vbf',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'm': {
          code: 'v62',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        's': {
          code: 'v4a',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'z': {
          code: 'v80',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        },
        'coda': {
          code: 'v4d',
          point: 40,
          x_shift: 0,
          y_shift: -8
          // width: 10 // optional
        },
        'pedal_open': {
          code: 'v36',
          point: 40,
          x_shift: 0,
          y_shift: 0
        },
        'pedal_close': {
          code: 'v5d',
          point: 40,
          x_shift: 0,
          y_shift: 3
        },
        'caesura_straight': {
          code: 'v34',
          point: 40,
          x_shift: 0,
          y_shift: 2
        },
        'caesura_curved': {
          code: 'v4b',
          point: 40,
          x_shift: 0,
          y_shift: 2
        },
        'breath': {
          code: 'v6c',
          point: 40,
          x_shift: 0,
          y_shift: 0
        },
        'tick': {
          code: 'v6f',
          point: 50,
          x_shift: 0,
          y_shift: 0
        },
        'turn': {
          code: 'v72',
          point: 40,
          x_shift: 0,
          y_shift: 0
        },
        'turn_inverted': {
          code: 'v33',
          point: 40,
          x_shift: 0,
          y_shift: 0
        },

        // DEPRECATED - please use "mordent_upper" or "mordent_lower"
        'mordent': {
          code: 'v1e',
          point: 40,
          x_shift: 0,
          y_shift: 0
          // width: 10 // optional
        }
      };
    }
  }]);

  function TextNote(text_struct) {
    _classCallCheck(this, TextNote);

    var _this = _possibleConstructorReturn(this, (TextNote.__proto__ || Object.getPrototypeOf(TextNote)).call(this, text_struct));

    _this.setAttribute('type', 'TextNote');

    // Note properties
    _this.text = text_struct.text;
    _this.superscript = text_struct.superscript;
    _this.subscript = text_struct.subscript;
    _this.glyph_type = text_struct.glyph;
    _this.glyph = null;
    _this.font = {
      family: 'Arial',
      size: 12,
      weight: ''
    };

    // Set font
    if (text_struct.font) _this.font = text_struct.font;

    // Determine and set initial note width. Note that the text width is
    // an approximation and isn't very accurate. The only way to accurately
    // measure the length of text is with `canvasmeasureText()`
    if (_this.glyph_type) {
      var struct = TextNote.GLYPHS[_this.glyph_type];
      if (!struct) throw new _vex.Vex.RERR('Invalid glyph type: ' + _this.glyph_type);

      _this.glyph = new _glyph.Glyph(struct.code, struct.point, { cache: false });

      if (struct.width) {
        _this.setWidth(struct.width);
      } else {
        _this.setWidth(_this.glyph.getMetrics().width);
      }

      _this.glyph_struct = struct;
    } else {
      _this.setWidth(_tables.Flow.textWidth(_this.text));
    }
    _this.line = text_struct.line || 0;
    _this.smooth = text_struct.smooth || false;
    _this.ignore_ticks = text_struct.ignore_ticks || false;
    _this.justification = TextNote.Justification.LEFT;
    return _this;
  }

  // Set the horizontal justification of the TextNote


  _createClass(TextNote, [{
    key: 'setJustification',
    value: function setJustification(just) {
      this.justification = just;
      return this;
    }

    // Set the Stave line on which the note should be placed

  }, {
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;
      return this;
    }

    // Pre-render formatting

  }, {
    key: 'preFormat',
    value: function preFormat() {
      this.checkContext();

      if (this.preFormatted) return;

      if (this.smooth) {
        this.setWidth(0);
      } else {
        if (this.glyph) {
          // Width already set.
        } else {
          this.setWidth(this.context.measureText(this.text).width);
        }
      }

      if (this.justification === TextNote.Justification.CENTER) {
        this.extraLeftPx = this.width / 2;
      } else if (this.justification === TextNote.Justification.RIGHT) {
        this.extraLeftPx = this.width;
      }

      this.setPreFormatted(true);
    }

    // Renders the TextNote

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();

      if (!this.stave) {
        throw new _vex.Vex.RERR('NoStave', "Can't draw without a stave.");
      }

      this.setRendered();
      var ctx = this.context;
      var x = this.getAbsoluteX();
      if (this.justification === TextNote.Justification.CENTER) {
        x -= this.getWidth() / 2;
      } else if (this.justification === TextNote.Justification.RIGHT) {
        x -= this.getWidth();
      }

      var y = void 0;
      if (this.glyph) {
        y = this.stave.getYForLine(this.line + -3);
        this.glyph.render(this.context, x + this.glyph_struct.x_shift, y + this.glyph_struct.y_shift);
      } else {
        y = this.stave.getYForLine(this.line + -3);
        this.applyStyle(ctx);
        ctx.setFont(this.font.family, this.font.size, this.font.weight);
        ctx.fillText(this.text, x, y);

        // Width of the letter M gives us the approximate height of the text
        var height = ctx.measureText('M').width;
        // Get accurate width of text
        var width = ctx.measureText(this.text).width;

        // Write superscript
        if (this.superscript) {
          ctx.setFont(this.font.family, this.font.size / 1.3, this.font.weight);
          ctx.fillText(this.superscript, x + width + 2, y - height / 2.2);
        }

        // Write subscript
        if (this.subscript) {
          ctx.setFont(this.font.family, this.font.size / 1.3, this.font.weight);
          ctx.fillText(this.subscript, x + width + 2, y + height / 2.2 - 1);
        }

        this.restoreStyle(ctx);
      }
    }
  }]);

  return TextNote;
}(_note.Note);

/***/ }),
/* 36 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Clef = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _stavemodifier = __webpack_require__(7);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna Cheppudira 2013.
// Co-author: Benjamin W. Bohl
//
// ## Description
//
// This file implements various types of clefs that can be rendered on a stave.
//
// See `tests/clef_tests.js` for usage examples.

// To enable logging for this class, set `Vex.Flow.Clef.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Clef.DEBUG) _vex.Vex.L('Vex.Flow.Clef', args);
}

var Clef = exports.Clef = function (_StaveModifier) {
  _inherits(Clef, _StaveModifier);

  _createClass(Clef, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'clefs';
    }

    // Every clef name is associated with a glyph code from the font file
    // and a default stave line number.

  }, {
    key: 'types',
    get: function get() {
      return {
        'treble': {
          code: 'v83',
          line: 3
        },
        'bass': {
          code: 'v79',
          line: 1
        },
        'alto': {
          code: 'vad',
          line: 2
        },
        'tenor': {
          code: 'vad',
          line: 1
        },
        'percussion': {
          code: 'v59',
          line: 2
        },
        'soprano': {
          code: 'vad',
          line: 4
        },
        'mezzo-soprano': {
          code: 'vad',
          line: 3
        },
        'baritone-c': {
          code: 'vad',
          line: 0
        },
        'baritone-f': {
          code: 'v79',
          line: 2
        },
        'subbass': {
          code: 'v79',
          line: 0
        },
        'french': {
          code: 'v83',
          line: 4
        },
        'tab': {
          code: 'v2f'
        }
      };
    }

    // Sizes affect the point-size of the clef.

  }, {
    key: 'sizes',
    get: function get() {
      return {
        'default': 40,
        'small': 32
      };
    }

    // Annotations attach to clefs -- such as "8" for octave up or down.

  }, {
    key: 'annotations',
    get: function get() {
      return {
        '8va': {
          code: 'v8',
          sizes: {
            'default': {
              point: 20,
              attachments: {
                'treble': {
                  line: -1.2,
                  x_shift: 11
                }
              }
            },
            'small': {
              point: 18,
              attachments: {
                'treble': {
                  line: -0.4,
                  x_shift: 8
                }
              }
            }
          }
        },
        '8vb': {
          code: 'v8',
          sizes: {
            'default': {
              point: 20,
              attachments: {
                'treble': {
                  line: 6.3,
                  x_shift: 10
                },
                'bass': {
                  line: 4,
                  x_shift: 1
                }
              }
            },
            'small': {
              point: 18,
              attachments: {
                'treble': {
                  line: 5.8,
                  x_shift: 6
                },
                'bass': {
                  line: 3.5,
                  x_shift: 0.5
                }
              }
            }
          }
        }
      };
    }

    // Create a new clef. The parameter `clef` must be a key from
    // `Clef.types`.

  }]);

  function Clef(type, size, annotation) {
    _classCallCheck(this, Clef);

    var _this = _possibleConstructorReturn(this, (Clef.__proto__ || Object.getPrototypeOf(Clef)).call(this));

    _this.setAttribute('type', 'Clef');

    _this.setPosition(_stavemodifier.StaveModifier.Position.BEGIN);
    _this.setType(type, size, annotation);
    _this.setWidth(_this.glyph.getMetrics().width);
    L('Creating clef:', type);
    return _this;
  }

  _createClass(Clef, [{
    key: 'getCategory',
    value: function getCategory() {
      return Clef.CATEGORY;
    }
  }, {
    key: 'setType',
    value: function setType(type, size, annotation) {
      this.type = type;
      this.clef = Clef.types[type];
      if (size === undefined) {
        this.size = 'default';
      } else {
        this.size = size;
      }
      this.clef.point = Clef.sizes[this.size];
      this.glyph = new _glyph.Glyph(this.clef.code, this.clef.point);

      // If an annotation, such as 8va, is specified, add it to the Clef object.
      if (annotation !== undefined) {
        var anno_dict = Clef.annotations[annotation];
        this.annotation = {
          code: anno_dict.code,
          point: anno_dict.sizes[this.size].point,
          line: anno_dict.sizes[this.size].attachments[this.type].line,
          x_shift: anno_dict.sizes[this.size].attachments[this.type].x_shift
        };

        this.attachment = new _glyph.Glyph(this.annotation.code, this.annotation.point);
        this.attachment.metrics.x_max = 0;
        this.attachment.setXShift(this.annotation.x_shift);
      } else {
        this.annotation = undefined;
      }

      return this;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      if (this.type === 'tab' && !this.stave) {
        throw new _vex.Vex.RERR('ClefError', "Can't get width without stave.");
      }

      return this.width;
    }
  }, {
    key: 'setStave',
    value: function setStave(stave) {
      this.stave = stave;

      if (this.type !== 'tab') return this;

      var glyphScale = void 0;
      var glyphOffset = void 0;
      var numLines = this.stave.getOptions().num_lines;
      switch (numLines) {
        case 8:
          glyphScale = 55;
          glyphOffset = 14;
          break;
        case 7:
          glyphScale = 47;
          glyphOffset = 8;
          break;
        case 6:
          glyphScale = 40;
          glyphOffset = 1;
          break;
        case 5:
          glyphScale = 30;
          glyphOffset = -6;
          break;
        case 4:
          glyphScale = 23;
          glyphOffset = -12;
          break;
        default:
          throw new _vex.Vex.RERR('ClefError', 'Invalid number of lines: ' + numLines);
      }

      this.glyph.setPoint(glyphScale);
      this.glyph.setYShift(glyphOffset);

      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      if (!this.x) throw new _vex.Vex.RERR('ClefError', "Can't draw clef without x.");
      if (!this.stave) throw new _vex.Vex.RERR('ClefError', "Can't draw clef without stave.");
      this.setRendered();

      this.glyph.setStave(this.stave);
      this.glyph.setContext(this.stave.context);
      if (this.clef.line !== undefined) {
        this.placeGlyphOnLine(this.glyph, this.stave, this.clef.line);
      }

      this.glyph.renderToStave(this.x);

      if (this.annotation !== undefined) {
        this.placeGlyphOnLine(this.attachment, this.stave, this.annotation.line);
        this.attachment.setStave(this.stave);
        this.attachment.setContext(this.stave.context);
        this.attachment.renderToStave(this.x);
      }
    }
  }]);

  return Clef;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 37 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TimeSignature = undefined;

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _glyph3 = __webpack_require__(2);

var _stavemodifier = __webpack_require__(7);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// Implements time signatures glyphs for staffs
// See tables.js for the internal time signatures
// representation

var assertIsValidFraction = function assertIsValidFraction(timeSpec) {
  var numbers = timeSpec.split('/').filter(function (number) {
    return number !== '';
  });

  if (numbers.length !== 2) {
    throw new _vex.Vex.RERR('BadTimeSignature', 'Invalid time spec: ' + timeSpec + '. Must be in the form "<numerator>/<denominator>"');
  }

  numbers.forEach(function (number) {
    if (isNaN(Number(number))) {
      throw new _vex.Vex.RERR('BadTimeSignature', 'Invalid time spec: ' + timeSpec + '. Must contain two valid numbers.');
    }
  });
};

var TimeSignature = exports.TimeSignature = function (_StaveModifier) {
  _inherits(TimeSignature, _StaveModifier);

  _createClass(TimeSignature, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'timesignatures';
    }
  }, {
    key: 'glyphs',
    get: function get() {
      return {
        'C': {
          code: 'v41',
          point: 40,
          line: 2
        },
        'C|': {
          code: 'vb6',
          point: 40,
          line: 2
        }
      };
    }
  }]);

  function TimeSignature() {
    var timeSpec = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
    var customPadding = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 15;

    _classCallCheck(this, TimeSignature);

    var _this = _possibleConstructorReturn(this, (TimeSignature.__proto__ || Object.getPrototypeOf(TimeSignature)).call(this));

    _this.setAttribute('type', 'TimeSignature');

    if (timeSpec === null) return _possibleConstructorReturn(_this);

    var padding = customPadding;

    _this.point = 40;
    _this.topLine = 2;
    _this.bottomLine = 4;
    _this.setPosition(_stavemodifier.StaveModifier.Position.BEGIN);
    _this.setTimeSig(timeSpec);
    _this.setWidth(_this.timeSig.glyph.getMetrics().width);
    _this.setPadding(padding);
    return _this;
  }

  _createClass(TimeSignature, [{
    key: 'getCategory',
    value: function getCategory() {
      return TimeSignature.CATEGORY;
    }
  }, {
    key: 'parseTimeSpec',
    value: function parseTimeSpec(timeSpec) {
      if (timeSpec === 'C' || timeSpec === 'C|') {
        var _TimeSignature$glyphs = TimeSignature.glyphs[timeSpec],
            line = _TimeSignature$glyphs.line,
            code = _TimeSignature$glyphs.code,
            point = _TimeSignature$glyphs.point;

        return {
          line: line,
          num: false,
          glyph: new _glyph3.Glyph(code, point)
        };
      }

      assertIsValidFraction(timeSpec);

      var _timeSpec$split$map = timeSpec.split('/').map(function (number) {
        return number.split('');
      }),
          _timeSpec$split$map2 = _slicedToArray(_timeSpec$split$map, 2),
          topDigits = _timeSpec$split$map2[0],
          botDigits = _timeSpec$split$map2[1];

      return {
        num: true,
        glyph: this.makeTimeSignatureGlyph(topDigits, botDigits)
      };
    }
  }, {
    key: 'makeTimeSignatureGlyph',
    value: function makeTimeSignatureGlyph(topDigits, botDigits) {
      var glyph = new _glyph3.Glyph('v0', this.point);
      glyph.topGlyphs = [];
      glyph.botGlyphs = [];

      var topWidth = 0;
      for (var i = 0; i < topDigits.length; ++i) {
        var num = topDigits[i];
        var topGlyph = new _glyph3.Glyph('v' + num, this.point);

        glyph.topGlyphs.push(topGlyph);
        topWidth += topGlyph.getMetrics().width;
      }

      var botWidth = 0;
      for (var _i = 0; _i < botDigits.length; ++_i) {
        var _num = botDigits[_i];
        var botGlyph = new _glyph3.Glyph('v' + _num, this.point);

        glyph.botGlyphs.push(botGlyph);
        botWidth += botGlyph.getMetrics().width;
      }

      var width = topWidth > botWidth ? topWidth : botWidth;
      var xMin = glyph.getMetrics().x_min;

      glyph.getMetrics = function () {
        return {
          x_min: xMin,
          x_max: xMin + width,
          width: width
        };
      };

      var topStartX = (width - topWidth) / 2.0;
      var botStartX = (width - botWidth) / 2.0;

      var that = this;
      glyph.renderToStave = function renderToStave(x) {
        var start_x = x + topStartX;
        for (var _i2 = 0; _i2 < this.topGlyphs.length; ++_i2) {
          var _glyph = this.topGlyphs[_i2];
          _glyph3.Glyph.renderOutline(this.context, _glyph.metrics.outline, _glyph.scale, start_x + _glyph.x_shift, this.stave.getYForLine(that.topLine) + 1);
          start_x += _glyph.getMetrics().width;
        }

        start_x = x + botStartX;
        for (var _i3 = 0; _i3 < this.botGlyphs.length; ++_i3) {
          var _glyph2 = this.botGlyphs[_i3];
          that.placeGlyphOnLine(_glyph2, this.stave, _glyph2.line);
          _glyph3.Glyph.renderOutline(this.context, _glyph2.metrics.outline, _glyph2.scale, start_x + _glyph2.x_shift, this.stave.getYForLine(that.bottomLine) + 1);
          start_x += _glyph2.getMetrics().width;
        }
      };

      return glyph;
    }
  }, {
    key: 'getTimeSig',
    value: function getTimeSig() {
      return this.timeSig;
    }
  }, {
    key: 'setTimeSig',
    value: function setTimeSig(timeSpec) {
      this.timeSig = this.parseTimeSpec(timeSpec);
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      if (!this.x) {
        throw new _vex.Vex.RERR('TimeSignatureError', "Can't draw time signature without x.");
      }

      if (!this.stave) {
        throw new _vex.Vex.RERR('TimeSignatureError', "Can't draw time signature without stave.");
      }

      this.setRendered();
      this.timeSig.glyph.setStave(this.stave);
      this.timeSig.glyph.setContext(this.stave.context);
      this.placeGlyphOnLine(this.timeSig.glyph, this.stave, this.timeSig.line);
      this.timeSig.glyph.renderToStave(this.x);
    }
  }]);

  return TimeSignature;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 38 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TabNote = undefined;

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

var _stem = __webpack_require__(9);

var _stemmablenote = __webpack_require__(20);

var _dot = __webpack_require__(21);

var _glyph2 = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// The file implements notes for Tablature notation. This consists of one or
// more fret positions, and can either be drawn with or without stems.
//
// See `tests/tabnote_tests.js` for usage examples

// Gets the unused strings grouped together if consecutive.
//
// Parameters:
// * num_lines - The number of lines
// * strings_used - An array of numbers representing which strings have fret positions
function getUnusedStringGroups(num_lines, strings_used) {
  var stem_through = [];
  var group = [];
  for (var string = 1; string <= num_lines; string++) {
    var is_used = strings_used.indexOf(string) > -1;

    if (!is_used) {
      group.push(string);
    } else {
      stem_through.push(group);
      group = [];
    }
  }
  if (group.length > 0) stem_through.push(group);

  return stem_through;
}

// Gets groups of points that outline the partial stem lines
// between fret positions
//
// Parameters:
// * stem_Y - The `y` coordinate the stem is located on
// * unused_strings - An array of groups of unused strings
// * stave - The stave to use for reference
// * stem_direction - The direction of the stem
function getPartialStemLines(stem_y, unused_strings, stave, stem_direction) {
  var up_stem = stem_direction !== 1;
  var down_stem = stem_direction !== -1;

  var line_spacing = stave.getSpacingBetweenLines();
  var total_lines = stave.getNumLines();

  var stem_lines = [];

  unused_strings.forEach(function (strings) {
    var containsLastString = strings.indexOf(total_lines) > -1;
    var containsFirstString = strings.indexOf(1) > -1;

    if (up_stem && containsFirstString || down_stem && containsLastString) {
      return;
    }

    // If there's only one string in the group, push a duplicate value.
    // We do this because we need 2 strings to convert into upper/lower y
    // values.
    if (strings.length === 1) {
      strings.push(strings[0]);
    }

    var line_ys = [];
    // Iterate through each group string and store it's y position
    strings.forEach(function (string, index, strings) {
      var isTopBound = string === 1;
      var isBottomBound = string === total_lines;

      // Get the y value for the appropriate staff line,
      // we adjust for a 0 index array, since string numbers are index 1
      var y = stave.getYForLine(string - 1);

      // Unless the string is the first or last, add padding to each side
      // of the line
      if (index === 0 && !isTopBound) {
        y -= line_spacing / 2 - 1;
      } else if (index === strings.length - 1 && !isBottomBound) {
        y += line_spacing / 2 - 1;
      }

      // Store the y value
      line_ys.push(y);

      // Store a subsequent y value connecting this group to the main
      // stem above/below the stave if it's the top/bottom string
      if (stem_direction === 1 && isTopBound) {
        line_ys.push(stem_y - 2);
      } else if (stem_direction === -1 && isBottomBound) {
        line_ys.push(stem_y + 2);
      }
    });

    // Add the sorted y values to the
    stem_lines.push(line_ys.sort(function (a, b) {
      return a - b;
    }));
  });

  return stem_lines;
}

var TabNote = exports.TabNote = function (_StemmableNote) {
  _inherits(TabNote, _StemmableNote);

  _createClass(TabNote, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'tabnotes';
    }

    // Initialize the TabNote with a `tab_struct` full of properties
    // and whether to `draw_stem` when rendering the note

  }]);

  function TabNote(tab_struct, draw_stem) {
    _classCallCheck(this, TabNote);

    var _this = _possibleConstructorReturn(this, (TabNote.__proto__ || Object.getPrototypeOf(TabNote)).call(this, tab_struct));

    _this.setAttribute('type', 'TabNote');

    _this.ghost = false; // Renders parenthesis around notes
    // Note properties
    //
    // The fret positions in the note. An array of `{ str: X, fret: X }`
    _this.positions = tab_struct.positions;

    // Render Options
    _vex.Vex.Merge(_this.render_options, {
      // font size for note heads and rests
      glyph_font_scale: _tables.Flow.DEFAULT_TABLATURE_FONT_SCALE,
      // Flag to draw a stem
      draw_stem: draw_stem,
      // Flag to draw dot modifiers
      draw_dots: draw_stem,
      // Flag to extend the main stem through the stave and fret positions
      draw_stem_through_stave: false,
      // vertical shift from stave line
      y_shift: 0,
      // normal glyph scale
      scale: 1.0,
      // default tablature font
      font: '10pt Arial'
    });

    _this.glyph = _tables.Flow.durationToGlyph(_this.duration, _this.noteType);

    if (!_this.glyph) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Invalid note initialization data (No glyph found): ' + JSON.stringify(tab_struct));
    }

    _this.buildStem();

    if (tab_struct.stem_direction) {
      _this.setStemDirection(tab_struct.stem_direction);
    } else {
      _this.setStemDirection(_stem.Stem.UP);
    }

    // Renders parenthesis around notes
    _this.ghost = false;
    _this.updateWidth();
    return _this;
  }

  _createClass(TabNote, [{
    key: 'reset',
    value: function reset() {
      if (this.stave) this.setStave(this.stave);
    }

    // The ModifierContext category

  }, {
    key: 'getCategory',
    value: function getCategory() {
      return TabNote.CATEGORY;
    }

    // Set as ghost `TabNote`, surrounds the fret positions with parenthesis.
    // Often used for indicating frets that are being bent to

  }, {
    key: 'setGhost',
    value: function setGhost(ghost) {
      this.ghost = ghost;
      this.updateWidth();
      return this;
    }

    // Determine if the note has a stem

  }, {
    key: 'hasStem',
    value: function hasStem() {
      return this.render_options.draw_stem;
    }

    // Get the default stem extension for the note

  }, {
    key: 'getStemExtension',
    value: function getStemExtension() {
      var glyph = this.getGlyph();

      if (this.stem_extension_override != null) {
        return this.stem_extension_override;
      }

      if (glyph) {
        return this.getStemDirection() === 1 ? glyph.tabnote_stem_up_extension : glyph.tabnote_stem_down_extension;
      }

      return 0;
    }

    // Add a dot to the note

  }, {
    key: 'addDot',
    value: function addDot() {
      var dot = new _dot.Dot();
      this.dots += 1;
      return this.addModifier(dot, 0);
    }

    // Calculate and store the width of the note

  }, {
    key: 'updateWidth',
    value: function updateWidth() {
      var _this2 = this;

      this.glyphs = [];
      this.width = 0;
      for (var i = 0; i < this.positions.length; ++i) {
        var fret = this.positions[i].fret;
        if (this.ghost) fret = '(' + fret + ')';
        var glyph = _tables.Flow.tabToGlyph(fret, this.render_options.scale);
        this.glyphs.push(glyph);
        this.width = Math.max(glyph.getWidth(), this.width);
      }
      // For some reason we associate a notehead glyph with a TabNote, and this
      // glyph is used for certain width calculations. Of course, this is totally
      // incorrect since a notehead is a poor approximation for the dimensions of
      // a fret number which can have multiple digits. As a result, we must
      // overwrite getWidth() to return the correct width
      this.glyph.getWidth = function () {
        return _this2.width;
      };
    }

    // Set the `stave` to the note

  }, {
    key: 'setStave',
    value: function setStave(stave) {
      var _this3 = this;

      _get(TabNote.prototype.__proto__ || Object.getPrototypeOf(TabNote.prototype), 'setStave', this).call(this, stave);
      this.context = stave.context;

      // Calculate the fret number width based on font used
      var i = void 0;
      if (this.context) {
        var ctx = this.context;
        this.width = 0;

        var _loop = function _loop() {
          var glyph = _this3.glyphs[i];
          var text = '' + glyph.text;
          if (text.toUpperCase() !== 'X') {
            ctx.save();
            ctx.setRawFont(_this3.render_options.font);
            glyph.width = ctx.measureText(text).width;
            ctx.restore();
            glyph.getWidth = function () {
              return glyph.width;
            };
          }
          _this3.width = Math.max(glyph.getWidth(), _this3.width);
        };

        for (i = 0; i < this.glyphs.length; ++i) {
          _loop();
        }
        this.glyph.getWidth = function () {
          return _this3.width;
        };
      }

      // we subtract 1 from `line` because getYForLine expects a 0-based index,
      // while the position.str is a 1-based index
      var ys = this.positions.map(function (_ref) {
        var line = _ref.str;
        return stave.getYForLine(line - 1);
      });

      this.setYs(ys);

      if (this.stem) {
        this.stem.setYBounds(this.getStemY(), this.getStemY());
      }

      return this;
    }

    // Get the fret positions for the note

  }, {
    key: 'getPositions',
    value: function getPositions() {
      return this.positions;
    }

    // Add self to the provided modifier context `mc`

  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext(mc) {
      this.setModifierContext(mc);
      for (var i = 0; i < this.modifiers.length; ++i) {
        this.modifierContext.addModifier(this.modifiers[i]);
      }
      this.modifierContext.addModifier(this);
      this.preFormatted = false;
      return this;
    }

    // Get the `x` coordinate to the right of the note

  }, {
    key: 'getTieRightX',
    value: function getTieRightX() {
      var tieStartX = this.getAbsoluteX();
      var note_glyph_width = this.glyph.getWidth();
      tieStartX += note_glyph_width / 2;
      tieStartX += -this.width / 2 + this.width + 2;

      return tieStartX;
    }

    // Get the `x` coordinate to the left of the note

  }, {
    key: 'getTieLeftX',
    value: function getTieLeftX() {
      var tieEndX = this.getAbsoluteX();
      var note_glyph_width = this.glyph.getWidth();
      tieEndX += note_glyph_width / 2;
      tieEndX -= this.width / 2 + 2;

      return tieEndX;
    }

    // Get the default `x` and `y` coordinates for a modifier at a specific
    // `position` at a fret position `index`

  }, {
    key: 'getModifierStartXY',
    value: function getModifierStartXY(position, index) {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call GetModifierStartXY on an unformatted note");
      }

      if (this.ys.length === 0) {
        throw new _vex.Vex.RERR('NoYValues', 'No Y-Values calculated for this note.');
      }

      var x = 0;
      if (position === _modifier.Modifier.Position.LEFT) {
        x = -1 * 2; // extra_left_px
      } else if (position === _modifier.Modifier.Position.RIGHT) {
        x = this.width + 2; // extra_right_px
      } else if (position === _modifier.Modifier.Position.BELOW || position === _modifier.Modifier.Position.ABOVE) {
        var note_glyph_width = this.glyph.getWidth();
        x = note_glyph_width / 2;
      }

      return {
        x: this.getAbsoluteX() + x,
        y: this.ys[index]
      };
    }

    // Get the default line for rest

  }, {
    key: 'getLineForRest',
    value: function getLineForRest() {
      return this.positions[0].str;
    }

    // Pre-render formatting

  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return;
      if (this.modifierContext) this.modifierContext.preFormat();
      // width is already set during init()
      this.setPreFormatted(true);
    }

    // Get the x position for the stem

  }, {
    key: 'getStemX',
    value: function getStemX() {
      return this.getCenterGlyphX();
    }

    // Get the y position for the stem

  }, {
    key: 'getStemY',
    value: function getStemY() {
      var num_lines = this.stave.getNumLines();

      // The decimal staff line amounts provide optimal spacing between the
      // fret number and the stem
      var stemUpLine = -0.5;
      var stemDownLine = num_lines - 0.5;
      var stemStartLine = _stem.Stem.UP === this.stem_direction ? stemUpLine : stemDownLine;

      return this.stave.getYForLine(stemStartLine);
    }

    // Get the stem extents for the tabnote

  }, {
    key: 'getStemExtents',
    value: function getStemExtents() {
      return this.stem.getExtents();
    }

    // Draw the fal onto the context

  }, {
    key: 'drawFlag',
    value: function drawFlag() {
      var beam = this.beam,
          glyph = this.glyph,
          context = this.context,
          stem = this.stem,
          stem_direction = this.stem_direction,
          _render_options = this.render_options,
          draw_stem = _render_options.draw_stem,
          glyph_font_scale = _render_options.glyph_font_scale;


      var shouldDrawFlag = beam == null && draw_stem;

      // Now it's the flag's turn.
      if (glyph.flag && shouldDrawFlag) {
        var flag_x = this.getStemX() + 1;
        var flag_y = this.getStemY() - stem.getHeight();

        var flag_code = stem_direction === _stem.Stem.DOWN ? glyph.code_flag_downstem // Down stems have flags on the left.
        : glyph.code_flag_upstem;

        // Draw the Flag
        _glyph2.Glyph.renderGlyph(context, flag_x, flag_y, glyph_font_scale, flag_code);
      }
    }

    // Render the modifiers onto the context

  }, {
    key: 'drawModifiers',
    value: function drawModifiers() {
      var _this4 = this;

      // Draw the modifiers
      this.modifiers.forEach(function (modifier) {
        // Only draw the dots if enabled
        if (modifier.getCategory() === 'dots' && !_this4.render_options.draw_dots) return;

        modifier.setContext(_this4.context);
        modifier.draw();
      });
    }

    // Render the stem extension through the fret positions

  }, {
    key: 'drawStemThrough',
    value: function drawStemThrough() {
      var stem_x = this.getStemX();
      var stem_y = this.getStemY();
      var ctx = this.context;

      var stem_through = this.render_options.draw_stem_through_stave;
      var draw_stem = this.render_options.draw_stem;
      if (draw_stem && stem_through) {
        var total_lines = this.stave.getNumLines();
        var strings_used = this.positions.map(function (position) {
          return position.str;
        });

        var unused_strings = getUnusedStringGroups(total_lines, strings_used);
        var stem_lines = getPartialStemLines(stem_y, unused_strings, this.getStave(), this.getStemDirection());

        ctx.save();
        ctx.setLineWidth(_stem.Stem.WIDTH);
        stem_lines.forEach(function (bounds) {
          if (bounds.length === 0) return;

          ctx.beginPath();
          ctx.moveTo(stem_x, bounds[0]);
          ctx.lineTo(stem_x, bounds[bounds.length - 1]);
          ctx.stroke();
          ctx.closePath();
        });
        ctx.restore();
      }
    }

    // Render the fret positions onto the context

  }, {
    key: 'drawPositions',
    value: function drawPositions() {
      var ctx = this.context;
      var x = this.getAbsoluteX();
      var ys = this.ys;
      for (var i = 0; i < this.positions.length; ++i) {
        var y = ys[i] + this.render_options.y_shift;
        var _glyph = this.glyphs[i];

        // Center the fret text beneath the notation note head
        var note_glyph_width = this.glyph.getWidth();
        var tab_x = x + note_glyph_width / 2 - _glyph.getWidth() / 2;

        // FIXME: Magic numbers.
        ctx.clearRect(tab_x - 2, y - 3, _glyph.getWidth() + 4, 6);

        if (_glyph.code) {
          _glyph2.Glyph.renderGlyph(ctx, tab_x, y, this.render_options.glyph_font_scale * this.render_options.scale, _glyph.code);
        } else {
          ctx.save();
          ctx.setRawFont(this.render_options.font);
          var _text = _glyph.text.toString();
          ctx.fillText(_text, tab_x, y + 5 * this.render_options.scale);
          ctx.restore();
        }
      }
    }

    // The main rendering function for the entire note

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();

      if (!this.stave) {
        throw new _vex.Vex.RERR('NoStave', "Can't draw without a stave.");
      }

      if (this.ys.length === 0) {
        throw new _vex.Vex.RERR('NoYValues', "Can't draw note without Y values.");
      }

      this.setRendered();
      var render_stem = this.beam == null && this.render_options.draw_stem;

      this.drawPositions();
      this.drawStemThrough();

      var stem_x = this.getStemX();

      this.stem.setNoteHeadXBounds(stem_x, stem_x);

      if (render_stem) {
        this.context.openGroup('stem', null, { pointerBBox: true });
        this.stem.setContext(this.context).draw();
        this.context.closeGroup();
      }

      this.drawFlag();
      this.drawModifiers();
    }
  }]);

  return TabNote;
}(_stemmablenote.StemmableNote);

/***/ }),
/* 39 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Registry = exports.X = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// @author Mohit Cheppudira
//
// ## Description
//
// This file implements a registry for VexFlow elements. It allows users
// to track, query, and manage some subset of generated elements, and
// dynamically get and set attributes.
//
// There are two ways to regiser with a registry:
//
// 1) Explicitly call `element.register(registry)`, or,
// 2) Call `Registry.enableDefaultRegistry(registry)` when ready, and all future
//    elements will automatically register with it.
//
// Once an element is registered, selected attributes are tracked and indexed by
// the registry. This allows fast look up of elements by attributes like id, type,
// and class.

var _vex = __webpack_require__(0);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var X = exports.X = _vex.Vex.MakeException('RegistryError');

function setIndexValue(index, name, value, id, elem) {
  if (!index[name][value]) index[name][value] = {};
  index[name][value][id] = elem;
}

var Registry = exports.Registry = function () {
  _createClass(Registry, null, [{
    key: 'INDEXES',
    get: function get() {
      return ['type'];
    }
  }]);

  function Registry() {
    _classCallCheck(this, Registry);

    this.clear();
  }

  // If you call `enableDefaultRegistry`, any new elements will auto-register with
  // the provided registry as soon as they're constructed.


  _createClass(Registry, [{
    key: 'clear',
    value: function clear() {
      // Indexes are represented as maps of maps (of maps). This allows
      // for both multi-labeling (e.g., an element can have multiple classes)
      // and efficient lookup.
      this.index = {
        id: {},
        type: {},
        class: {}
      };
      return this;
    }

    // Updates the indexes for element 'id'. If an element's attribute changes
    // from A -> B, make sure to remove the element from A.

  }, {
    key: 'updateIndex',
    value: function updateIndex(_ref) {
      var id = _ref.id,
          name = _ref.name,
          value = _ref.value,
          oldValue = _ref.oldValue;

      var elem = this.getElementById(id);
      if (oldValue !== null && this.index[name][oldValue]) {
        delete this.index[name][oldValue][id];
      }
      if (value !== null) {
        setIndexValue(this.index, name, value, elem.getAttribute('id'), elem);
      }
    }

    // Register element `elem` with this registry. This adds the element to its index and watches
    // it for attribute changes.

  }, {
    key: 'register',
    value: function register(elem, id) {
      var _this = this;

      id = id || elem.getAttribute('id');

      if (!id) {
        throw new X('Can\'t add element without `id` attribute to registry', elem);
      }

      // Manually add id to index, then update other indexes.
      elem.setAttribute('id', id);
      setIndexValue(this.index, 'id', id, id, elem);
      Registry.INDEXES.forEach(function (name) {
        _this.updateIndex({ id: id, name: name, value: elem.getAttribute(name), oldValue: null });
      });
      elem.onRegister(this);
      return this;
    }
  }, {
    key: 'getElementById',
    value: function getElementById(id) {
      return this.index.id[id] ? this.index.id[id][id] : null;
    }
  }, {
    key: 'getElementsByAttribute',
    value: function getElementsByAttribute(attrName, value) {
      var index = this.index[attrName];
      if (index && index[value]) {
        return Object.keys(index[value]).map(function (i) {
          return index[value][i];
        });
      } else {
        return [];
      }
    }
  }, {
    key: 'getElementsByType',
    value: function getElementsByType(type) {
      return this.getElementsByAttribute('type', type);
    }
  }, {
    key: 'getElementsByClass',
    value: function getElementsByClass(className) {
      return this.getElementsByAttribute('class', className);
    }

    // This is called by the element when an attribute value changes. If an indexed
    // attribute changes, then update the local index.

  }, {
    key: 'onUpdate',
    value: function onUpdate(_ref2) {
      var id = _ref2.id,
          name = _ref2.name,
          value = _ref2.value,
          oldValue = _ref2.oldValue;

      function includes(array, value) {
        return array.filter(function (x) {
          return x === value;
        }).length > 0;
      }

      if (!includes(Registry.INDEXES.concat(['id', 'class']), name)) return this;
      this.updateIndex({ id: id, name: name, value: value, oldValue: oldValue });
      return this;
    }
  }], [{
    key: 'enableDefaultRegistry',
    value: function enableDefaultRegistry(registry) {
      Registry.defaultRegistry = registry;
    }
  }, {
    key: 'getDefaultRegistry',
    value: function getDefaultRegistry() {
      return Registry.defaultRegistry;
    }
  }, {
    key: 'disableDefaultRegistry',
    value: function disableDefaultRegistry() {
      Registry.defaultRegistry = null;
    }
  }]);

  return Registry;
}();

Registry.defaultRegistry = null;

/***/ }),
/* 40 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
var Font = exports.Font = {
  "glyphs": {
    "v0": { "x_min": 0, "x_max": 514.5, "ha": 525, "o": "m 236 648 b 246 648 238 648 242 648 b 288 646 261 648 283 648 b 472 513 364 634 428 587 b 514 347 502 464 514 413 b 462 163 514 272 499 217 b 257 44 409 83 333 44 b 50 163 181 44 103 83 b 0 347 14 217 0 272 b 40 513 0 413 12 464 b 236 648 87 591 155 638 m 277 614 b 253 616 273 616 261 616 b 242 616 247 616 243 616 b 170 499 193 609 181 589 b 159 348 163 446 159 398 b 166 222 159 308 161 266 b 201 91 174 138 183 106 b 257 76 215 81 235 76 b 311 91 277 76 299 81 b 347 222 330 106 338 138 b 353 348 352 266 353 308 b 344 499 353 398 351 446 b 277 614 333 587 322 606 m 257 -1 l 258 -1 l 255 -1 l 257 -1 m 257 673 l 258 673 l 255 673 l 257 673 " },
    "v1": { "x_min": -1.359375, "x_max": 344.359375, "ha": 351, "o": "m 126 637 l 129 638 l 198 638 l 266 638 l 269 635 b 274 631 272 634 273 632 l 277 627 l 277 395 b 279 156 277 230 277 161 b 329 88 281 123 295 106 b 344 69 341 81 344 79 b 337 55 344 62 343 59 l 333 54 l 197 54 l 61 54 l 58 55 b 50 69 53 59 50 62 b 65 88 50 79 53 81 b 80 97 72 91 74 93 b 117 156 103 113 112 129 b 117 345 117 161 117 222 l 117 528 l 100 503 l 38 406 b 14 383 24 384 23 383 b -1 398 5 383 -1 390 b 4 415 -1 403 1 409 b 16 437 5 416 10 426 l 72 539 l 100 596 b 121 632 119 631 119 631 b 126 637 122 634 125 635 m 171 -1 l 172 -1 l 170 -1 l 171 -1 m 171 673 l 172 673 l 170 673 l 171 673 " },
    "v2": { "x_min": -1.359375, "x_max": 458.6875, "ha": 468, "o": "m 197 648 b 216 648 201 648 208 648 b 258 646 232 648 253 648 b 419 546 333 637 393 599 b 432 489 428 528 432 509 b 356 342 432 440 405 384 b 235 278 322 313 288 295 b 69 170 166 256 107 217 b 69 169 69 170 69 169 b 69 169 69 169 69 169 b 74 173 69 169 72 170 b 209 222 112 204 163 222 b 310 195 247 222 274 215 b 371 179 332 184 352 179 b 396 181 379 179 387 179 b 428 202 409 184 423 194 b 442 212 431 209 436 212 b 458 197 450 212 458 206 b 441 148 458 190 449 165 b 299 44 409 84 353 44 b 288 45 295 44 292 44 b 250 61 274 45 268 49 b 122 99 212 86 164 99 b 73 91 104 99 88 97 b 28 63 53 84 34 72 b 14 54 25 56 20 54 b 1 62 9 54 4 56 l -1 65 l -1 79 b 0 99 -1 91 0 95 b 2 113 1 102 2 108 b 164 309 20 197 81 272 b 285 470 232 341 277 398 b 287 487 287 476 287 481 b 171 595 287 551 239 595 b 155 595 166 595 160 595 b 142 592 145 594 142 594 b 145 589 142 592 142 591 b 179 527 168 576 179 551 b 132 455 179 496 163 467 b 104 451 122 452 112 451 b 27 530 62 451 27 487 b 29 555 27 538 27 546 b 197 648 44 601 115 639 m 228 -1 l 230 -1 l 227 -1 l 228 -1 m 228 673 l 230 673 l 227 673 l 228 673 " },
    "v3": { "x_min": -1.359375, "x_max": 409.6875, "ha": 418, "o": "m 174 648 b 191 648 176 648 183 648 b 225 648 204 648 220 648 b 402 523 317 638 389 588 b 404 503 404 517 404 510 b 402 484 404 495 404 488 b 264 373 389 437 334 394 b 257 370 259 371 257 371 b 257 370 257 370 257 370 b 264 369 258 370 261 369 b 409 202 359 334 409 267 b 318 72 409 152 381 104 b 200 43 281 52 240 43 b 23 113 134 43 69 68 b 0 169 6 129 0 149 b 77 249 0 210 29 249 l 77 249 b 152 174 125 249 152 212 b 103 102 152 145 137 116 b 103 102 103 102 103 102 b 147 94 103 101 132 95 b 153 94 149 94 151 94 b 265 206 219 94 265 141 b 264 226 265 213 265 219 b 147 355 253 299 204 353 b 126 371 133 356 126 362 b 147 388 126 383 132 388 b 254 474 196 391 238 424 b 259 502 258 484 259 494 b 182 592 259 544 228 582 b 156 595 175 595 166 595 b 115 592 142 595 129 594 l 111 591 l 115 588 b 152 524 141 574 152 549 b 92 449 152 491 130 458 b 76 448 87 448 81 448 b -1 530 32 448 -1 488 b 20 581 -1 548 5 566 b 174 648 55 619 108 641 m 204 -1 l 205 -1 l 202 -1 l 204 -1 m 204 673 l 205 673 l 202 673 l 204 673 " },
    "v4": { "x_min": 0, "x_max": 468.21875, "ha": 478, "o": "m 174 637 b 232 638 175 638 189 638 b 277 638 245 638 259 638 l 378 638 l 381 635 b 389 623 386 632 389 627 b 382 609 389 617 386 613 b 366 589 381 606 372 598 l 313 528 l 245 451 l 209 410 l 155 348 l 84 267 b 59 240 72 252 59 240 b 59 240 59 240 59 240 b 151 238 59 238 68 238 l 242 238 l 242 303 b 243 371 242 369 242 370 b 289 426 245 374 254 385 l 303 441 l 317 456 l 338 483 l 360 506 l 371 520 b 386 527 375 526 381 527 b 400 519 392 527 397 524 b 401 440 401 516 401 514 b 401 377 401 423 401 402 l 401 238 l 426 238 b 453 237 449 238 450 238 b 465 217 461 234 465 226 b 460 202 465 212 464 206 b 426 197 454 197 453 197 l 401 197 l 401 180 b 451 88 402 129 412 109 b 468 69 465 81 468 79 b 461 55 468 62 466 59 l 458 54 l 321 54 l 185 54 l 182 55 b 175 69 176 59 175 62 b 191 88 175 79 176 81 b 240 180 230 109 240 129 l 240 197 l 125 197 b 73 195 104 195 87 195 b 8 197 10 195 9 197 b 0 212 2 199 0 205 b 0 212 0 212 0 212 b 20 242 0 219 0 219 b 163 610 104 344 163 492 b 174 637 163 628 166 634 m 234 -1 l 235 -1 l 232 -1 l 234 -1 m 234 673 l 235 673 l 232 673 l 234 673 " },
    "v5": { "x_min": 0, "x_max": 409.6875, "ha": 418, "o": "m 47 637 b 53 638 49 638 50 638 b 69 634 55 638 61 637 b 210 610 114 619 161 610 b 363 634 259 610 311 619 b 382 638 372 637 378 638 b 392 634 386 638 389 637 b 397 623 396 630 397 627 b 393 610 397 620 396 616 b 298 505 368 552 338 520 b 212 494 277 498 246 494 b 65 517 163 494 106 502 b 61 517 62 517 61 517 b 61 517 61 517 61 517 b 51 408 61 517 51 412 b 51 408 51 408 51 408 b 51 408 51 408 51 408 b 61 412 53 408 55 409 b 125 434 80 421 103 430 b 185 441 145 440 166 441 b 409 244 310 441 409 353 b 401 191 409 227 406 209 b 197 43 375 105 287 43 b 159 47 183 43 171 44 b 23 123 112 56 61 86 b 0 180 6 140 0 159 b 76 260 0 220 31 260 b 92 259 81 260 87 259 b 152 183 132 251 152 216 b 100 112 152 152 134 122 b 95 111 98 112 95 111 b 95 111 95 111 95 111 b 129 98 95 109 119 101 b 148 97 136 97 141 97 b 264 235 206 97 261 158 b 265 248 265 240 265 244 b 210 398 265 312 243 373 b 179 408 201 406 194 408 b 174 408 178 408 176 408 b 53 369 130 408 88 394 b 34 359 39 359 38 359 b 17 374 24 359 17 365 b 39 628 17 384 38 625 b 47 637 40 631 43 635 m 204 -1 l 205 -1 l 202 -1 l 204 -1 m 204 673 l 205 673 l 202 673 l 204 673 " },
    "v6": { "x_min": 0, "x_max": 475.03125, "ha": 485, "o": "m 255 648 b 274 648 259 648 266 648 b 314 646 288 648 307 648 b 450 555 374 637 438 594 b 454 530 453 546 454 538 b 375 451 454 485 416 451 b 328 467 359 451 343 455 b 300 526 310 483 300 503 b 352 598 300 557 319 589 b 356 599 355 598 356 599 b 352 602 356 599 355 601 b 288 616 330 612 308 616 b 210 584 257 616 230 605 b 164 433 189 559 174 508 b 160 374 163 415 160 381 b 160 374 160 374 160 374 b 160 374 160 374 160 374 b 168 377 160 374 164 376 b 258 395 200 390 228 395 b 366 367 294 395 328 387 b 475 223 436 333 475 283 b 472 197 475 215 473 206 b 349 65 462 141 419 95 b 259 43 317 51 288 43 b 167 69 230 43 200 52 b 4 290 80 113 20 195 b 0 349 1 309 0 328 b 20 467 0 391 6 433 b 255 648 58 563 155 637 m 269 363 b 257 363 265 363 261 363 b 210 345 236 363 220 356 b 186 226 196 324 186 272 b 187 198 186 216 186 206 b 213 95 191 151 202 112 b 257 76 221 83 238 76 b 270 77 261 76 266 76 b 321 156 299 81 310 99 b 329 229 326 183 329 206 b 321 301 329 252 326 274 b 269 363 311 342 298 359 m 236 -1 l 238 -1 l 235 -1 l 236 -1 m 236 673 l 238 673 l 235 673 l 236 673 " },
    "v7": { "x_min": 0, "x_max": 442.359375, "ha": 451, "o": "m 147 648 b 166 649 153 649 160 649 b 313 598 217 649 273 630 b 340 587 323 588 328 587 l 341 587 b 412 628 367 587 390 601 b 427 638 416 635 421 638 b 439 632 431 638 435 637 b 442 623 441 630 442 628 b 430 569 442 616 439 603 b 352 369 408 492 377 410 b 300 259 325 324 313 298 b 273 84 283 205 273 140 b 265 55 273 65 272 59 l 261 54 l 181 54 l 99 54 l 96 55 b 91 61 95 56 92 59 l 89 63 l 89 77 b 147 263 89 133 111 202 b 261 401 176 313 212 355 b 378 541 315 449 349 489 l 382 548 l 375 544 b 240 495 333 512 285 495 b 129 535 198 495 160 509 b 84 560 108 552 95 560 b 76 559 81 560 78 560 b 31 487 59 555 43 530 b 14 470 27 473 24 470 b 1 477 8 470 4 471 l 0 480 l 0 553 l 0 627 l 1 630 b 16 638 4 635 9 638 b 23 635 17 638 20 637 b 49 626 36 626 39 626 b 96 638 59 626 80 630 b 104 639 99 638 102 639 b 117 644 107 641 112 642 b 147 648 125 645 137 648 m 220 -1 l 221 -1 l 219 -1 l 220 -1 m 220 673 l 221 673 l 219 673 l 220 673 " },
    "v8": { "x_min": 0, "x_max": 488.640625, "ha": 499, "o": "m 217 648 b 245 649 225 648 235 649 b 453 516 343 649 430 595 b 458 478 455 503 458 491 b 412 370 458 440 441 398 b 411 369 412 369 411 369 b 415 365 411 367 412 367 b 488 231 462 331 488 281 b 472 165 488 208 483 186 b 243 43 434 86 338 43 b 63 104 178 43 112 62 b 0 233 20 140 0 186 b 73 365 0 283 24 331 l 77 369 l 72 374 b 29 476 42 406 29 441 b 217 648 29 557 103 635 m 258 605 b 242 606 253 605 247 606 b 157 552 198 606 157 580 b 160 541 157 548 159 544 b 319 413 176 503 242 452 l 337 403 l 338 406 b 359 476 352 428 359 452 b 258 605 359 537 318 595 m 138 326 b 130 330 134 328 130 330 b 130 330 130 330 130 330 b 107 305 127 330 112 313 b 84 231 91 281 84 256 b 243 86 84 156 151 86 b 249 87 245 86 246 87 b 347 156 303 88 347 120 b 344 172 347 162 345 167 b 156 319 325 227 257 281 b 138 326 151 322 144 324 m 243 -1 l 245 -1 l 242 -1 l 243 -1 m 243 673 l 245 673 l 242 673 l 243 673 " },
    "v9": { "x_min": 0, "x_max": 475.03125, "ha": 485, "o": "m 191 646 b 212 649 198 648 205 649 b 255 644 227 649 243 646 b 458 448 348 616 428 539 b 475 342 469 415 475 378 b 460 244 475 308 469 274 b 193 44 421 124 303 44 b 91 69 157 44 122 51 b 19 161 43 97 19 126 b 21 181 19 167 20 174 b 98 241 32 220 65 241 b 170 186 129 241 160 223 b 172 166 171 179 172 173 b 121 94 172 134 152 102 b 117 93 118 94 117 93 b 121 90 117 93 118 91 b 185 76 142 80 164 76 b 270 119 220 76 251 91 b 308 259 287 145 300 194 b 313 317 310 277 313 310 b 313 317 313 317 313 317 b 313 317 313 317 313 317 b 304 315 313 317 308 316 b 216 295 273 302 245 295 b 145 308 193 295 170 299 b 19 398 88 327 42 360 b 0 469 5 420 0 444 b 24 551 0 496 8 526 b 191 646 54 596 125 637 m 227 614 b 215 616 224 616 220 616 b 202 614 210 616 206 616 b 152 535 174 610 163 592 b 144 463 147 509 144 485 b 152 391 144 440 147 417 b 216 328 163 344 179 328 b 280 391 253 328 269 344 b 288 463 285 417 288 440 b 280 535 288 485 285 509 b 227 614 269 594 258 610 m 236 -1 l 238 -1 l 235 -1 l 236 -1 m 236 673 l 238 673 l 235 673 l 236 673 " },
    "va": { "x_min": -149.71875, "x_max": 148.359375, "ha": 151, "o": "m -8 -1 b -1 0 -5 -1 -4 0 b 16 -11 5 0 13 -4 b 83 -186 17 -12 47 -90 l 148 -358 l 148 -363 b 127 -385 148 -376 138 -385 b 112 -378 122 -385 118 -383 b 54 -226 110 -374 114 -385 b 0 -81 24 -147 0 -81 b -55 -226 -1 -81 -25 -147 b -114 -378 -115 -385 -111 -374 b -129 -385 -119 -383 -123 -385 b -149 -363 -140 -385 -149 -376 l -149 -358 l -84 -186 b -19 -11 -49 -90 -19 -12 b -8 -1 -17 -8 -12 -4 " },
    "vb": { "x_min": 0, "x_max": 428.75, "ha": 438, "o": "m 262 186 b 273 186 266 186 272 186 b 274 186 273 186 274 186 b 285 186 274 186 280 186 b 428 48 375 181 428 122 b 386 -68 428 12 416 -29 b 155 -187 329 -145 236 -187 b 12 -111 92 -187 38 -162 b 0 -51 4 -91 0 -72 b 262 186 0 58 122 179 " },
    "vc": { "x_min": 0, "x_max": 447.8125, "ha": 457, "o": "m 0 86 l 0 173 l 223 173 l 447 173 l 447 86 l 447 0 l 223 0 l 0 0 l 0 86 " },
    "vf": { "x_min": 0, "x_max": 370.21875, "ha": 378, "o": "m 0 0 l 0 277 l 61 277 l 122 277 l 122 0 l 122 -278 l 61 -278 l 0 -278 l 0 0 m 246 -1 l 246 277 l 308 277 l 370 277 l 370 -1 l 370 -278 l 308 -278 l 246 -278 l 246 -1 " },
    "v10": { "x_min": 0, "x_max": 559.421875, "ha": 571, "o": "m 5 127 b 14 127 6 127 9 127 b 51 126 25 127 43 127 b 175 98 93 122 138 112 l 186 94 b 279 51 210 86 255 65 b 285 47 280 51 283 48 b 319 27 291 44 311 31 l 326 22 b 359 0 332 19 352 4 l 367 -6 b 371 -9 368 -6 370 -8 l 379 -15 b 387 -22 383 -18 386 -20 l 398 -30 l 411 -40 l 417 -47 l 427 -55 l 434 -61 b 441 -66 436 -62 439 -65 l 446 -72 l 453 -77 l 462 -87 b 558 -188 490 -113 549 -176 b 559 -195 559 -191 559 -194 b 548 -205 559 -201 555 -205 b 541 -204 547 -205 544 -205 b 534 -198 539 -201 536 -199 l 525 -191 b 481 -162 518 -187 490 -167 b 472 -155 477 -159 472 -156 b 468 -152 470 -155 469 -154 b 461 -149 466 -152 464 -151 b 428 -130 454 -145 441 -137 b 371 -99 413 -122 372 -99 b 363 -95 371 -99 367 -98 b 353 -91 357 -94 353 -91 b 348 -90 353 -91 352 -91 b 332 -81 343 -87 341 -86 b 27 -12 230 -37 127 -13 b 0 -5 4 -11 2 -11 b 0 58 0 -2 0 27 b 0 122 0 88 0 120 b 5 127 1 124 4 126 " },
    "v11": { "x_min": -155.171875, "x_max": 153.8125, "ha": 157, "o": "m -137 353 b -130 353 -136 353 -133 353 b -112 349 -125 353 -119 352 b -100 342 -110 347 -104 344 b 0 317 -69 326 -35 317 b 111 349 38 317 76 328 b 129 353 117 352 123 353 b 153 327 142 353 153 344 b 144 302 153 320 153 317 b 27 6 93 226 50 113 b 21 -13 24 -11 24 -11 b 0 -26 17 -22 8 -26 b -24 -12 -9 -26 -19 -22 b -28 5 -24 -9 -27 -2 b -145 302 -53 117 -95 224 b -155 327 -155 317 -155 320 b -137 353 -155 340 -148 349 " },
    "v18": { "x_min": 0, "x_max": 323.9375, "ha": 331, "o": "m 217 535 b 225 537 220 537 221 537 b 245 524 235 537 242 533 l 246 521 l 247 390 l 247 258 l 273 265 b 306 270 288 269 299 270 b 322 259 315 270 319 267 b 323 208 323 256 323 233 b 322 158 323 184 323 159 b 288 140 318 148 315 147 b 247 130 254 131 247 130 b 247 65 247 130 247 104 b 247 20 247 51 247 36 l 247 -88 l 273 -81 b 306 -76 289 -77 299 -76 b 318 -81 311 -76 315 -77 b 323 -123 323 -87 323 -86 l 323 -138 l 323 -154 b 318 -195 323 -191 323 -190 b 269 -210 314 -199 315 -199 b 249 -216 259 -213 250 -216 l 247 -216 l 247 -349 l 246 -483 l 245 -487 b 225 -499 242 -495 234 -499 b 206 -487 219 -499 210 -495 l 205 -483 l 205 -355 l 205 -227 l 204 -227 l 181 -233 l 138 -244 b 117 -249 127 -247 117 -249 b 115 -385 115 -249 115 -256 l 115 -523 l 114 -526 b 95 -538 110 -534 102 -538 b 74 -526 87 -538 78 -534 l 73 -523 l 73 -391 b 72 -260 73 -269 73 -260 b 72 -260 72 -260 72 -260 b 19 -273 61 -263 23 -273 b 0 -260 10 -273 4 -267 b 0 -209 0 -256 0 -256 l 0 -162 l 1 -158 b 61 -134 5 -148 5 -148 l 73 -131 l 73 -22 b 72 86 73 79 73 86 b 72 86 72 86 72 86 b 19 74 61 83 23 74 b 0 86 10 74 4 79 b 0 137 0 90 0 90 l 0 184 l 1 188 b 61 212 5 198 5 198 l 73 215 l 73 348 l 73 481 l 74 485 b 95 498 78 492 87 498 b 103 495 98 498 100 496 b 114 485 107 494 111 489 l 115 481 l 115 353 l 115 226 l 121 226 b 159 235 123 227 141 231 l 198 247 l 205 248 l 205 384 l 205 521 l 206 524 b 217 535 209 528 212 533 m 205 9 b 205 119 205 70 205 119 l 205 119 b 182 113 204 119 194 116 l 138 102 b 117 97 127 99 117 97 b 115 -12 115 97 115 91 l 115 -122 l 121 -120 b 159 -111 123 -119 141 -115 l 198 -101 l 205 -98 l 205 9 " },
    "v1b": { "x_min": 0, "x_max": 559.421875, "ha": 571, "o": "m 544 204 b 548 204 545 204 547 204 b 559 194 555 204 559 199 b 559 190 559 192 559 191 b 530 156 559 188 556 184 b 462 86 510 134 481 104 b 453 76 458 81 454 77 l 446 70 l 441 65 b 434 59 439 63 436 61 l 427 54 b 409 37 426 51 416 44 b 392 23 398 29 394 26 b 387 19 389 22 387 20 b 379 13 386 19 383 16 l 371 8 l 367 5 l 359 -1 l 337 -16 b 285 -48 319 -29 298 -41 l 279 -52 b 186 -95 255 -66 210 -87 l 175 -99 b 23 -129 127 -117 68 -129 b 17 -129 20 -129 19 -129 b 1 -123 2 -129 2 -129 b 0 -49 0 -122 0 -83 b 0 4 0 -22 0 1 b 27 11 2 9 4 9 b 185 31 78 12 145 20 b 198 34 186 31 193 33 b 314 73 234 44 277 58 b 349 88 328 79 340 84 b 353 90 352 90 353 90 b 363 94 353 90 357 93 b 371 98 367 97 371 98 b 428 129 372 98 413 120 b 461 148 441 136 454 144 b 468 151 464 149 466 151 b 472 154 469 152 470 154 b 481 161 473 155 477 158 b 525 190 490 166 518 186 l 534 197 b 540 201 536 198 539 199 b 544 204 541 202 544 204 " },
    "v1d": { "x_min": 0, "x_max": 619.3125, "ha": 632, "o": "m 274 184 b 307 186 285 186 296 186 b 616 22 465 186 597 116 b 619 -1 617 13 619 5 b 308 -187 619 -104 483 -187 b 0 -1 133 -187 0 -102 b 5 36 0 11 1 23 b 274 184 29 115 141 176 m 289 161 b 272 162 284 162 277 162 b 171 41 209 162 171 108 b 205 -73 171 5 182 -34 b 345 -163 243 -133 298 -163 b 436 -98 385 -163 420 -142 b 446 -43 443 -80 446 -62 b 289 161 446 47 377 147 " },
    "v1e": { "x_min": -402.890625, "x_max": 401.53125, "ha": 410, "o": "m -219 173 b -213 174 -217 174 -215 174 b -202 173 -209 174 -205 173 b -114 86 -200 172 -179 151 b -28 0 -66 37 -28 0 b 40 84 -28 0 2 37 b 117 174 111 173 110 172 b 122 174 118 174 119 174 b 132 173 125 174 129 173 b 295 11 134 172 171 134 l 307 -1 l 336 34 b 374 76 366 72 368 74 b 381 77 375 77 378 77 b 401 56 392 77 401 68 b 400 48 401 54 401 51 b 223 -172 397 41 230 -166 b 210 -176 220 -174 215 -176 b 201 -174 206 -176 204 -176 b 112 -87 198 -173 178 -152 b 27 0 65 -38 27 0 b -42 -86 27 0 -4 -38 b -118 -174 -112 -174 -111 -173 b -123 -176 -119 -176 -121 -176 b -133 -174 -126 -176 -130 -174 b -296 -12 -136 -173 -172 -137 l -308 0 l -337 -34 b -375 -77 -367 -73 -370 -76 b -382 -79 -377 -79 -379 -79 b -402 -58 -393 -79 -402 -69 b -401 -49 -402 -55 -402 -52 b -224 172 -398 -43 -228 167 b -219 173 -223 172 -220 173 " },
    "v1f": { "x_min": -340.28125, "x_max": 338.921875, "ha": 346, "o": "m -32 520 b -29 521 -31 520 -31 521 b -23 519 -27 521 -24 520 b -20 513 -21 517 -20 516 b -21 506 -20 512 -20 509 b -31 474 -23 502 -27 488 l -53 402 l -66 352 l -68 349 l -57 349 b -32 351 -51 349 -40 351 b 123 370 19 352 74 359 b 137 371 127 370 133 371 b 170 356 152 371 164 366 b 171 355 170 355 170 355 b 216 366 174 355 183 358 b 280 378 268 377 266 377 b 287 378 283 378 284 378 b 332 349 307 378 322 369 b 338 319 336 341 338 330 b 332 301 338 310 336 302 b 242 280 329 299 246 280 b 242 280 242 280 242 280 b 235 288 236 280 235 283 b 235 292 235 290 235 291 b 236 302 236 297 236 299 b 220 337 236 316 230 330 l 216 340 l 210 335 b 159 276 189 322 172 301 b 118 149 152 265 156 274 b 81 34 84 36 85 36 b -8 13 78 33 -4 13 b -8 13 -8 13 -8 13 b -14 20 -12 15 -14 15 b -8 44 -14 24 -12 31 b -2 66 -5 55 -2 65 b -2 66 -2 66 -2 66 l -2 66 b -43 41 -2 66 -21 55 b -114 4 -98 8 -98 8 b -144 0 -123 0 -134 0 b -242 99 -197 0 -242 43 b -242 109 -242 102 -242 105 b -212 219 -240 122 -242 116 b -185 312 -197 270 -185 312 l -185 312 b -189 312 -185 312 -186 312 b -259 312 -200 312 -227 312 b -321 310 -291 312 -310 310 b -334 312 -330 310 -334 312 b -340 319 -338 313 -340 316 b -336 326 -340 322 -338 324 b -291 337 -334 326 -314 331 l -247 347 l -210 348 b -172 348 -190 348 -172 348 b -168 363 -172 348 -171 355 b -145 442 -151 424 -145 441 b -133 452 -144 444 -140 446 l -77 489 b -32 520 -53 506 -32 520 m 57 334 b 53 335 55 335 54 335 b 44 334 50 335 49 335 b -70 316 8 326 -28 320 b -78 309 -78 316 -78 316 b -108 202 -80 305 -88 274 b -141 81 -136 112 -141 93 b -140 74 -141 79 -141 77 b -117 49 -137 59 -127 49 b -107 52 -114 49 -110 51 b 16 127 -106 54 14 126 b 42 217 16 127 42 215 b 49 241 42 222 44 229 b 73 320 53 251 73 317 b 57 334 73 327 65 333 " },
    "v20": { "x_min": -571.671875, "x_max": 570.3125, "ha": 582, "o": "m -559 351 b -551 352 -556 352 -553 352 b -530 338 -543 352 -533 348 b -529 169 -530 337 -529 291 l -529 1 l -507 27 l -441 112 b -382 174 -394 169 -390 174 b -378 174 -381 174 -379 174 b -281 86 -370 174 -375 179 b -196 0 -234 37 -196 0 b -126 84 -196 0 -164 37 b -50 174 -55 173 -57 172 b -44 174 -49 174 -47 174 b -35 173 -42 174 -38 173 b 53 86 -32 172 -12 151 b 138 0 100 37 138 0 b 208 84 140 0 170 37 b 284 174 279 173 279 172 b 289 174 285 174 288 174 b 300 173 294 174 298 173 b 462 11 303 172 340 134 l 475 -1 l 503 34 b 541 76 534 72 536 74 b 548 77 544 77 545 77 b 570 56 560 77 570 68 b 567 48 570 54 568 51 b 392 -172 564 41 397 -166 b 378 -176 387 -174 382 -176 b 368 -174 375 -176 371 -176 b 280 -87 367 -173 347 -152 b 194 0 234 -38 194 0 b 126 -86 194 0 163 -38 b 49 -174 54 -174 55 -173 b 44 -176 47 -176 46 -176 b 34 -174 40 -176 36 -174 b -54 -87 31 -173 10 -152 b -140 0 -102 -38 -140 0 b -209 -86 -140 0 -171 -38 b -285 -174 -280 -174 -279 -173 b -291 -176 -287 -176 -288 -176 b -300 -174 -294 -176 -298 -174 b -464 -11 -303 -173 -374 -102 l -476 0 l -506 -37 b -539 -76 -528 -65 -537 -74 b -551 -80 -543 -79 -547 -80 b -570 -68 -558 -80 -566 -76 l -571 -65 l -571 136 b -570 340 -571 331 -571 337 b -559 351 -568 344 -564 348 " },
    "v22": { "x_min": 0, "x_max": 432.828125, "ha": 442, "o": "m 209 186 b 213 187 210 187 212 187 b 216 187 215 187 216 187 b 224 174 216 186 220 180 b 420 -1 269 105 338 43 b 432 -12 431 -8 432 -9 b 421 -23 432 -15 432 -16 b 228 -180 345 -70 264 -137 b 219 -188 221 -188 221 -188 l 219 -188 b 208 -177 215 -188 215 -188 b 10 1 163 -106 93 -44 b 0 11 0 6 0 8 b 10 22 0 13 0 15 b 202 179 87 69 167 136 b 209 186 206 183 209 186 " },
    "v23": { "x_min": 0, "x_max": 133.390625, "ha": 136, "o": "m 54 66 b 65 68 58 68 61 68 b 122 37 88 68 110 56 b 133 -1 130 26 133 12 b 104 -58 133 -23 123 -44 b 66 -69 92 -65 78 -69 b 10 -38 44 -69 23 -58 b 0 -1 2 -27 0 -13 b 54 66 0 30 20 61 " },
    "v25": { "x_min": 0, "x_max": 318.5, "ha": 325, "o": "m 20 376 b 167 377 23 377 96 377 b 296 376 231 377 294 377 b 318 347 311 371 318 359 b 296 316 318 333 311 320 b 159 315 294 315 227 315 b 21 316 91 315 24 315 b 0 345 6 320 0 333 b 20 376 0 359 6 371 " },
    "v26": { "x_min": -21.78125, "x_max": 483.1875, "ha": 493, "o": "m -8 631 b -1 632 -6 632 -4 632 b 19 620 8 632 16 628 b 20 383 20 616 20 616 l 20 148 l 21 151 b 140 199 59 183 102 199 b 206 179 164 199 187 192 l 210 176 l 210 396 l 210 617 l 212 621 b 231 632 216 628 223 632 b 250 620 239 632 247 628 b 251 383 251 616 251 616 l 251 148 l 254 151 b 370 199 291 183 332 199 b 415 191 385 199 400 197 b 483 84 458 176 483 134 b 461 0 483 58 476 29 b 332 -142 439 -40 411 -72 l 255 -215 b 231 -229 240 -229 239 -229 b 216 -223 224 -229 220 -227 b 210 -158 210 -217 210 -223 b 210 -120 210 -148 210 -136 l 210 -29 l 205 -34 b 100 -142 182 -65 159 -88 l 23 -215 b -1 -229 9 -229 6 -229 b -20 -216 -9 -229 -17 -224 l -21 -212 l -21 201 l -21 616 l -20 620 b -8 631 -17 624 -13 630 m 110 131 b 96 133 106 133 100 133 b 89 133 93 133 91 133 b 24 87 63 129 40 113 l 20 80 l 20 -37 l 20 -156 l 23 -152 b 144 81 96 -72 144 20 l 144 83 b 110 131 144 113 134 126 m 341 131 b 328 133 337 133 332 133 b 322 133 326 133 323 133 b 257 87 296 129 273 113 l 251 80 l 251 -37 l 251 -156 l 255 -152 b 375 81 328 -72 375 20 l 375 83 b 341 131 375 113 367 126 " },
    "v27": { "x_min": 0, "x_max": 432.828125, "ha": 442, "o": "m 208 184 b 213 187 209 186 212 187 b 224 176 217 187 221 183 b 245 147 225 172 235 159 b 419 -1 288 90 347 38 b 431 -8 424 -4 431 -8 b 432 -12 432 -9 432 -11 b 430 -18 432 -13 432 -16 b 364 -61 424 -20 383 -47 b 225 -183 307 -102 250 -152 b 223 -187 224 -184 223 -187 b 220 -188 221 -188 220 -188 b 208 -176 216 -188 210 -184 b 187 -148 205 -173 197 -159 b 12 0 144 -90 84 -38 b 0 11 4 5 0 8 b 16 24 0 13 4 18 b 183 158 83 69 141 115 b 208 184 194 169 198 173 m 183 105 b 176 113 181 109 176 113 b 172 109 176 113 175 112 b 92 45 149 90 117 62 l 88 41 l 102 31 b 247 -105 160 -6 210 -55 l 254 -115 l 257 -112 l 269 -102 b 340 -45 287 -87 319 -61 l 344 -43 l 330 -33 b 183 105 272 6 221 54 " },
    "v28": { "x_min": -73.5, "x_max": 72.140625, "ha": 74, "o": "m -72 252 l -73 254 l 0 254 l 72 254 l 70 252 b 0 -1 70 248 0 -1 b -72 252 -1 -1 -72 248 " },
    "v29": { "x_min": -590.71875, "x_max": 589.359375, "ha": 601, "o": "m 175 273 b 182 274 178 273 181 274 b 202 262 190 274 198 269 b 204 158 204 259 204 259 l 204 56 l 250 112 b 303 174 296 172 298 172 b 308 174 304 174 307 174 b 318 173 313 174 317 173 b 481 11 322 172 357 134 l 494 -1 l 522 34 b 560 76 553 72 555 74 b 567 77 563 77 564 77 b 589 56 579 77 589 68 b 586 48 589 54 588 51 b 411 -172 583 41 416 -166 b 397 -176 406 -174 401 -176 b 387 -174 393 -176 390 -176 b 299 -87 386 -173 366 -152 b 213 0 253 -38 213 0 b 208 -6 213 0 210 -2 l 204 -12 l 204 -147 b 204 -210 204 -173 204 -194 b 198 -292 204 -297 204 -287 b 183 -299 194 -297 189 -299 b 164 -287 175 -299 167 -295 b 163 -174 163 -284 163 -284 l 161 -63 l 119 -117 b 65 -176 76 -170 73 -176 b 61 -176 63 -176 62 -176 b -35 -87 51 -174 57 -180 b -121 0 -83 -38 -121 0 b -190 -86 -122 0 -152 -38 b -266 -174 -261 -174 -259 -173 b -272 -176 -268 -176 -270 -176 b -281 -174 -276 -176 -280 -174 b -371 -86 -284 -173 -304 -152 b -457 0 -417 -38 -457 0 l -457 0 b -477 -26 -457 0 -470 -16 b -548 -227 -524 -88 -548 -161 b -536 -303 -548 -254 -544 -280 b -533 -317 -534 -309 -533 -313 b -553 -338 -533 -330 -541 -338 b -577 -315 -566 -338 -571 -333 b -590 -227 -586 -287 -590 -258 b -518 -9 -590 -154 -564 -77 b -465 56 -509 2 -504 8 l -402 134 b -363 174 -374 170 -371 174 b -359 174 -362 174 -360 174 b -262 86 -351 174 -356 179 b -176 0 -216 37 -176 0 b -107 84 -176 0 -145 37 b -31 174 -36 173 -38 172 b -25 174 -29 174 -28 174 b -16 173 -23 174 -19 173 b 147 11 -13 172 35 123 l 157 -1 l 160 1 l 163 4 l 163 130 b 164 260 163 256 163 258 b 175 273 166 266 170 270 " },
    "v2a": { "x_min": -21.78125, "x_max": 366.140625, "ha": 374, "o": "m 276 1378 b 284 1379 279 1379 281 1379 b 306 1360 292 1379 298 1374 b 352 1247 326 1326 343 1286 b 366 1139 362 1213 366 1175 b 347 1009 366 1093 359 1049 l 344 1002 l 347 992 b 352 971 348 986 351 977 b 366 863 362 936 366 899 b 347 732 366 818 359 773 l 344 725 l 347 716 b 352 695 348 710 351 700 b 366 588 362 659 366 623 b 223 262 366 464 314 345 b 189 233 212 252 212 252 b 35 76 126 183 73 129 b -1 16 20 56 2 27 b -19 4 -4 9 -12 4 l -21 4 l -21 137 l -21 270 l -17 270 b 186 344 59 281 134 308 b 319 606 270 399 319 499 b 317 650 319 620 319 635 l 315 659 l 314 655 b 223 537 288 607 258 570 b 189 509 212 528 212 528 b 35 352 126 459 73 405 b -1 292 20 333 2 303 b -19 280 -4 285 -12 280 l -21 280 l -21 413 l -21 546 l -17 546 b 186 620 59 557 134 584 b 319 882 270 675 319 775 b 317 925 319 896 319 911 l 315 935 l 314 931 b 223 813 288 884 258 846 b 189 785 212 805 212 805 b 35 628 126 735 73 681 b -1 569 20 609 2 580 b -19 556 -4 562 -12 556 l -21 556 l -21 689 l -21 823 l -17 823 b 202 907 68 835 152 867 b 319 1157 280 968 319 1061 b 270 1338 319 1218 303 1281 b 262 1358 264 1349 262 1353 b 262 1364 262 1360 262 1363 b 276 1378 265 1371 269 1376 " },
    "v2c": { "x_min": -597.53125, "x_max": 596.171875, "ha": 608, "o": "m -413 173 b -408 174 -412 174 -409 174 b -397 173 -404 174 -400 173 b -308 86 -394 172 -374 151 b -223 0 -261 37 -223 0 b -153 84 -223 0 -191 37 b -77 174 -83 173 -84 172 b -72 174 -76 174 -74 174 b -62 173 -68 174 -63 173 b 25 86 -59 172 -39 151 b 112 0 73 37 111 0 b 181 84 112 0 144 37 b 257 174 251 173 251 172 b 262 174 258 174 261 174 b 273 173 266 174 270 173 b 436 9 276 172 347 101 l 447 -1 l 477 36 b 522 79 511 79 513 79 l 522 79 b 552 51 533 79 539 73 b 596 -112 582 6 596 -51 b 567 -262 596 -161 586 -213 b 539 -322 558 -287 544 -316 b 524 -327 534 -326 529 -327 b 504 -315 515 -327 507 -323 b 503 -308 503 -312 503 -309 b 511 -285 503 -302 504 -297 b 555 -113 540 -227 555 -169 b 544 -34 555 -86 551 -59 b 522 19 540 -16 530 8 l 521 22 l 481 -26 l 405 -122 b 353 -176 366 -172 362 -176 b 349 -176 352 -176 351 -176 b 253 -87 341 -176 347 -180 b 167 0 206 -38 167 0 b 99 -86 167 0 136 -38 b 21 -174 27 -174 28 -173 b 17 -176 20 -176 19 -176 b 6 -174 13 -176 9 -174 b -81 -87 4 -173 -14 -152 b -167 0 -129 -38 -167 0 b -236 -86 -167 0 -198 -38 b -313 -174 -307 -174 -306 -173 b -318 -176 -314 -176 -315 -176 b -328 -174 -321 -176 -325 -174 b -491 -12 -330 -173 -367 -137 l -503 0 l -530 -34 b -570 -77 -562 -73 -564 -76 b -577 -79 -571 -79 -574 -79 b -597 -58 -588 -79 -597 -69 b -596 -49 -597 -55 -597 -52 b -417 172 -593 -43 -423 167 b -413 173 -417 172 -415 173 " },
    "v2d": { "x_min": 0, "x_max": 438.28125, "ha": 447, "o": "m 212 190 b 219 191 213 191 216 191 b 236 176 225 191 228 190 b 419 18 277 105 341 49 b 436 5 431 13 434 11 b 438 -1 438 4 438 1 b 424 -16 438 -8 432 -13 b 356 -49 409 -20 379 -36 b 234 -180 306 -83 258 -133 b 219 -192 230 -188 224 -192 b 200 -176 213 -192 206 -187 b 9 -15 157 -102 89 -45 b 0 0 2 -12 0 -6 b 16 18 0 9 2 12 b 200 176 93 48 159 104 b 212 190 205 186 208 188 m 239 113 b 236 117 238 116 238 117 b 230 108 235 117 234 115 b 92 -15 196 58 140 8 b 88 -18 91 -16 88 -18 b 92 -20 88 -18 91 -19 b 198 -116 130 -43 166 -74 b 200 -117 200 -117 200 -117 b 201 -117 200 -117 201 -117 b 264 -43 212 -98 242 -62 b 345 15 288 -19 321 4 b 348 18 347 16 348 16 b 344 20 348 18 347 19 b 239 113 307 41 266 79 " },
    "v2f": { "x_min": -1.359375, "x_max": 680.5625, "ha": 694, "o": "m 597 1042 b 604 1042 600 1042 602 1042 b 642 1002 627 1042 642 1022 b 619 966 642 988 635 974 b 439 927 574 942 503 927 l 426 927 l 426 921 b 430 838 428 893 430 866 b 345 480 430 696 398 560 b 179 391 307 423 249 391 b 156 392 171 391 164 392 b 138 394 149 394 142 394 b 103 434 115 396 103 416 b 129 471 103 451 111 466 b 141 474 133 473 137 474 b 172 459 153 474 164 469 b 181 455 175 456 176 455 b 187 456 182 455 185 455 b 253 520 212 460 234 483 b 315 836 294 605 315 714 b 311 928 315 867 314 898 b 302 945 310 943 311 942 b 245 953 283 950 262 953 b 130 891 193 953 149 931 b 84 860 119 870 102 860 b 36 905 61 860 39 877 b 36 910 36 907 36 909 b 80 970 36 931 50 949 b 249 1017 125 1000 187 1017 b 322 1009 273 1017 299 1014 l 341 1003 b 436 991 372 995 406 991 b 577 1031 495 991 545 1004 b 597 1042 583 1038 590 1041 m 416 360 b 424 360 419 360 421 360 b 481 309 454 360 479 338 b 503 145 484 280 495 199 b 585 -185 525 16 555 -106 b 630 -245 596 -213 613 -237 l 634 -247 l 638 -245 b 647 -244 641 -245 645 -244 b 680 -278 666 -244 680 -262 b 664 -308 680 -290 675 -301 b 638 -312 658 -310 650 -312 b 613 -309 631 -312 623 -310 b 477 -201 555 -303 502 -260 b 417 -2 460 -159 434 -72 b 416 5 417 1 416 5 b 416 5 416 5 416 5 b 411 -5 415 5 413 0 b 359 -97 397 -33 377 -70 b 353 -106 355 -102 353 -105 b 359 -112 353 -108 355 -109 b 409 -130 375 -123 390 -129 b 426 -134 420 -130 421 -131 b 431 -147 428 -137 431 -141 b 420 -162 431 -152 427 -159 b 382 -169 409 -166 396 -169 b 323 -155 363 -169 341 -165 l 317 -152 l 314 -155 b 62 -303 240 -240 148 -295 b 36 -305 55 -305 44 -305 b 23 -303 29 -305 24 -305 b -1 -273 6 -299 -1 -287 b 31 -240 -1 -256 10 -240 b 36 -240 32 -240 34 -240 b 42 -241 38 -241 39 -241 b 134 -204 63 -241 99 -226 b 367 288 265 -115 357 81 b 375 330 368 313 370 320 b 416 360 383 347 400 358 m 360 -359 b 379 -359 363 -359 371 -359 b 424 -360 396 -359 416 -359 b 646 -502 536 -373 624 -430 b 649 -527 649 -510 649 -519 b 530 -673 649 -578 604 -635 l 521 -677 l 529 -681 b 653 -811 592 -714 637 -762 b 660 -853 658 -827 660 -839 b 645 -911 660 -873 656 -892 b 426 -1021 608 -981 519 -1021 b 283 -989 377 -1021 328 -1011 b 235 -949 249 -972 239 -964 b 234 -936 234 -946 234 -941 b 234 -928 234 -934 234 -931 l 235 -925 l 234 -927 l 225 -934 b 87 -982 186 -966 138 -982 b 80 -982 85 -982 83 -982 b 55 -981 70 -981 58 -981 b 17 -943 32 -981 17 -964 b 54 -904 17 -921 35 -904 b 78 -914 62 -904 72 -909 l 83 -918 l 88 -918 b 190 -831 122 -918 166 -881 b 269 -506 242 -727 269 -612 b 268 -462 269 -492 269 -477 b 266 -449 266 -458 266 -452 b 265 -444 266 -445 266 -444 b 257 -446 264 -444 261 -445 b 132 -545 196 -470 152 -505 b 88 -573 122 -563 104 -573 b 39 -523 63 -573 39 -553 b 63 -476 39 -505 44 -494 b 360 -359 136 -408 235 -369 m 419 -424 b 393 -423 411 -423 406 -423 l 375 -423 l 377 -426 b 379 -439 377 -427 378 -434 b 383 -510 382 -463 383 -487 b 314 -811 383 -609 360 -710 b 266 -893 296 -850 285 -870 b 264 -898 265 -896 264 -898 l 264 -898 b 264 -898 264 -898 264 -898 b 268 -898 264 -898 266 -898 b 273 -898 270 -898 272 -898 b 300 -909 283 -898 291 -900 b 426 -957 340 -941 385 -957 b 476 -949 443 -957 460 -954 b 547 -853 522 -931 547 -893 b 485 -745 547 -816 526 -775 b 397 -707 460 -727 432 -714 b 366 -675 375 -703 366 -692 b 396 -642 366 -657 377 -645 b 530 -557 455 -637 511 -601 b 536 -527 534 -548 536 -537 b 419 -424 536 -480 490 -437 " },
    "v30": { "x_min": -21.78125, "x_max": 367.5, "ha": 375, "o": "m 276 1900 b 284 1901 279 1900 281 1901 b 306 1883 291 1901 298 1896 b 367 1686 347 1825 367 1757 b 343 1558 367 1643 359 1600 l 338 1549 l 343 1537 b 367 1411 359 1497 367 1454 b 343 1282 367 1367 359 1324 l 338 1272 l 343 1261 b 367 1135 359 1221 367 1178 b 343 1007 367 1090 359 1047 l 338 996 l 343 985 b 367 859 359 945 367 902 b 343 731 367 814 359 771 l 338 720 l 343 709 b 367 582 359 667 367 626 b 289 362 367 503 340 426 b 239 312 276 345 259 330 b 29 77 152 237 76 152 b -1 18 14 54 2 30 b -19 4 -4 11 -12 4 l -21 4 l -21 133 l -20 260 l -13 262 b 98 299 17 269 62 284 b 111 305 103 302 110 305 b 167 334 123 310 156 327 b 319 595 264 391 319 491 b 313 659 319 616 318 638 b 310 667 311 664 311 667 b 307 663 310 667 308 666 b 240 588 289 637 269 614 b 16 331 141 505 62 413 b -1 294 8 316 1 302 b -19 280 -4 287 -12 280 l -21 280 l -21 408 l -20 537 l -13 538 b 98 576 17 545 62 560 b 111 581 103 578 110 581 b 167 610 123 587 156 603 b 319 871 264 667 319 767 b 313 935 319 892 318 913 b 310 942 311 941 311 942 b 307 939 310 942 308 941 b 240 864 289 913 269 889 b 16 607 141 781 62 689 b -1 570 8 592 1 578 b -19 556 -4 563 -12 556 l -21 556 l -21 684 l -20 813 l -13 814 b 98 852 17 821 62 836 b 111 857 103 855 110 857 b 167 886 123 863 156 880 b 319 1147 264 943 319 1043 b 313 1211 319 1168 318 1189 b 310 1218 311 1217 311 1218 b 307 1215 310 1218 308 1217 b 240 1140 289 1188 269 1165 b 16 884 141 1057 62 966 b -1 846 8 868 1 855 b -19 832 -4 839 -12 832 l -21 832 l -21 960 l -20 1089 l -13 1090 b 98 1128 17 1097 62 1111 b 111 1134 103 1131 110 1134 b 167 1163 123 1139 156 1156 b 319 1424 264 1220 319 1320 b 313 1486 319 1444 318 1465 b 310 1494 311 1493 311 1494 b 307 1492 310 1494 308 1493 b 240 1417 289 1464 269 1442 b 16 1160 141 1333 62 1242 b -1 1121 8 1145 1 1131 b -19 1109 -4 1115 -12 1109 l -21 1109 l -21 1236 l -20 1365 l -13 1367 b 98 1404 17 1374 62 1388 b 111 1410 103 1407 110 1410 b 250 1508 172 1437 215 1467 b 319 1701 296 1564 319 1633 b 270 1859 319 1757 303 1814 b 262 1882 265 1868 262 1875 b 276 1900 262 1890 266 1896 " },
    "v31": { "x_min": 0, "x_max": 386.5625, "ha": 394, "o": "m 0 173 l 0 347 l 193 347 l 386 347 l 386 173 l 386 0 l 193 0 l 0 0 l 0 173 " },
    "v33": { "x_min": -423.3125, "x_max": 421.9375, "ha": 431, "o": "m -10 276 b -2 277 -8 277 -5 277 b 17 265 5 277 13 273 b 19 163 19 260 19 260 l 19 68 l 39 45 b 277 -95 122 -34 200 -81 b 289 -97 281 -97 285 -97 b 378 0 332 -97 371 -54 b 378 11 378 4 378 6 b 302 83 378 55 345 83 b 242 66 283 83 262 77 b 208 56 231 59 219 56 b 148 120 175 56 148 81 b 200 186 148 151 164 172 b 261 198 220 194 240 198 b 420 45 341 198 411 137 b 421 22 421 37 421 29 b 257 -198 421 -86 347 -188 b 242 -198 251 -198 247 -198 b 20 -105 181 -198 95 -163 l 19 -104 l 19 -183 b 19 -216 19 -195 19 -206 b 12 -273 19 -272 17 -267 b -2 -278 8 -277 2 -278 b -21 -266 -10 -278 -19 -274 b -23 -165 -23 -263 -23 -262 l -23 -69 l -44 -47 b -250 86 -117 23 -183 66 b -295 94 -270 93 -284 94 b -315 91 -302 94 -308 94 b -381 5 -356 81 -381 43 b -355 -56 -381 -16 -372 -40 b -299 -81 -338 -73 -319 -81 b -246 -68 -283 -81 -265 -77 b -212 -58 -234 -61 -223 -58 b -168 -77 -196 -58 -179 -65 b -151 -122 -156 -90 -151 -105 b -179 -174 -151 -141 -160 -162 b -239 -195 -194 -184 -217 -192 b -257 -197 -245 -195 -250 -197 b -423 -5 -349 -197 -423 -113 b -423 0 -423 -4 -423 -1 b -277 194 -420 97 -362 173 b -247 197 -268 197 -258 197 b -24 104 -185 197 -100 162 l -23 102 l -23 181 b -21 265 -23 260 -23 260 b -10 276 -20 269 -14 274 " },
    "v34": { "x_min": 0, "x_max": 622.03125, "ha": 635, "o": "m 398 417 b 406 419 401 419 404 419 b 427 398 417 419 427 409 b 427 391 427 395 427 392 b 34 -274 424 385 38 -272 b 20 -280 29 -278 25 -280 b 0 -259 9 -280 0 -270 b 0 -252 0 -256 0 -254 b 393 413 2 -247 389 410 b 398 417 394 415 397 416 m 592 417 b 600 419 594 419 597 419 b 622 398 611 419 622 409 b 620 391 622 395 620 392 b 227 -274 617 385 231 -272 b 213 -280 223 -278 219 -280 b 193 -259 202 -280 193 -270 b 194 -252 193 -256 193 -254 b 586 413 196 -247 582 410 b 592 417 588 415 590 416 " },
    "v36": { "x_min": -1.359375, "x_max": 1064.390625, "ha": 1086, "o": "m 296 692 b 314 694 302 694 307 694 b 386 685 337 694 366 689 b 548 498 480 660 548 580 b 548 481 548 492 548 487 b 455 395 541 426 499 395 b 370 462 420 395 383 417 b 362 496 364 477 362 488 b 377 514 362 509 367 514 b 393 501 386 514 390 510 b 432 474 397 484 413 474 b 470 487 445 474 458 478 b 491 530 484 496 491 510 b 490 544 491 534 491 539 b 333 660 479 606 411 657 l 323 662 l 315 646 b 269 524 285 591 269 556 b 321 431 269 492 287 466 b 349 395 338 413 343 408 b 363 342 359 378 363 362 b 359 312 363 333 362 322 b 285 158 348 266 318 206 b 281 152 283 155 281 152 b 281 152 281 152 281 152 b 287 154 283 152 284 152 b 318 155 298 154 308 155 b 461 98 371 155 419 136 l 464 97 l 483 112 b 503 129 494 120 503 127 b 504 130 503 129 504 129 b 503 138 504 131 503 134 b 500 180 500 152 500 166 b 553 326 500 238 518 288 b 604 366 560 331 592 358 b 649 381 617 376 632 381 b 696 362 665 381 681 374 b 724 302 714 347 724 324 b 695 238 724 278 714 255 b 660 210 691 234 662 212 b 579 148 658 209 582 151 b 579 148 579 148 579 148 b 596 106 579 144 589 119 b 622 77 604 88 609 83 b 657 69 632 72 645 69 b 748 112 688 69 721 84 b 755 123 754 117 755 120 b 755 127 755 124 755 126 b 751 165 752 137 751 151 b 758 219 751 183 754 202 b 894 387 774 290 820 347 b 896 390 896 388 896 388 b 891 398 896 391 895 392 b 622 560 827 477 730 535 b 600 580 605 564 600 569 b 617 596 600 591 607 596 b 628 595 622 596 624 596 b 1057 248 846 552 1020 412 b 1064 191 1061 229 1064 209 b 922 0 1064 94 1005 9 b 902 -1 916 -1 909 -1 b 774 76 847 -1 800 26 b 769 83 770 81 770 83 b 769 81 769 83 769 83 b 627 -1 733 29 677 -1 b 548 27 597 -1 570 8 b 515 88 537 37 525 61 l 513 95 l 510 93 l 453 45 b 390 0 396 0 396 0 b 390 0 390 0 390 0 b 374 15 381 0 377 4 b 268 105 359 69 314 105 b 250 104 262 105 257 105 l 243 102 l 234 90 b 155 1 201 49 159 2 b 147 -1 152 0 149 -1 b 130 15 138 -1 130 6 b 132 20 130 18 132 19 b 136 31 133 22 134 27 b 220 131 149 74 178 109 b 231 137 225 134 230 136 b 302 278 280 202 302 244 b 265 335 302 299 295 309 b 209 442 234 363 213 402 b 209 455 209 446 209 451 b 279 648 209 502 232 564 l 285 659 l 283 659 b 176 627 238 653 210 645 b 57 477 111 594 66 538 b 55 459 55 471 55 464 b 72 409 55 437 61 415 b 93 403 78 405 87 403 b 152 467 123 403 151 431 b 168 488 153 483 157 488 b 185 462 181 488 185 483 l 185 460 b 137 344 183 409 168 369 b 78 322 119 328 98 322 b 13 360 50 322 25 335 b -1 426 4 380 -1 402 b 89 610 -1 488 32 559 b 296 692 147 659 210 685 m 926 348 b 921 353 924 351 922 353 b 914 348 920 353 918 351 b 823 167 857 306 823 237 b 828 124 823 154 826 138 b 890 31 837 79 862 40 b 896 31 892 31 894 31 b 956 104 916 31 940 59 b 970 191 965 129 970 159 b 966 241 970 208 969 224 b 926 348 959 277 945 313 m 627 326 b 619 326 624 326 622 326 b 598 316 611 326 604 323 b 568 215 579 288 568 255 b 568 208 568 213 568 210 b 571 183 570 195 570 184 l 571 183 b 594 201 571 183 582 191 l 634 231 b 660 259 653 247 656 248 b 664 278 662 266 664 272 b 627 326 664 299 649 320 " },
    "v38": { "x_min": -1.359375, "x_max": 651.96875, "ha": 665, "o": "m 389 644 b 405 645 394 645 400 645 b 504 566 450 645 492 613 b 507 541 506 557 507 549 b 480 471 507 514 498 489 l 477 467 l 483 470 b 609 591 539 485 586 531 b 613 601 611 595 613 599 b 631 609 619 607 624 609 b 651 588 641 609 651 602 b 200 -946 651 584 204 -941 b 182 -957 197 -953 190 -957 b 163 -945 174 -957 166 -953 b 161 -939 161 -942 161 -942 b 217 -743 161 -931 170 -904 b 272 -555 247 -639 272 -555 b 272 -555 272 -555 272 -555 b 264 -560 272 -555 268 -557 b 140 -603 227 -589 182 -603 b 36 -567 102 -603 65 -592 b -1 -487 12 -548 -1 -517 b 17 -427 -1 -466 5 -445 b 103 -380 38 -395 70 -380 b 191 -433 137 -380 172 -398 b 205 -484 201 -448 205 -466 b 178 -553 205 -509 196 -535 l 175 -557 l 182 -555 b 307 -435 236 -539 284 -494 b 372 -213 308 -430 372 -215 b 372 -213 372 -213 372 -213 b 364 -219 372 -213 368 -216 b 240 -262 328 -247 283 -262 b 137 -226 202 -262 166 -249 b 99 -145 112 -206 99 -176 b 118 -84 99 -124 106 -104 b 204 -38 138 -54 171 -38 b 292 -91 238 -38 273 -56 b 306 -141 302 -106 306 -124 b 279 -212 306 -167 296 -194 l 276 -215 l 281 -213 b 408 -93 336 -198 385 -151 b 473 129 409 -88 473 127 b 473 129 473 129 473 129 b 465 122 473 129 469 126 b 341 80 428 94 383 80 b 236 115 303 80 266 91 b 200 195 213 136 200 165 b 217 256 200 217 206 238 b 304 303 239 287 272 303 b 393 249 338 303 374 285 b 406 199 402 234 406 217 b 379 129 406 173 397 148 l 377 126 l 382 127 b 509 248 436 142 485 190 b 574 470 510 254 574 469 b 574 470 574 470 574 470 b 566 464 574 470 570 467 b 442 421 529 435 484 421 b 337 458 404 421 367 433 b 300 537 313 478 300 508 b 389 644 300 585 334 635 " },
    "v39": { "x_min": -171.5, "x_max": 251.8125, "ha": 257, "o": "m -8 631 b -1 632 -6 632 -4 632 b 19 620 8 632 16 628 b 20 553 20 616 20 614 b 20 491 20 503 20 491 l 20 491 b 153 535 47 501 149 535 b 174 514 167 535 174 524 b 164 496 174 508 171 501 b 92 470 164 495 132 484 l 20 445 l 20 390 b 20 363 20 378 20 370 b 20 333 20 340 20 333 l 20 333 b 153 377 47 344 149 377 b 174 356 167 377 174 367 b 164 338 174 349 171 342 b 92 312 164 338 132 326 l 20 288 l 20 219 l 20 148 l 21 151 b 137 199 59 183 99 199 b 182 191 152 199 167 197 b 251 84 227 176 251 134 b 228 0 251 58 243 29 b 100 -142 206 -40 178 -72 l 23 -215 b -1 -229 9 -229 6 -229 b -20 -216 -9 -229 -17 -224 b -21 30 -21 -212 -21 -212 b -21 273 -21 163 -21 273 b -84 252 -21 273 -50 263 b -152 230 -133 234 -145 230 b -157 231 -155 230 -156 231 b -171 252 -166 234 -171 244 b -160 270 -171 259 -167 266 b -27 316 -159 270 -93 294 l -21 319 l -21 374 b -21 431 -21 406 -21 431 b -84 409 -21 431 -50 421 b -152 388 -133 392 -145 388 b -157 390 -155 388 -156 388 b -171 409 -166 392 -171 401 b -160 428 -171 417 -167 424 b -27 474 -159 428 -93 451 l -21 476 l -21 546 b -20 620 -21 614 -21 616 b -8 631 -17 624 -13 630 m 110 131 b 96 133 106 133 100 133 b 89 133 93 133 91 133 b 24 87 63 129 40 113 l 20 80 l 20 -37 l 20 -156 l 23 -152 b 144 81 96 -72 144 20 l 144 83 b 110 131 144 113 134 126 " },
    "v3b": { "x_min": 0, "x_max": 484.5625, "ha": 494, "o": "m 228 245 b 239 247 234 247 239 247 b 243 247 240 247 242 247 b 303 238 257 247 287 242 b 484 -2 417 208 484 104 b 412 -177 484 -65 461 -127 b 243 -248 363 -226 303 -248 b 6 -63 138 -248 36 -180 b 0 -1 1 -41 0 -20 b 228 245 0 127 98 240 m 255 181 b 240 183 247 183 245 183 b 232 181 238 183 235 183 b 142 152 200 180 168 170 l 138 149 l 190 97 l 242 44 l 294 97 l 345 149 l 340 152 b 255 181 315 169 284 180 m 147 -54 l 197 -1 l 147 51 l 95 104 l 91 99 b 62 -1 72 70 62 34 b 66 -43 62 -15 63 -29 b 91 -101 72 -63 80 -84 l 95 -106 l 147 -54 m 393 99 b 389 104 390 102 389 104 b 337 51 389 104 366 80 l 285 -1 l 337 -54 l 389 -106 l 393 -101 b 421 -1 412 -72 421 -36 b 393 99 421 34 412 69 m 294 -98 b 242 -45 265 -69 242 -45 b 190 -98 242 -45 219 -69 l 138 -151 l 142 -154 b 242 -184 172 -174 206 -184 b 340 -154 276 -184 311 -174 l 345 -151 l 294 -98 " },
    "v3c": { "x_min": 0, "x_max": 450.53125, "ha": 460, "o": "m 189 302 b 204 303 193 302 198 303 b 303 224 250 303 292 270 b 306 199 304 216 306 208 b 279 129 306 173 296 147 l 276 126 l 281 127 b 408 249 337 142 385 190 b 412 259 409 254 412 258 b 430 267 417 265 423 267 b 450 247 441 267 450 259 b 200 -605 450 242 204 -599 b 182 -616 197 -612 190 -616 b 163 -602 174 -616 166 -610 b 161 -598 161 -601 161 -601 b 217 -402 161 -589 170 -562 b 272 -213 247 -298 272 -213 b 272 -213 272 -213 272 -213 b 264 -219 272 -213 268 -216 b 140 -262 227 -247 182 -262 b 36 -226 102 -262 65 -249 b 0 -145 12 -206 0 -176 b 17 -84 0 -124 5 -104 b 103 -38 38 -54 70 -38 b 191 -91 137 -38 172 -56 b 205 -141 201 -106 205 -124 b 178 -212 205 -167 196 -194 l 175 -215 l 182 -213 b 307 -93 236 -198 284 -151 b 372 129 308 -88 372 127 b 372 129 372 129 372 129 b 364 122 372 129 368 126 b 240 80 328 94 283 80 b 137 115 202 80 166 91 b 99 194 111 136 99 165 b 189 302 99 244 133 292 " },
    "v3e": { "x_min": 0, "x_max": 406.96875, "ha": 415, "o": "m 21 183 b 28 183 24 183 25 183 b 42 181 34 183 39 183 b 127 108 47 179 47 179 b 202 41 168 72 202 41 b 279 108 204 41 238 72 b 357 177 321 145 356 176 b 375 183 363 181 370 183 b 406 151 392 183 406 169 b 404 137 406 147 405 141 b 322 62 401 131 398 129 b 251 0 284 27 251 0 b 322 -63 251 -1 284 -29 b 404 -138 398 -130 401 -133 b 406 -152 405 -142 406 -148 b 375 -184 406 -170 392 -184 b 357 -179 370 -184 363 -183 b 279 -109 356 -177 321 -147 b 202 -43 238 -73 204 -43 b 127 -109 202 -43 168 -73 b 49 -179 85 -147 50 -177 b 31 -184 43 -183 36 -184 b 0 -152 13 -184 0 -170 b 2 -138 0 -148 0 -142 b 83 -63 5 -133 8 -130 b 155 0 122 -29 155 -1 b 83 62 155 0 122 27 b 8 129 43 97 10 127 b 0 151 2 136 0 144 b 21 183 0 165 8 177 " },
    "v3f": { "x_min": -24.5, "x_max": 317.140625, "ha": 324, "o": "m -24 -147 l -24 -5 l -20 -5 b -1 -19 -12 -5 -4 -11 b 58 -123 6 -43 31 -86 b 196 -278 93 -173 134 -219 b 317 -570 274 -356 317 -460 b 294 -713 317 -617 308 -666 l 289 -724 l 294 -735 b 317 -873 308 -780 317 -827 b 235 -1132 317 -963 288 -1054 b 209 -1165 228 -1140 224 -1146 b 189 -1177 204 -1172 196 -1177 b 171 -1164 182 -1177 175 -1172 b 168 -1154 170 -1161 168 -1159 b 181 -1132 168 -1149 172 -1142 b 269 -891 238 -1064 269 -975 b 269 -881 269 -886 269 -884 b 262 -814 269 -857 265 -827 b 258 -800 261 -811 259 -806 b 142 -628 240 -731 198 -667 b -8 -589 112 -606 47 -589 b -20 -589 -13 -589 -19 -589 l -24 -589 l -24 -449 l -24 -308 l -20 -308 b -1 -322 -12 -308 -4 -313 b 58 -424 6 -345 31 -388 b 194 -580 93 -476 136 -523 b 259 -660 221 -606 245 -635 b 261 -663 259 -662 261 -663 b 264 -656 262 -663 262 -660 b 269 -587 268 -632 269 -610 b 264 -521 269 -566 268 -544 b 262 -512 264 -517 262 -513 b 258 -498 261 -509 259 -503 b 142 -326 240 -428 198 -365 b -8 -287 112 -303 47 -288 b -20 -287 -13 -287 -19 -287 l -24 -287 l -24 -147 " },
    "v40": { "x_min": -1.359375, "x_max": 436.921875, "ha": 446, "o": "m 213 205 b 217 205 215 205 216 205 b 234 194 224 205 234 199 b 236 187 234 194 235 190 l 245 167 l 261 129 l 270 106 b 355 -61 294 54 329 -13 b 420 -163 381 -105 402 -138 b 436 -188 435 -184 436 -184 b 436 -191 436 -190 436 -190 b 421 -206 436 -201 431 -206 l 421 -206 l 416 -206 l 405 -201 b 217 -158 347 -172 283 -158 b 31 -201 153 -158 88 -172 l 20 -206 l 14 -206 l 14 -206 b 0 -191 5 -206 0 -201 b -1 -188 0 -190 -1 -190 b 14 -163 -1 -186 0 -184 b 95 -34 36 -136 72 -77 b 166 106 119 8 148 68 l 175 129 l 183 148 l 200 188 b 213 205 205 199 208 202 " },
    "v41": { "x_min": -1.359375, "x_max": 556.6875, "ha": 568, "o": "m 294 322 b 318 323 299 322 308 323 b 360 320 334 323 352 322 b 526 217 430 310 490 273 b 543 166 537 202 543 184 b 447 70 543 117 503 70 b 445 70 447 70 446 70 b 359 159 394 72 359 113 b 368 201 359 173 362 187 b 442 245 382 229 412 245 b 455 244 446 245 451 245 b 460 244 458 244 460 244 b 460 244 460 244 460 244 b 454 248 460 244 458 245 b 325 291 417 276 372 291 b 285 287 313 291 299 290 b 144 -2 183 269 144 190 b 281 -290 144 -208 179 -280 b 304 -291 289 -291 298 -291 b 524 -105 412 -291 506 -212 b 541 -84 526 -88 530 -84 b 556 -101 551 -84 556 -90 b 549 -138 556 -111 553 -122 b 334 -322 521 -237 435 -310 b 302 -324 323 -323 313 -324 b 13 -101 172 -324 54 -234 b -1 -1 4 -68 -1 -34 b 294 322 -1 161 121 303 " },
    "v42": { "x_min": -348.4375, "x_max": 24.5, "ha": 25, "o": "m -330 155 b -322 156 -329 156 -326 156 b -315 156 -319 156 -317 156 b -298 147 -311 155 -308 154 b -19 30 -224 98 -122 55 l 2 26 b 24 -1 17 22 24 13 b 2 -27 24 -15 17 -23 l -19 -31 b -298 -148 -122 -56 -224 -99 b -322 -158 -313 -158 -315 -158 b -348 -131 -338 -158 -348 -145 b -344 -117 -348 -127 -347 -122 b -328 -104 -341 -112 -338 -111 b -127 -8 -269 -65 -202 -33 b -106 0 -115 -4 -106 -1 b -127 6 -106 0 -115 2 b -328 102 -202 31 -269 63 b -344 116 -338 109 -341 111 b -348 130 -347 120 -348 124 b -330 155 -348 141 -341 152 " },
    "v43": { "x_min": -442.359375, "x_max": 441, "ha": 450, "o": "m -31 487 b -1 488 -21 488 -10 488 b 434 104 216 488 397 330 b 441 27 438 79 441 47 b 439 12 441 20 439 15 b 419 0 435 4 427 0 b 404 5 413 0 408 1 b 398 30 400 11 398 13 b 0 351 390 213 213 351 b -59 348 -20 351 -39 349 b -400 30 -251 324 -393 191 b -405 5 -400 13 -401 11 b -420 0 -409 1 -415 0 b -441 12 -428 0 -436 4 b -442 27 -441 15 -442 20 b -435 104 -442 47 -439 79 b -31 487 -401 316 -235 474 m -13 131 b -1 133 -9 133 -5 133 b 51 105 19 133 39 123 b 61 70 58 95 61 83 b 51 34 61 58 58 45 b -1 6 39 16 19 6 b -46 27 -17 6 -34 13 b -62 69 -57 38 -62 54 b -13 131 -62 98 -44 124 " },
    "v44": { "x_min": -21.78125, "x_max": 251.8125, "ha": 257, "o": "m -8 631 b -1 632 -6 632 -4 632 b 19 620 8 632 16 628 b 20 383 20 616 20 616 l 20 148 l 21 151 b 137 199 59 183 99 199 b 182 191 152 199 167 197 b 251 84 227 176 251 134 b 228 0 251 58 243 29 b 100 -142 206 -40 178 -72 l 23 -215 b 0 -229 9 -229 6 -229 b -20 -216 -9 -229 -17 -224 l -21 -212 l -21 201 l -21 616 l -20 620 b -8 631 -17 624 -13 630 m 110 131 b 96 133 106 133 100 133 b 89 133 93 133 91 133 b 24 87 63 129 40 113 l 20 80 l 20 -37 l 20 -156 l 23 -152 b 144 81 96 -72 144 20 l 144 83 b 110 131 144 113 134 126 " },
    "v45": { "x_min": -402.890625, "x_max": 401.53125, "ha": 410, "o": "m -10 273 b -4 274 -9 273 -6 274 b 16 262 4 274 12 269 b 17 158 17 259 17 259 l 17 56 l 62 112 b 117 174 110 172 110 172 b 122 174 118 174 119 174 b 132 173 125 174 129 173 b 295 11 134 172 171 134 l 307 -1 l 336 34 b 374 76 366 72 368 74 b 381 77 375 77 378 77 b 401 56 392 77 401 68 b 400 48 401 54 401 51 b 223 -172 397 41 230 -166 b 210 -176 220 -174 215 -176 b 201 -174 206 -176 204 -176 b 112 -87 198 -173 178 -152 b 27 0 65 -38 27 0 b 21 -6 27 0 24 -2 l 17 -12 l 17 -147 b 17 -210 17 -173 17 -194 b 10 -292 17 -297 16 -287 b -2 -299 6 -297 2 -299 b -21 -287 -10 -299 -19 -295 b -24 -174 -23 -284 -23 -284 l -24 -63 l -66 -117 b -121 -176 -110 -170 -114 -176 b -125 -176 -122 -176 -123 -176 b -296 -12 -134 -174 -125 -184 l -308 0 l -337 -34 b -375 -77 -367 -73 -370 -76 b -382 -79 -377 -79 -379 -79 b -402 -58 -393 -79 -402 -69 b -401 -49 -402 -55 -402 -52 b -224 170 -398 -43 -231 165 b -212 174 -221 173 -216 174 b -202 173 -208 174 -205 174 b -39 11 -200 172 -151 122 l -28 -1 l -25 1 l -24 4 l -24 130 b -23 260 -24 256 -24 258 b -10 273 -20 266 -16 270 " },
    "v46": { "x_min": 0, "x_max": 627.46875, "ha": 640, "o": "m 306 190 b 314 191 308 191 311 191 b 326 184 318 191 322 190 l 336 173 b 510 52 377 127 442 80 b 515 49 513 51 515 49 b 611 16 537 40 579 24 b 627 0 624 13 627 9 b 607 -18 627 -11 624 -13 b 330 -181 490 -49 389 -109 b 314 -192 323 -190 319 -192 b 306 -191 311 -192 308 -192 b 294 -177 302 -188 302 -188 b 257 -140 287 -170 265 -148 b 19 -18 193 -84 114 -44 b 0 0 2 -13 0 -11 b 16 16 0 9 2 13 b 110 49 47 24 89 40 b 117 52 111 49 114 51 b 145 65 126 56 130 58 b 281 163 200 93 245 124 b 300 186 288 170 291 174 b 306 190 300 187 303 188 m 317 137 b 313 142 315 141 314 142 b 308 137 313 142 311 141 b 161 4 276 84 220 33 b 155 0 159 1 155 0 b 163 -4 155 0 159 -2 b 308 -138 220 -34 276 -84 b 313 -142 311 -141 313 -142 b 317 -138 314 -142 315 -141 b 464 -4 351 -84 406 -34 b 470 0 468 -2 470 0 b 464 4 470 0 468 1 b 317 137 406 33 351 84 " },
    "v47": { "x_min": -24.5, "x_max": 315.78125, "ha": 322, "o": "m -24 -145 l -24 -5 l -20 -5 b 1 -26 -10 -5 -6 -9 b 175 -241 31 -86 96 -166 b 314 -548 259 -323 304 -420 b 315 -589 315 -555 315 -571 b 314 -630 315 -606 315 -623 b 298 -730 311 -664 306 -699 l 295 -742 l 296 -748 b 314 -850 304 -778 311 -813 b 315 -892 315 -857 315 -874 b 314 -932 315 -909 315 -925 b 298 -1032 311 -967 306 -1002 l 295 -1045 l 296 -1050 b 314 -1153 304 -1081 311 -1115 b 315 -1193 315 -1160 315 -1177 b 314 -1235 315 -1211 315 -1228 b 217 -1526 306 -1338 270 -1444 b 201 -1533 213 -1532 208 -1533 b 182 -1522 193 -1533 185 -1529 b 179 -1514 181 -1518 179 -1517 b 189 -1489 179 -1508 182 -1501 b 266 -1217 240 -1403 266 -1308 b 262 -1156 266 -1196 265 -1177 b 110 -907 247 -1043 190 -950 b 0 -889 87 -895 50 -889 l -1 -889 l -24 -889 l -24 -749 l -24 -610 l -20 -610 b 1 -631 -10 -610 -6 -614 b 175 -846 31 -691 96 -771 b 259 -956 213 -884 236 -914 b 265 -966 262 -961 264 -966 b 265 -966 265 -966 265 -966 b 265 -953 265 -964 265 -959 b 266 -920 266 -943 266 -932 b 262 -853 266 -898 265 -873 b 110 -605 247 -741 190 -648 b 0 -587 87 -592 50 -587 l -1 -587 l -24 -587 l -24 -448 l -24 -308 l -20 -308 b 1 -328 -10 -308 -6 -312 b 175 -544 31 -388 96 -469 b 259 -655 213 -581 236 -612 b 265 -663 262 -659 264 -663 b 265 -663 265 -663 265 -663 b 265 -650 265 -663 265 -657 b 266 -617 266 -641 266 -630 b 262 -551 266 -595 265 -570 b 110 -303 247 -438 190 -345 b 0 -284 87 -290 50 -284 l -1 -284 l -24 -284 l -24 -145 " },
    "v49": { "x_min": 0, "x_max": 630.203125, "ha": 643, "o": "m 308 204 b 314 205 310 205 313 205 b 326 201 319 205 323 204 b 355 154 328 199 338 180 b 401 83 362 142 392 95 l 409 72 b 431 41 412 66 424 49 b 619 -174 498 -51 570 -134 b 630 -192 626 -180 630 -186 b 626 -202 630 -195 628 -199 b 616 -206 623 -205 620 -206 b 552 -188 608 -206 592 -202 b 310 -155 488 -169 392 -155 b 268 -156 295 -155 281 -155 b 77 -188 197 -161 126 -173 b 13 -206 35 -202 20 -206 b 9 -206 12 -206 10 -206 b 0 -191 2 -202 0 -197 b 8 -176 0 -186 2 -180 b 204 49 58 -136 138 -43 l 220 72 l 227 83 b 295 188 245 108 281 166 b 308 204 299 197 304 202 m 315 147 b 314 147 315 147 314 147 b 314 147 314 147 314 147 b 306 129 314 145 310 138 l 296 105 b 281 72 292 97 284 77 l 274 56 b 181 -123 247 -4 212 -72 l 174 -134 l 176 -133 b 314 -123 215 -127 272 -123 b 451 -133 356 -123 413 -127 l 454 -134 l 449 -123 b 353 56 417 -72 381 -4 l 347 72 b 332 105 344 77 336 97 l 322 129 b 315 147 318 138 315 145 " },
    "v4a": { "x_min": 70.78125, "x_max": 378.390625, "ha": 315, "o": "m 246 373 b 254 373 249 373 251 373 b 372 324 303 373 360 351 b 378 302 377 317 378 309 b 338 251 378 278 362 255 b 328 249 334 249 332 249 b 283 294 303 249 283 270 b 288 315 283 301 284 308 b 289 319 289 317 289 319 b 289 319 289 319 289 319 b 283 320 289 320 287 320 b 270 322 279 322 274 322 b 206 288 242 322 215 308 b 206 283 206 287 206 285 b 257 223 206 267 230 238 b 284 206 272 213 277 210 b 351 90 328 173 351 130 b 340 47 351 74 348 59 b 205 -30 314 -2 264 -30 b 182 -29 198 -30 190 -30 b 84 15 147 -24 103 -5 b 70 48 74 24 70 36 b 108 99 70 70 85 94 b 121 102 112 101 117 102 b 167 56 147 102 167 80 b 159 31 167 48 164 40 l 156 26 l 157 26 b 190 20 167 22 178 20 b 220 26 201 20 212 22 b 258 65 243 34 258 51 b 257 70 258 66 258 69 b 204 126 249 94 234 109 b 114 258 148 158 114 209 b 125 302 114 273 118 288 b 246 373 147 342 193 370 " },
    "v4b": { "x_min": 0, "x_max": 503.609375, "ha": 514, "o": "m 274 430 b 277 430 276 430 277 430 b 310 394 296 430 310 415 b 308 383 310 391 308 387 b 306 367 307 381 307 374 b 236 120 298 305 272 210 b 40 -273 189 -5 125 -134 b 20 -287 35 -283 27 -287 b 5 -281 14 -287 9 -285 b 0 -267 1 -277 0 -273 b 9 -242 0 -262 2 -255 b 246 395 137 -12 232 242 b 274 430 249 416 257 427 m 468 430 b 472 430 469 430 470 430 b 503 394 490 430 503 415 b 502 383 503 391 503 387 b 499 367 502 381 500 374 b 431 120 491 305 465 210 b 234 -273 382 -5 318 -134 b 213 -287 228 -283 220 -287 b 198 -281 208 -287 202 -285 b 193 -267 194 -277 193 -273 b 202 -242 193 -262 196 -255 b 439 395 330 -12 426 242 b 468 430 442 416 451 427 " },
    "v4d": { "x_min": -311.6875, "x_max": 310.328125, "ha": 317, "o": "m -9 388 b -2 390 -8 390 -5 390 b 5 388 1 390 4 390 b 19 378 10 387 16 383 b 23 333 23 371 23 371 b 24 298 23 299 24 298 b 81 276 34 298 65 285 b 213 91 145 240 190 177 b 224 24 217 76 224 36 b 257 24 224 24 235 24 b 299 19 292 24 292 24 b 310 -1 306 15 310 6 b 299 -23 310 -11 306 -19 b 257 -27 292 -27 292 -27 b 224 -29 235 -27 224 -29 b 213 -95 224 -40 217 -80 b 81 -280 190 -181 145 -244 b 24 -301 65 -290 34 -301 b 23 -335 24 -301 23 -303 l 23 -340 b 17 -381 23 -374 23 -374 b -1 -391 13 -388 5 -391 b -21 -381 -9 -391 -17 -388 b -27 -340 -27 -374 -27 -374 l -27 -335 b -28 -301 -27 -303 -27 -301 b -85 -280 -38 -301 -69 -290 b -217 -95 -149 -244 -194 -181 b -228 -29 -221 -80 -228 -40 b -259 -27 -228 -29 -238 -27 b -300 -23 -294 -27 -294 -27 b -311 -2 -307 -19 -311 -11 b -294 23 -311 8 -304 19 b -259 24 -291 23 -284 24 b -228 24 -239 24 -228 24 b -217 91 -228 36 -221 76 b -85 276 -194 177 -149 240 b -28 298 -69 285 -38 298 b -27 333 -27 298 -27 299 b -27 371 -27 362 -27 369 b -9 388 -24 378 -17 385 m -27 136 b -28 247 -27 197 -28 247 b -61 216 -31 247 -53 226 b -123 33 -95 172 -121 98 l -125 24 l -76 24 l -27 24 l -27 136 m 29 242 b 24 247 27 245 24 247 b 23 136 24 247 23 197 l 23 24 l 72 24 l 121 24 l 119 33 b 29 242 115 116 77 206 m -27 -140 l -27 -27 l -76 -27 l -125 -27 l -123 -36 b -61 -220 -121 -102 -95 -176 b -28 -251 -53 -230 -31 -251 b -27 -140 -28 -251 -27 -201 m 119 -36 l 121 -27 l 72 -27 l 23 -27 l 23 -140 b 24 -251 23 -201 24 -251 b 57 -220 27 -251 49 -230 b 119 -36 91 -176 117 -102 " },
    "v4e": { "x_min": 0, "x_max": 239.5625, "ha": 244, "o": "m 10 460 b 20 462 13 462 14 462 b 39 449 28 462 35 458 l 40 446 l 40 326 b 40 205 40 259 40 205 b 127 227 40 205 80 215 b 220 249 196 244 213 249 b 227 247 224 249 225 248 b 238 237 231 245 235 241 l 239 233 l 239 -106 l 239 -448 l 238 -451 b 219 -463 234 -459 225 -463 b 198 -451 210 -463 202 -459 l 197 -448 l 197 -324 b 197 -201 197 -248 197 -201 b 110 -223 196 -201 157 -210 b 17 -245 42 -240 24 -245 b 10 -242 13 -245 13 -244 b 0 -233 6 -241 2 -237 l 0 -230 l 0 108 l 0 446 l 0 449 b 10 460 2 453 6 458 m 197 22 b 197 70 197 41 197 58 b 196 116 197 113 197 116 l 196 116 b 118 97 196 116 160 106 l 40 77 l 40 -18 b 40 -112 40 -69 40 -112 l 119 -93 l 197 -73 l 197 22 " },
    "v51": { "x_min": -1.359375, "x_max": 455.96875, "ha": 465, "o": "m 352 541 b 357 542 353 542 355 542 b 377 530 364 542 372 537 l 378 526 l 378 394 l 379 262 l 404 266 b 436 270 420 269 430 270 b 450 265 443 270 446 269 b 455 220 455 259 455 260 l 455 208 l 455 161 l 454 156 b 411 140 449 147 447 147 b 378 133 393 137 379 134 b 378 68 378 133 378 106 b 378 22 378 54 378 38 l 379 -87 l 404 -83 b 436 -79 420 -80 430 -79 b 450 -84 443 -79 446 -80 b 455 -129 455 -90 455 -88 l 455 -141 l 455 -188 l 454 -192 b 413 -209 449 -202 447 -202 b 382 -215 398 -212 383 -215 l 378 -215 l 378 -345 l 378 -380 b 375 -485 378 -484 378 -480 b 357 -494 371 -491 364 -494 b 340 -485 351 -494 344 -491 b 336 -383 337 -480 336 -484 l 336 -349 l 336 -223 l 334 -223 b 291 -231 334 -223 314 -227 l 247 -240 l 247 -371 l 246 -503 l 245 -506 b 225 -519 242 -514 234 -519 b 206 -506 219 -519 210 -514 l 205 -503 l 205 -376 l 205 -248 l 160 -256 l 115 -265 l 115 -396 l 115 -527 l 114 -531 b 95 -544 110 -539 102 -544 b 76 -531 87 -544 78 -539 l 73 -527 l 73 -399 b 73 -273 73 -330 73 -273 b 49 -277 73 -273 61 -274 b 17 -281 32 -280 24 -281 b 4 -276 10 -281 8 -280 b -1 -234 0 -269 -1 -272 b 0 -219 -1 -229 0 -224 l 0 -170 l 1 -167 b 10 -158 2 -163 6 -159 b 49 -149 13 -156 16 -155 l 73 -145 l 73 -34 b 73 76 73 26 73 76 b 49 72 73 76 61 74 b 17 68 32 69 24 68 b 4 73 10 68 8 69 b -1 115 0 80 -1 77 b 0 130 -1 120 0 124 l 0 179 l 1 181 b 10 191 2 186 6 190 b 49 199 13 192 16 194 l 73 204 l 73 338 b 73 374 73 352 73 365 b 77 483 73 484 73 477 b 95 492 81 489 88 492 b 111 483 100 492 107 489 b 115 378 115 477 115 483 l 115 342 b 117 212 115 223 115 212 b 204 229 117 212 200 227 l 205 229 l 205 365 l 205 502 l 206 505 b 225 517 210 513 219 517 b 245 505 234 517 242 513 l 246 502 l 247 369 l 247 237 l 249 237 b 336 254 253 238 336 254 b 337 390 336 254 337 302 l 337 526 l 338 530 b 352 541 341 535 347 539 m 336 15 b 336 126 336 102 336 126 l 336 126 b 291 117 336 126 315 122 l 247 109 l 247 -1 l 247 -112 l 249 -112 b 336 -95 253 -111 336 -95 b 336 15 336 -95 336 -56 m 205 -120 b 205 -55 205 -120 205 -93 b 205 -9 205 -41 205 -24 l 205 101 l 160 93 l 115 84 l 115 -26 b 115 -83 115 -49 115 -69 b 117 -137 115 -133 115 -137 b 205 -120 118 -137 204 -120 " },
    "v52": { "x_min": -10.890625, "x_max": 298.078125, "ha": 294, "o": "m 138 473 b 142 474 140 473 141 474 b 164 459 148 474 153 470 b 191 402 183 442 191 423 b 181 353 191 388 187 371 b 178 349 179 352 178 349 b 179 348 178 348 179 348 b 185 349 181 348 182 348 b 255 376 210 355 234 363 b 272 381 264 381 266 381 b 298 355 287 381 298 370 b 288 330 298 348 298 345 b 171 34 238 254 194 141 b 166 13 168 16 168 16 b 144 1 161 5 152 1 b 121 15 134 1 125 5 b 115 33 119 18 117 24 b 0 330 91 145 49 252 b -10 355 -9 345 -10 348 b 13 381 -10 371 0 381 b 31 376 19 381 25 380 b 132 345 61 358 103 345 l 136 345 l 137 355 b 145 378 138 359 142 370 b 152 415 149 394 152 405 b 137 452 152 427 148 438 b 133 464 134 458 133 460 b 138 473 133 467 134 470 " },
    "v53": { "x_min": 0, "x_max": 902.421875, "ha": 921, "o": "m 17 240 b 24 241 19 241 21 241 b 32 240 28 241 31 241 b 46 229 38 238 43 234 b 50 88 50 223 50 237 b 50 -1 50 63 50 34 b 50 -90 50 -36 50 -65 b 46 -231 50 -238 50 -224 b 25 -242 42 -238 34 -242 b 0 -224 14 -242 4 -235 b 0 2 0 -222 0 -108 b 0 223 0 112 0 220 b 17 240 2 230 9 237 m 110 240 b 118 241 111 241 114 241 b 126 240 121 241 123 241 b 142 223 133 237 140 230 b 144 123 144 220 144 205 b 144 29 144 45 144 29 b 144 29 144 29 144 29 b 393 183 166 106 264 167 b 450 186 412 184 431 186 b 756 29 600 186 732 120 b 756 29 756 29 756 29 b 758 123 758 29 758 45 b 760 227 758 226 758 223 b 784 241 766 237 774 241 b 804 229 792 241 800 237 b 809 88 808 223 809 237 l 809 -1 l 809 -90 b 804 -231 809 -238 808 -224 b 784 -242 800 -238 792 -242 b 762 -231 775 -242 766 -238 b 758 -124 756 -224 758 -231 b 756 -30 758 -47 758 -30 b 756 -30 756 -30 756 -30 b 509 -184 736 -108 637 -169 b 450 -187 488 -187 469 -187 b 144 -30 300 -187 168 -122 b 144 -30 144 -30 144 -30 b 144 -124 144 -30 144 -47 b 140 -231 144 -231 144 -224 b 118 -242 134 -238 126 -242 b 92 -224 107 -242 96 -235 b 92 2 92 -222 92 -108 b 92 223 92 112 92 220 b 110 240 95 230 102 237 m 432 161 b 413 162 426 162 420 162 b 313 41 351 162 313 109 b 347 -73 313 5 323 -34 b 487 -163 385 -133 439 -163 b 578 -97 526 -163 562 -142 b 588 -43 585 -80 588 -62 b 432 161 588 47 518 147 m 868 240 b 876 241 869 241 872 241 b 884 240 879 241 882 241 b 898 229 890 238 894 234 b 902 88 902 223 902 237 l 902 -1 l 902 -90 b 898 -231 902 -238 902 -224 b 876 -242 892 -238 884 -242 b 852 -224 865 -242 854 -235 b 850 2 850 -222 850 -108 b 852 223 850 112 850 220 b 868 240 853 230 860 237 " },
    "v54": { "x_min": -24.5, "x_max": 317.140625, "ha": 324, "o": "m -24 -161 l -24 -5 l -20 -5 b 0 -24 -9 -5 -2 -12 b 171 -315 21 -124 84 -233 b 317 -660 268 -406 317 -531 b 187 -1014 317 -782 274 -909 b 161 -1034 172 -1034 171 -1034 b 141 -1013 149 -1034 141 -1025 b 152 -991 141 -1004 142 -1002 b 266 -682 228 -899 266 -788 b 174 -430 266 -588 236 -498 b -23 -317 136 -388 66 -348 b -24 -161 -23 -316 -24 -285 " },
    "v55": { "x_min": 0, "x_max": 551.25, "ha": 563, "o": "m 289 644 b 304 645 294 645 299 645 b 404 566 349 645 392 613 b 406 541 405 557 406 549 b 379 471 406 514 397 489 l 377 467 l 382 470 b 509 591 438 485 485 531 b 513 601 510 595 513 599 b 530 609 518 607 524 609 b 551 588 540 609 551 602 b 200 -605 551 584 204 -599 b 182 -616 197 -612 190 -616 b 163 -602 174 -616 166 -610 b 161 -598 161 -601 161 -601 b 217 -402 161 -589 170 -562 b 272 -213 247 -298 272 -213 b 272 -213 272 -213 272 -213 b 264 -219 272 -213 268 -216 b 140 -262 227 -247 182 -262 b 36 -226 102 -262 65 -249 b 0 -145 12 -206 0 -176 b 17 -84 0 -124 5 -104 b 103 -38 38 -54 70 -38 b 191 -91 137 -38 172 -56 b 205 -141 201 -106 205 -124 b 178 -212 205 -167 196 -194 l 175 -215 l 182 -213 b 307 -93 236 -198 284 -151 b 372 129 308 -88 372 127 b 372 129 372 129 372 129 b 364 122 372 129 368 126 b 240 80 328 94 283 80 b 137 115 202 80 166 91 b 99 195 112 136 99 165 b 118 256 99 217 106 238 b 204 303 138 287 171 303 b 292 249 238 303 273 285 b 306 199 302 234 306 217 b 279 129 306 173 296 148 l 276 126 l 281 127 b 408 248 336 142 385 190 b 473 470 409 254 473 469 b 473 470 473 470 473 470 b 465 464 473 470 469 467 b 341 421 428 435 383 421 b 236 458 303 421 266 433 b 200 537 212 478 200 508 b 289 644 200 585 234 635 " },
    "v58": { "x_min": -21.78125, "x_max": 367.5, "ha": 375, "o": "m 259 1553 b 265 1553 261 1553 264 1553 b 288 1540 272 1553 277 1550 b 367 1351 340 1493 367 1424 b 336 1221 367 1308 357 1263 l 332 1211 l 333 1208 b 367 1077 356 1170 367 1124 b 336 945 367 1032 357 986 l 332 935 l 333 932 b 367 800 356 893 367 848 b 336 669 367 756 357 710 l 332 659 l 333 656 b 367 523 356 617 367 571 b 345 412 367 485 360 446 b 231 273 322 356 284 310 b -1 19 121 195 27 93 b -17 4 -4 11 -10 5 l -21 4 l -21 134 l -21 265 l -17 265 b 133 291 20 265 96 278 b 318 537 245 328 318 433 b 307 603 318 559 315 582 b 303 614 304 612 304 614 b 298 609 302 614 300 613 b 231 549 281 589 258 567 b -1 295 121 471 27 369 b -17 280 -4 287 -10 281 l -21 280 l -21 410 l -21 541 l -17 541 b 133 567 20 541 96 555 b 318 813 245 605 318 709 b 307 880 318 835 315 859 b 303 891 304 888 304 891 b 298 885 302 891 300 888 b 231 825 281 866 258 843 b -1 571 121 748 27 645 b -17 556 -4 563 -10 557 l -21 556 l -21 687 l -21 817 l -17 817 b 133 843 20 817 96 830 b 318 1089 245 881 318 985 b 307 1156 318 1111 315 1134 b 303 1167 304 1164 304 1167 b 298 1161 302 1167 300 1164 b 231 1102 281 1140 258 1120 b -1 848 121 1024 27 921 b -17 832 -4 839 -10 834 l -21 832 l -21 963 l -21 1093 l -17 1093 b 114 1113 12 1093 78 1103 b 313 1314 215 1142 289 1218 b 318 1364 317 1331 318 1347 b 255 1511 318 1422 295 1478 b 243 1532 247 1519 243 1525 b 259 1553 243 1540 250 1550 " },
    "v59": { "x_min": 0, "x_max": 464.140625, "ha": 474, "o": "m 0 0 l 0 347 l 76 347 l 153 347 l 153 0 l 153 -348 l 76 -348 l 0 -348 l 0 0 m 308 -1 l 308 347 l 386 347 l 464 347 l 464 -1 l 464 -348 l 386 -348 l 308 -348 l 308 -1 " },
    "v5a": { "x_min": -171.5, "x_max": 170.140625, "ha": 174, "o": "m -6 566 b 0 567 -5 567 -2 567 b 14 556 6 567 12 563 b 92 285 14 555 50 433 b 170 13 166 33 170 19 b 168 13 170 13 170 13 b 161 1 168 8 167 4 l 159 0 l 122 0 l 84 0 l 81 1 b 21 195 76 5 78 -5 b -32 381 -8 297 -32 381 b -87 197 -32 381 -57 298 b -141 8 -115 94 -140 9 b -155 0 -142 2 -149 0 b -171 15 -163 0 -171 5 b -14 556 -171 18 -24 528 b -6 566 -14 560 -10 564 " },
    "v5b": { "x_min": -441, "x_max": 439.640625, "ha": 449, "o": "m -428 -2 b -421 0 -427 -1 -424 0 b -406 -6 -416 0 -409 -2 b -400 -31 -401 -12 -400 -15 b -1 -352 -392 -215 -215 -352 b 58 -349 19 -352 38 -351 b 398 -31 250 -326 392 -192 b 404 -6 398 -15 400 -12 b 419 -1 408 -2 413 -1 b 439 -13 427 -1 435 -5 b 439 -29 439 -16 439 -22 b 434 -105 439 -48 438 -80 b 0 -489 397 -333 213 -489 b -68 -484 -23 -489 -44 -488 b -441 -36 -280 -452 -436 -263 b -441 -30 -441 -34 -441 -31 b -428 -2 -441 -11 -439 -5 m -13 -9 b -1 -8 -9 -8 -5 -8 b 50 -36 19 -8 39 -19 b 61 -72 57 -47 61 -59 b 50 -106 61 -84 57 -97 b -1 -134 39 -124 19 -134 b -46 -115 -17 -134 -34 -129 b -62 -72 -57 -102 -62 -87 b -13 -9 -62 -44 -44 -16 " },
    "v5c": { "x_min": 0, "x_max": 447.8125, "ha": 457, "o": "m 0 -87 l 0 0 l 223 0 l 447 0 l 447 -87 l 447 -174 l 223 -174 l 0 -174 l 0 -87 " },
    "v5d": { "x_min": -1.359375, "x_max": 592.078125, "ha": 604, "o": "m 280 692 b 295 694 283 692 289 694 b 310 692 300 694 307 692 b 357 630 340 684 357 657 b 336 580 357 612 351 594 b 311 538 321 566 311 549 b 352 492 311 512 330 492 b 366 495 357 492 362 492 b 397 553 390 503 397 517 b 415 603 397 576 402 591 b 460 623 427 617 443 623 b 509 599 479 623 498 614 b 522 559 518 587 522 573 b 494 506 522 538 513 519 b 451 495 481 498 473 496 b 415 488 432 495 426 494 b 394 449 404 483 394 464 b 394 448 394 448 394 448 l 394 440 l 397 433 b 428 409 404 420 413 413 b 438 408 431 408 435 408 b 479 431 450 408 462 415 b 528 455 495 448 510 455 b 548 452 534 455 541 453 b 592 391 577 442 592 416 b 549 331 592 365 577 340 b 528 327 541 328 534 327 b 479 351 510 327 495 335 b 438 374 464 367 450 374 b 417 369 431 374 424 373 b 394 333 402 360 394 348 b 400 312 394 326 396 319 b 451 287 408 294 420 288 b 513 258 484 285 499 278 b 522 223 519 247 522 234 b 461 159 522 190 496 159 b 449 161 457 159 453 159 b 397 229 416 167 397 191 b 366 288 397 265 390 278 b 352 290 362 290 357 290 b 315 262 336 290 321 280 b 311 245 313 256 311 251 b 334 204 311 233 318 220 b 355 170 348 190 351 184 b 357 152 356 166 357 159 b 355 136 357 147 356 140 b 295 88 345 104 321 88 b 232 152 264 88 232 112 b 255 204 232 174 238 186 b 279 244 273 222 279 231 l 279 245 b 238 290 279 270 259 290 b 224 288 234 290 228 290 b 193 229 200 278 193 265 b 141 161 193 191 174 167 b 129 159 137 159 133 159 b 68 223 93 159 68 190 b 77 258 68 234 70 247 b 138 287 91 278 106 285 b 185 302 166 287 175 291 b 196 333 193 312 196 323 b 174 369 196 347 187 360 b 152 374 166 373 159 374 b 111 351 140 374 126 367 b 62 327 95 335 80 327 b 51 328 58 327 54 327 b -1 391 16 334 -1 363 b 53 455 -1 420 17 449 b 62 455 57 455 59 455 b 111 431 80 455 95 448 b 152 408 127 415 140 408 b 161 409 155 408 159 408 b 193 433 176 413 186 420 l 196 440 l 196 448 b 196 451 196 449 196 449 b 190 471 196 459 194 463 b 137 495 182 489 167 495 l 134 495 l 134 495 b 68 560 95 495 68 521 b 129 623 68 596 95 623 b 144 621 134 623 138 623 b 193 553 175 614 193 589 b 224 495 193 517 200 503 b 238 492 228 492 234 492 b 279 538 259 492 279 512 b 254 580 279 549 269 566 b 232 630 239 594 232 612 b 280 692 232 657 250 684 m 307 456 b 295 458 303 458 299 458 b 230 391 258 458 230 426 b 236 360 230 381 231 371 b 295 324 249 337 272 324 b 353 360 318 324 341 337 b 360 391 357 370 360 381 b 307 456 360 421 340 451 " },
    "v60": { "x_min": -590.71875, "x_max": 589.359375, "ha": 601, "o": "m -367 173 b -362 174 -366 174 -364 174 b -351 173 -357 174 -353 173 b -262 86 -348 172 -328 151 b -176 0 -216 37 -176 0 b -107 84 -176 0 -145 37 b -31 174 -36 173 -38 172 b -25 174 -29 174 -28 174 b -16 173 -23 174 -19 173 b 72 86 -13 172 6 151 b 157 0 119 37 157 0 b 227 84 159 0 189 37 b 303 174 298 173 296 172 b 308 174 304 174 307 174 b 318 173 313 174 317 173 b 481 11 322 172 357 134 l 494 -1 l 522 34 b 560 76 553 72 555 74 b 567 77 563 77 564 77 b 589 56 579 77 589 68 b 586 48 589 54 588 51 b 411 -172 583 41 416 -166 b 397 -176 406 -174 401 -176 b 387 -174 393 -176 390 -176 b 299 -87 386 -173 366 -152 b 213 0 253 -38 213 0 b 144 -86 213 0 182 -38 b 68 -174 73 -174 74 -173 b 62 -176 66 -176 65 -176 b 53 -174 59 -176 55 -174 b -35 -87 50 -173 29 -152 b -121 0 -83 -38 -121 0 b -190 -86 -122 0 -152 -38 b -266 -174 -261 -174 -259 -173 b -272 -176 -268 -176 -270 -176 b -281 -174 -276 -176 -280 -174 b -371 -86 -284 -173 -304 -152 b -457 0 -417 -38 -457 0 l -457 0 b -477 -26 -457 0 -470 -16 b -548 -227 -524 -88 -548 -161 b -536 -303 -548 -254 -544 -280 b -533 -317 -534 -309 -533 -313 b -553 -338 -533 -330 -541 -338 b -577 -315 -566 -338 -571 -333 b -590 -227 -586 -287 -590 -258 b -518 -9 -590 -154 -564 -77 b -465 56 -509 2 -504 8 l -402 134 b -367 173 -375 169 -372 172 " },
    "v62": { "x_min": 46.28125, "x_max": 669.671875, "ha": 563, "o": "m 183 376 b 189 376 185 376 187 376 b 212 374 197 376 208 376 b 265 337 234 369 253 355 b 274 317 268 331 273 320 b 274 316 274 317 274 316 b 280 323 276 316 276 319 b 311 358 288 337 299 348 b 319 366 315 360 318 365 b 356 376 326 373 340 376 b 382 371 364 376 374 374 b 428 337 400 366 417 352 b 436 317 431 331 436 320 b 438 316 436 317 436 316 b 442 323 438 316 439 319 b 475 358 451 337 462 348 b 483 366 477 360 481 365 b 518 376 488 373 503 376 b 544 373 528 376 536 376 b 604 285 579 360 604 326 b 597 249 604 273 601 258 b 543 63 596 247 544 70 b 541 54 543 61 541 55 b 540 44 540 51 540 47 b 552 23 540 33 545 23 b 552 23 552 23 552 23 b 647 126 586 29 627 72 b 658 138 651 136 653 138 b 660 138 660 138 660 138 b 669 129 666 137 669 136 b 654 88 669 122 665 109 b 562 -12 631 43 602 9 l 549 -19 b 521 -27 540 -24 530 -27 b 447 30 490 -27 458 -4 b 443 58 445 38 443 48 b 450 93 443 72 446 84 b 504 278 453 97 504 272 b 507 288 506 283 506 287 b 509 298 507 292 509 295 b 491 326 509 310 502 320 b 487 327 490 327 488 327 b 479 324 484 327 483 326 b 441 270 462 316 443 288 b 435 249 441 265 436 254 b 398 127 434 248 419 195 b 362 4 379 61 362 5 b 328 -1 359 -1 362 -1 b 314 -1 323 -1 319 -1 b 302 -1 310 -1 306 -1 b 266 4 266 -1 269 -1 b 265 6 265 5 265 5 b 303 144 265 13 272 34 b 343 278 325 216 343 276 b 344 288 343 281 344 285 b 345 298 345 291 345 295 b 330 326 345 310 340 320 b 323 327 328 327 325 327 b 317 324 322 327 321 326 b 279 270 300 316 281 288 b 273 249 279 265 274 254 b 236 127 272 248 255 195 b 200 4 216 61 200 5 b 164 -1 197 -1 198 -1 b 151 -1 161 -1 156 -1 b 140 -1 147 -1 142 -1 b 103 4 104 -1 106 -1 b 103 6 103 5 103 5 b 141 144 103 13 108 34 b 181 278 161 216 179 276 b 182 288 181 281 181 285 b 183 298 182 291 183 295 b 168 324 183 310 178 320 b 160 327 166 326 163 327 b 141 320 156 327 151 324 b 69 230 112 305 85 272 b 57 215 65 217 62 215 b 55 215 57 215 55 215 b 46 224 49 215 46 217 b 59 260 46 231 50 242 b 151 363 81 306 112 341 b 161 369 155 365 160 367 b 183 376 166 371 174 374 " },
    "v68": { "x_min": -597.53125, "x_max": 596.171875, "ha": 608, "o": "m -533 324 b -525 327 -530 326 -528 327 b -504 305 -514 327 -504 317 b -504 305 -504 305 -504 305 b -513 284 -504 299 -504 299 b -556 112 -541 226 -556 167 b -545 33 -556 84 -552 58 b -524 -20 -541 15 -532 -9 l -522 -23 l -491 15 l -413 111 b -355 174 -367 169 -363 174 b -351 174 -353 174 -352 174 b -254 86 -343 174 -348 179 b -168 -1 -208 37 -168 -1 b -100 84 -168 -1 -137 37 b -23 173 -28 173 -29 172 b -19 174 -21 174 -20 174 b -8 173 -14 174 -10 173 b 155 11 -5 172 43 123 l 166 -1 l 168 1 l 170 4 l 170 130 b 171 260 170 256 170 258 b 191 274 175 269 183 274 b 205 267 196 274 201 272 b 212 158 212 262 210 273 l 212 56 l 257 112 b 311 173 304 172 304 172 b 317 174 313 174 314 174 b 326 173 319 174 323 173 b 490 11 329 172 366 134 l 502 -1 l 530 34 b 568 76 560 72 563 74 b 575 77 570 77 573 77 b 596 56 586 77 596 68 b 594 48 596 54 596 51 b 417 -172 592 41 424 -166 b 405 -176 415 -174 409 -176 b 396 -174 401 -176 398 -176 b 307 -87 393 -173 372 -152 b 221 -1 259 -38 221 -1 b 216 -6 221 -1 219 -2 l 212 -12 l 212 -147 b 212 -210 212 -173 212 -194 b 205 -292 212 -297 210 -287 b 191 -299 201 -297 196 -299 b 172 -287 183 -299 175 -295 b 170 -174 171 -284 171 -284 l 170 -63 l 127 -117 b 73 -176 84 -170 80 -176 b 68 -176 72 -176 70 -176 b -27 -87 59 -174 65 -180 b -114 0 -74 -38 -112 0 b -182 -86 -114 0 -145 -38 b -258 -174 -253 -174 -253 -173 b -264 -176 -259 -176 -262 -176 b -274 -174 -268 -176 -272 -174 b -438 -11 -277 -173 -348 -102 l -449 0 l -479 -37 b -524 -80 -513 -80 -514 -80 l -524 -80 b -553 -52 -534 -80 -540 -74 b -597 109 -583 -8 -597 48 b -560 280 -597 165 -585 224 b -533 324 -548 310 -540 322 " },
    "v6c": { "x_min": -1.359375, "x_max": 193.28125, "ha": 197, "o": "m 78 233 b 87 233 81 233 84 233 b 187 140 132 233 174 195 b 193 102 190 127 193 115 b 43 -113 193 22 136 -62 b 27 -119 36 -116 31 -119 b 19 -108 21 -119 19 -115 b 29 -97 19 -102 20 -101 b 102 13 73 -72 102 -27 b 92 51 102 26 98 40 l 91 54 l 84 54 b 8 104 53 54 21 74 b -1 142 1 116 -1 130 b 78 233 -1 187 31 227 " },
    "v6d": { "x_min": -590.71875, "x_max": 589.359375, "ha": 601, "o": "m 544 335 b 553 337 548 337 551 337 b 575 313 563 337 570 330 b 589 226 583 285 589 256 b 517 8 589 152 563 76 b 464 -58 507 -4 503 -9 l 401 -136 b 362 -176 372 -172 370 -176 b 357 -176 360 -176 359 -176 b 261 -87 349 -174 355 -180 b 175 0 215 -38 175 0 b 106 -86 175 0 144 -38 b 29 -174 35 -174 36 -173 b 24 -176 28 -176 27 -176 b 14 -174 21 -176 17 -174 b -73 -87 12 -173 -8 -152 b -159 0 -121 -38 -159 0 b -228 -86 -160 0 -190 -38 b -304 -174 -299 -174 -298 -173 b -310 -176 -306 -176 -308 -176 b -319 -174 -314 -176 -318 -174 b -483 -12 -323 -173 -359 -137 l -495 0 l -524 -34 b -562 -77 -553 -73 -556 -76 b -568 -79 -564 -79 -566 -79 b -590 -58 -581 -79 -590 -69 b -588 -49 -590 -55 -589 -52 b -412 170 -585 -43 -417 165 b -398 174 -408 173 -402 174 b -389 173 -394 174 -392 174 b -300 86 -387 172 -366 151 b -215 -1 -254 37 -215 -1 b -145 84 -215 -1 -183 37 b -69 173 -74 173 -76 172 b -63 174 -68 174 -66 174 b -54 173 -61 174 -57 173 b 34 86 -51 172 -31 151 b 119 -1 81 37 119 -1 b 189 84 121 -1 151 37 b 265 173 259 173 258 172 b 270 174 266 174 269 174 b 280 173 274 174 279 173 b 370 84 283 172 303 151 b 455 -1 416 37 455 -1 l 455 -1 b 476 24 455 -1 469 15 b 547 226 522 87 547 159 b 534 302 547 252 543 278 b 532 317 533 308 532 313 b 544 335 532 326 536 333 " },
    "v6f": { "x_min": -80.3125, "x_max": 78.9375, "ha": 81, "o": "m 63 191 b 69 192 65 192 66 192 b 77 188 72 192 76 191 b 78 183 78 187 78 186 b 74 158 78 179 77 172 l 66 115 b 9 -161 49 30 10 -158 b -10 -187 6 -172 -1 -181 b -34 -194 -17 -191 -25 -194 b -80 -147 -58 -194 -80 -174 b -80 -141 -80 -144 -80 -142 b 9 70 -80 -134 -73 -117 l 49 163 b 63 191 59 188 61 190 " },
    "v70": { "x_min": 0, "x_max": 436.921875, "ha": 446, "o": "m 213 190 b 217 191 215 191 216 191 b 231 184 223 191 228 188 b 249 154 240 167 246 159 b 419 18 292 91 348 45 b 436 -1 435 11 436 8 b 424 -16 436 -9 434 -13 b 308 -87 394 -26 340 -59 b 231 -186 276 -117 257 -142 b 219 -192 228 -191 225 -192 b 198 -174 209 -192 208 -191 b 47 -33 161 -113 110 -63 b 10 -16 34 -26 17 -19 b 0 -1 2 -13 0 -9 b 17 18 0 8 1 11 b 198 173 95 48 156 101 b 213 190 206 187 208 188 " },
    "v72": { "x_min": -423.3125, "x_max": 421.9375, "ha": 431, "o": "m -262 197 b -247 197 -257 197 -253 197 b -118 162 -210 197 -163 184 b 40 45 -61 134 -13 98 b 277 -95 119 -33 200 -81 b 289 -97 281 -97 285 -97 b 378 0 332 -97 371 -55 b 378 11 378 4 378 6 b 302 83 378 55 345 83 b 242 66 283 83 262 77 b 208 56 231 59 219 56 b 148 120 175 56 148 81 b 201 186 148 151 164 172 b 261 198 220 194 240 198 b 420 45 341 198 411 136 b 421 22 421 37 421 29 b 245 -199 421 -93 338 -199 b 238 -198 243 -199 240 -199 b -44 -47 148 -194 50 -141 b -250 86 -114 22 -183 66 b -295 94 -270 91 -283 94 b -315 91 -302 94 -307 94 b -381 4 -356 81 -381 43 b -355 -56 -381 -18 -372 -40 b -298 -81 -338 -73 -319 -81 b -246 -68 -283 -81 -265 -77 b -212 -58 -234 -61 -223 -58 b -178 -69 -200 -58 -189 -62 b -151 -122 -160 -81 -151 -101 b -171 -167 -151 -138 -157 -155 b -239 -195 -185 -181 -213 -192 b -257 -197 -245 -197 -250 -197 b -423 -5 -352 -197 -423 -109 b -412 65 -423 16 -419 40 b -262 197 -389 137 -329 188 " },
    "v74": { "x_min": -206.890625, "x_max": 428.75, "ha": 438, "o": "m 389 -351 b 394 -351 390 -351 393 -351 b 428 -385 413 -351 428 -367 b 428 -394 428 -388 428 -391 b 394 -428 426 -406 421 -410 l 332 -473 l 269 -516 l 205 -560 l 141 -603 l 77 -648 l 13 -692 l -50 -737 l -114 -780 l -145 -802 b -171 -813 -157 -810 -163 -813 b -175 -813 -172 -813 -174 -813 b -206 -777 -194 -811 -206 -795 b -202 -760 -206 -771 -205 -766 b -87 -675 -197 -752 -206 -757 l -34 -639 l 83 -557 l 145 -514 l 209 -470 l 272 -427 b 389 -351 375 -356 381 -352 " },
    "v75": { "x_min": -149.71875, "x_max": 148.359375, "ha": 151, "o": "m -137 381 b -130 383 -134 383 -133 383 b -111 371 -122 383 -114 378 b -55 224 -110 370 -85 305 b 0 80 -25 145 -1 80 b 54 224 0 80 24 145 b 112 377 114 384 110 373 b 127 384 118 381 122 384 b 148 362 138 384 148 374 l 148 356 l 83 183 b 16 9 47 88 17 11 b -1 0 12 2 5 0 b -14 5 -5 0 -10 1 b -84 183 -19 9 -13 -6 l -149 356 l -149 362 b -137 381 -149 371 -145 378 " },
    "v78": { "x_min": 0, "x_max": 193.28125, "ha": 197, "o": "m 85 514 b 95 517 88 517 89 517 b 114 505 103 517 110 513 l 115 502 l 115 376 b 115 249 115 306 115 249 b 141 258 117 249 127 252 l 167 266 l 172 266 b 190 254 181 265 187 262 l 193 251 l 193 202 l 193 188 b 187 147 193 149 191 152 b 147 130 183 142 182 141 l 115 119 l 115 9 b 115 -99 115 -51 115 -99 b 141 -91 115 -99 127 -95 b 171 -81 166 -81 167 -81 l 171 -81 b 191 -94 181 -81 189 -87 b 193 -142 191 -97 193 -120 b 191 -195 193 -167 191 -194 b 125 -227 187 -205 187 -204 l 115 -230 l 115 -366 l 115 -503 l 114 -506 b 95 -519 110 -514 102 -519 b 74 -506 87 -519 78 -514 l 73 -503 l 73 -374 b 73 -245 73 -260 73 -245 b 73 -245 73 -245 73 -245 b 55 -252 72 -245 63 -249 l 32 -260 b 19 -263 27 -262 23 -263 b 4 -256 13 -263 8 -260 b 0 -215 0 -251 0 -254 b 0 -199 0 -210 0 -206 l 0 -152 l 1 -149 b 8 -140 2 -145 5 -141 b 42 -127 9 -140 24 -133 l 73 -116 l 73 -5 b 73 23 73 4 73 15 b 73 105 73 70 73 105 b 49 97 73 105 61 101 b 17 88 32 91 23 88 b 4 95 10 88 8 91 b 0 137 0 101 0 98 b 0 151 0 141 0 145 l 0 199 l 1 202 b 43 224 5 212 5 212 l 73 234 l 73 367 l 73 502 l 74 505 b 85 514 77 509 81 513 " },
    "v79": { "x_min": -1.359375, "x_max": 899.703125, "ha": 918, "o": "m 307 349 b 332 351 315 351 323 351 b 443 340 367 351 408 347 b 741 47 607 306 720 195 b 744 0 743 31 744 16 b 660 -303 744 -90 713 -206 b 28 -755 534 -531 304 -695 b 14 -756 23 -755 19 -756 b -1 -741 4 -756 -1 -750 b 21 -720 -1 -731 1 -728 b 567 -56 337 -601 548 -344 b 568 -11 568 -41 568 -24 b 442 285 568 129 525 233 b 325 319 406 308 367 319 b 93 177 232 319 137 266 b 84 154 91 170 84 155 b 84 154 84 154 84 154 b 88 156 84 154 85 155 b 159 177 110 170 134 177 b 257 134 194 177 231 162 b 294 41 281 108 294 73 b 171 -97 294 -24 246 -90 b 156 -98 166 -97 161 -98 b 6 74 73 -98 6 -22 b 6 80 6 76 6 79 b 307 349 10 223 141 340 m 839 215 b 845 216 841 216 842 216 b 862 213 852 216 860 215 b 899 163 887 206 899 184 b 872 117 899 145 890 127 b 847 111 865 112 856 111 b 808 130 833 111 818 117 b 796 162 800 140 796 151 b 839 215 796 187 812 212 m 839 -112 b 845 -112 841 -112 842 -112 b 862 -115 852 -112 860 -113 b 899 -165 887 -122 899 -144 b 872 -210 899 -183 890 -201 b 847 -217 865 -215 856 -217 b 808 -198 833 -217 818 -210 b 796 -165 800 -188 796 -177 b 839 -112 796 -140 812 -116 " },
    "v7a": { "x_min": -1.359375, "x_max": 386.5625, "ha": 394, "o": "m 249 535 b 257 537 251 537 253 537 b 276 524 266 537 273 533 l 277 521 l 279 419 l 279 316 l 304 323 b 337 328 319 326 330 328 b 353 316 347 328 349 324 b 355 266 355 315 355 290 b 353 215 355 241 355 217 b 319 198 349 206 347 205 b 279 187 284 190 279 188 b 279 156 279 187 279 174 b 279 136 279 151 279 144 l 279 84 l 289 87 l 330 98 b 367 105 352 102 362 105 b 378 101 372 105 375 104 b 386 61 385 95 386 94 b 386 40 386 55 386 48 l 386 -5 l 385 -8 b 374 -19 383 -12 378 -18 b 291 -40 372 -19 347 -26 b 279 -43 284 -41 279 -43 b 279 -83 279 -43 279 -59 b 279 -95 279 -87 279 -91 l 279 -145 l 304 -140 b 337 -133 321 -136 330 -133 b 349 -140 343 -133 347 -136 b 355 -181 355 -145 355 -142 l 355 -197 l 355 -210 b 349 -252 355 -249 355 -247 b 300 -269 345 -258 347 -258 b 280 -274 291 -272 281 -273 l 279 -274 l 277 -378 l 277 -483 l 276 -487 b 257 -499 273 -495 265 -499 b 238 -487 249 -499 242 -495 l 236 -483 l 236 -384 l 236 -285 l 235 -285 l 212 -291 l 170 -301 b 148 -308 159 -305 148 -306 b 147 -415 147 -308 147 -313 l 147 -523 l 145 -526 b 126 -538 141 -534 133 -538 b 106 -526 118 -538 110 -534 l 104 -523 l 104 -420 b 103 -317 104 -326 104 -317 b 103 -317 103 -317 103 -317 b 50 -330 92 -322 54 -330 b 31 -317 42 -330 35 -326 b 29 -267 29 -315 29 -315 l 29 -219 l 32 -216 b 92 -192 36 -206 36 -206 l 104 -190 l 104 -138 b 103 -87 104 -91 104 -87 b 103 -87 103 -87 103 -87 b 88 -91 103 -87 96 -88 l 49 -101 b 17 -106 32 -105 23 -106 b 6 -102 13 -106 10 -105 b -1 -62 0 -97 -1 -95 b 0 -41 -1 -56 0 -49 l 0 4 l 1 6 b 10 16 2 11 6 15 b 91 37 12 18 38 24 l 104 41 l 104 93 b 103 144 104 140 104 144 b 103 144 103 144 103 144 b 50 131 92 141 54 131 b 31 144 42 131 35 137 b 29 195 29 147 29 148 l 29 242 l 32 245 b 92 269 36 255 36 255 l 104 273 l 104 377 l 104 481 l 106 485 b 126 498 110 492 118 498 b 134 495 129 498 132 496 b 145 485 138 494 142 489 l 147 481 l 147 383 l 147 283 l 152 284 b 190 294 155 285 171 290 l 230 303 l 236 305 l 236 413 l 236 521 l 238 524 b 249 535 240 528 243 533 m 236 126 b 235 177 236 154 236 177 l 235 177 b 213 172 235 177 225 174 l 170 161 b 147 155 157 158 147 155 b 147 124 147 155 147 142 b 147 102 147 117 147 111 l 147 52 l 153 54 l 228 72 l 236 74 l 236 126 m 236 -105 b 235 -54 236 -65 236 -54 l 235 -54 b 231 -55 235 -54 234 -54 b 172 -69 227 -55 204 -62 l 149 -76 l 147 -76 l 147 -127 l 147 -179 l 152 -177 b 190 -167 155 -177 171 -173 l 230 -158 l 236 -156 l 236 -105 " },
    "v7c": { "x_min": 0, "x_max": 300.8125, "ha": 307, "o": "m 49 505 b 53 506 50 505 51 506 b 70 496 58 506 62 503 b 81 485 73 492 78 488 l 96 473 l 111 459 l 122 449 l 134 438 l 182 396 l 255 330 b 292 291 292 298 292 298 l 292 290 l 292 284 l 283 270 b 209 36 234 197 209 113 b 288 -170 209 -44 235 -119 b 299 -184 295 -179 299 -181 b 300 -191 300 -187 300 -188 b 285 -206 300 -199 294 -206 b 280 -206 283 -206 281 -206 b 247 -201 270 -202 259 -201 b 176 -222 223 -201 197 -208 b 114 -340 136 -249 114 -292 b 172 -471 114 -384 134 -433 b 185 -492 182 -481 185 -487 b 181 -502 185 -496 183 -499 b 171 -508 176 -505 174 -508 b 152 -498 166 -508 160 -503 b 0 -284 65 -428 12 -352 b 0 -260 0 -278 0 -270 b 1 -238 0 -252 0 -242 b 148 -140 16 -177 73 -140 b 209 -148 167 -140 189 -142 b 215 -149 212 -148 215 -149 b 215 -149 215 -149 215 -149 l 215 -149 b 201 -136 215 -148 209 -142 l 157 -97 l 96 -41 b 17 34 21 24 17 29 b 17 37 17 36 17 36 b 17 38 17 37 17 38 b 25 56 17 44 17 44 b 110 298 81 131 110 219 b 46 474 110 367 88 431 b 38 491 40 480 38 487 b 49 505 38 498 42 502 " },
    "v7d": { "x_min": -1.359375, "x_max": 436.921875, "ha": 446, "o": "m 213 205 b 217 205 215 205 216 205 b 234 194 224 205 234 199 b 236 187 234 194 235 190 l 245 167 l 261 129 l 270 106 b 355 -61 294 54 329 -13 b 420 -163 381 -105 402 -138 b 436 -188 435 -184 436 -184 b 436 -191 436 -190 436 -190 b 421 -206 436 -201 431 -206 l 421 -206 l 416 -206 l 405 -201 b 217 -158 347 -172 283 -158 b 31 -201 153 -158 88 -172 l 20 -206 l 14 -206 l 14 -206 b 0 -191 5 -206 0 -201 b -1 -188 0 -190 -1 -190 b 14 -163 -1 -186 0 -184 b 95 -34 36 -136 72 -77 b 166 106 119 8 148 68 l 175 129 l 183 148 l 200 188 b 213 205 205 199 208 202 " },
    "v7f": { "x_min": 0, "x_max": 367.5, "ha": 375, "o": "m 0 124 l 0 187 l 61 187 l 122 187 l 122 138 l 122 91 l 153 61 l 183 30 l 213 61 l 243 91 l 243 138 l 243 187 l 306 187 l 367 187 l 367 124 l 367 61 l 321 61 l 274 61 l 243 30 l 213 0 l 243 -31 l 274 -62 l 321 -62 l 367 -62 l 367 -124 l 367 -188 l 306 -188 l 243 -188 l 243 -140 l 243 -93 l 213 -62 l 183 -31 l 153 -62 l 122 -93 l 122 -140 l 122 -188 l 61 -188 l 0 -188 l 0 -124 l 0 -62 l 46 -62 l 92 -62 l 123 -31 l 153 0 l 123 30 l 92 61 l 46 61 l 0 61 l 0 124 " },
    "v80": { "x_min": 29.9375, "x_max": 420.578125, "ha": 371, "o": "m 115 345 b 221 347 117 345 166 347 b 411 345 306 347 409 345 b 420 330 416 342 420 335 b 415 319 420 326 419 321 b 178 118 397 303 179 118 b 178 117 178 118 178 117 b 181 117 178 117 178 117 b 189 117 182 117 185 117 b 193 117 190 117 191 117 b 247 98 215 117 232 111 b 296 75 266 83 280 76 b 302 75 299 75 300 75 b 322 91 311 75 315 79 b 322 91 322 91 322 91 b 322 91 322 91 322 91 b 319 91 322 91 321 91 b 313 90 318 90 315 90 b 283 107 300 90 288 97 b 277 126 279 114 277 121 b 319 167 277 149 295 167 b 319 167 319 167 319 167 b 362 118 347 167 362 147 b 355 82 362 108 359 96 b 311 33 349 65 340 55 b 224 1 284 12 253 1 b 194 5 213 1 204 2 b 168 18 183 8 178 11 b 110 36 151 30 130 36 b 57 15 88 36 68 29 b 47 11 54 12 51 11 b 31 20 40 11 34 13 b 29 26 31 22 29 25 b 68 66 29 36 39 45 b 285 250 73 71 281 248 b 285 250 285 250 285 250 b 231 252 285 252 261 252 b 137 250 190 252 141 250 b 93 227 122 248 110 241 b 78 220 88 222 83 220 b 66 227 74 220 70 222 b 63 234 65 229 63 231 b 85 291 63 241 69 252 b 115 345 108 342 108 344 " },
    "v81": { "x_min": 0, "x_max": 428.75, "ha": 438, "o": "m 262 186 b 273 186 266 186 272 186 b 274 186 273 186 274 186 b 285 186 274 186 280 186 b 428 48 375 181 428 122 b 386 -68 428 12 416 -29 b 155 -187 329 -145 236 -187 b 12 -111 92 -187 38 -162 b 0 -51 4 -91 0 -72 b 262 186 0 58 122 179 m 366 131 b 352 134 362 133 357 134 b 219 81 321 134 269 115 b 47 -111 126 23 50 -62 b 47 -112 47 -111 47 -112 b 77 -136 47 -129 58 -136 b 264 -45 118 -136 194 -101 b 382 109 336 12 382 76 b 366 131 382 120 377 129 " },
    "v83": { "x_min": -1.359375, "x_max": 847.96875, "ha": 865, "o": "m 488 1499 b 495 1500 490 1500 492 1500 b 541 1465 507 1500 521 1490 b 679 1078 622 1372 679 1210 b 677 1050 679 1068 677 1060 b 477 642 668 893 604 764 l 443 609 l 431 596 l 431 592 l 438 562 l 449 508 l 460 458 b 481 355 475 390 481 355 b 481 355 481 355 481 355 b 490 356 481 355 485 355 b 528 358 495 356 511 358 b 558 356 540 358 552 356 b 839 95 699 338 808 237 b 847 22 845 72 847 47 b 631 -303 847 -113 766 -242 b 620 -309 623 -308 620 -309 l 620 -310 b 631 -359 620 -310 626 -333 l 646 -435 l 660 -496 b 672 -588 668 -535 672 -563 b 664 -653 672 -610 669 -630 b 383 -875 630 -792 509 -875 b 201 -810 321 -875 257 -855 b 129 -680 151 -768 129 -730 b 274 -530 129 -592 200 -530 b 351 -553 300 -530 326 -538 b 412 -669 393 -582 412 -626 b 287 -805 412 -735 366 -800 l 279 -805 l 285 -809 b 383 -830 318 -823 351 -830 b 586 -718 464 -830 540 -789 b 626 -584 612 -678 626 -631 b 619 -528 626 -566 623 -548 b 612 -495 619 -526 616 -510 b 577 -324 590 -387 577 -324 b 577 -324 577 -324 577 -324 b 568 -326 575 -324 571 -324 b 528 -334 558 -328 537 -333 b 465 -338 506 -337 485 -338 b 24 -11 269 -338 87 -206 b -1 145 8 41 -1 93 b 96 442 -1 249 32 351 b 322 714 166 541 236 626 l 352 745 l 345 782 l 332 843 l 315 921 b 303 984 310 950 304 978 b 295 1082 298 1017 295 1049 b 413 1426 295 1208 336 1329 b 488 1499 436 1456 477 1496 m 549 1301 b 541 1301 547 1301 544 1301 b 411 1207 500 1301 447 1263 b 355 1004 374 1152 355 1079 b 359 942 355 984 356 963 b 371 881 362 927 363 917 l 385 818 b 392 782 389 799 392 784 l 392 782 b 434 828 393 782 424 816 b 607 1165 534 941 594 1060 b 608 1193 608 1175 608 1183 b 597 1270 608 1224 604 1254 b 549 1301 589 1286 571 1299 m 398 528 b 393 555 396 542 393 553 b 392 555 393 555 393 555 b 317 470 390 555 347 505 b 190 298 266 408 212 334 b 127 70 148 227 127 148 b 155 -77 127 19 137 -30 b 468 -303 209 -216 333 -303 b 519 -299 484 -303 502 -302 b 568 -284 541 -295 568 -287 l 568 -284 b 563 -263 568 -284 566 -274 l 534 -120 l 511 -13 l 496 61 l 480 133 b 469 187 472 176 469 187 b 468 188 469 187 469 188 b 416 162 462 188 430 172 b 337 13 364 126 337 69 b 413 -124 337 -40 363 -93 b 428 -144 424 -131 428 -137 b 428 -149 428 -145 428 -148 b 409 -166 426 -161 419 -166 b 394 -162 405 -166 400 -165 b 240 77 302 -122 240 -27 l 240 77 b 430 342 240 197 315 301 l 436 344 l 426 394 l 398 528 m 548 194 b 526 195 540 195 532 195 b 519 195 524 195 521 195 l 514 195 l 518 177 l 539 79 l 552 15 l 566 -48 l 594 -187 l 605 -240 b 612 -266 609 -254 611 -266 b 612 -266 612 -266 612 -266 b 641 -248 613 -266 630 -256 b 744 -98 692 -212 730 -156 b 751 -40 749 -79 751 -59 b 548 194 751 76 665 181 " },
    "v84": { "x_min": 25.859375, "x_max": 164.6875, "ha": 168, "o": "m 34 369 b 40 370 35 370 38 370 b 59 353 49 370 50 367 b 164 40 122 254 155 158 b 164 0 164 33 164 16 b 164 -40 164 -16 164 -34 b 59 -353 155 -158 122 -254 b 40 -371 53 -366 47 -371 b 34 -370 38 -371 36 -370 b 25 -358 28 -367 25 -363 b 31 -337 25 -352 27 -347 b 92 0 72 -234 92 -117 b 31 335 92 116 72 233 b 25 356 27 345 25 352 b 34 369 25 363 28 366 " },
    "v86": { "x_min": -571.671875, "x_max": 570.3125, "ha": 582, "o": "m -386 173 b -381 174 -385 174 -383 174 b -370 173 -377 174 -372 173 b -281 86 -367 172 -347 151 b -196 0 -235 37 -196 0 b -126 84 -196 0 -164 37 b -50 174 -55 173 -57 172 b -44 174 -49 174 -47 174 b -35 173 -42 174 -38 173 b 53 86 -32 172 -12 151 b 138 0 100 37 138 0 b 208 84 140 0 170 37 b 284 174 279 173 277 172 b 289 174 285 174 288 174 b 299 173 294 174 298 173 b 462 11 303 172 338 134 l 475 -1 l 503 34 b 541 76 534 72 536 74 b 548 77 544 77 545 77 b 570 56 560 77 570 68 b 567 48 570 54 568 51 b 392 -172 564 41 397 -166 b 378 -176 387 -174 382 -176 b 368 -174 374 -176 371 -176 b 280 -87 367 -173 345 -152 b 194 0 234 -38 194 0 b 125 -86 194 0 163 -38 b 49 -174 54 -174 55 -173 b 43 -176 47 -176 46 -176 b 34 -174 40 -176 36 -174 b -54 -87 31 -173 10 -152 b -140 0 -102 -38 -140 0 b -209 -86 -141 0 -171 -38 b -285 -174 -280 -174 -279 -173 b -291 -176 -287 -176 -289 -176 b -300 -174 -295 -176 -299 -174 b -464 -12 -304 -173 -340 -137 l -476 0 l -504 -34 b -543 -77 -534 -73 -537 -76 b -549 -79 -545 -79 -547 -79 b -571 -58 -562 -79 -571 -69 b -568 -49 -571 -55 -570 -52 b -392 172 -566 -43 -396 167 b -386 173 -390 172 -387 173 " },
    "v8a": { "x_min": -170.140625, "x_max": 168.78125, "ha": 172, "o": "m -160 567 b -122 567 -159 567 -149 567 l -87 567 l -84 566 b -74 553 -78 563 -77 560 b -20 366 -73 551 -49 466 b 31 186 8 267 31 186 b 85 371 31 186 55 269 b 140 559 114 473 138 557 b 153 567 141 564 148 567 b 168 559 159 567 166 564 b 168 555 168 557 168 557 b 92 281 168 548 159 513 b 14 13 50 134 14 13 b 0 0 14 6 6 0 b -17 15 -8 0 -17 8 b -93 283 -17 15 -51 136 b -170 552 -166 533 -170 548 b -170 553 -170 552 -170 552 b -160 567 -170 560 -167 564 " },
    "v8b": { "x_min": 0, "x_max": 319.859375, "ha": 326, "o": "m 149 508 b 159 509 152 509 155 509 b 186 494 170 509 181 503 b 190 440 190 487 190 488 l 190 430 l 190 377 l 242 377 l 251 377 b 303 373 298 377 296 377 b 319 345 314 367 319 356 b 304 319 319 335 314 324 b 250 315 296 315 299 315 l 242 315 l 190 315 l 190 262 l 190 252 b 186 198 190 204 190 205 b 159 183 179 188 170 183 b 132 198 148 183 138 188 b 127 252 127 205 127 204 l 127 262 l 127 315 l 76 315 l 68 315 b 14 319 20 315 21 315 b 0 347 4 324 0 335 b 14 373 0 356 4 367 b 68 377 21 377 20 377 l 76 377 l 127 377 l 127 430 l 127 440 b 132 494 127 488 127 487 b 149 508 136 501 142 505 " },
    "v8c": { "x_min": -330.75, "x_max": 329.390625, "ha": 336, "o": "m -133 483 b -117 484 -127 484 -122 484 b 31 373 -51 484 9 440 b 35 348 34 365 35 356 b -25 285 35 313 10 285 b -87 331 -55 285 -76 302 b -167 402 -100 376 -133 402 b -191 398 -175 402 -183 401 b -227 341 -215 388 -227 369 b -225 320 -227 334 -227 327 b -13 74 -209 230 -125 133 b 6 65 -4 70 5 66 l 9 63 l 10 65 b 117 231 12 68 40 112 l 189 341 l 242 424 b 268 460 262 456 264 458 b 283 464 273 463 277 464 b 308 438 296 464 308 453 l 308 437 b 287 396 308 430 308 428 l 95 98 l 59 43 l 58 41 l 65 37 b 253 -156 151 -8 217 -77 b 281 -285 272 -199 281 -244 b 148 -481 281 -381 231 -463 b 115 -485 137 -484 126 -485 b -32 -376 51 -485 -9 -442 b -36 -349 -35 -366 -36 -358 b 25 -287 -36 -315 -12 -287 b 85 -333 54 -287 74 -302 b 166 -403 99 -377 133 -403 b 190 -399 174 -403 182 -402 b 225 -342 215 -390 225 -370 b 224 -322 225 -335 225 -328 b 12 -76 208 -231 125 -134 b -8 -66 2 -72 -6 -68 l -10 -65 l -12 -66 b -118 -231 -13 -68 -42 -113 l -190 -342 l -243 -426 b -269 -462 -264 -458 -265 -458 b -284 -466 -274 -464 -279 -466 b -310 -440 -298 -466 -310 -455 l -310 -438 b -288 -398 -310 -430 -308 -430 l -96 -99 l -59 -44 l -59 -43 l -66 -38 b -281 284 -198 33 -281 158 l -281 284 b -133 483 -281 392 -220 474 m 254 177 b 266 179 258 177 262 179 b 319 149 287 179 307 167 b 329 115 326 140 329 127 b 319 79 329 102 326 90 b 268 51 307 61 287 51 b 221 72 250 51 234 58 b 205 115 210 84 205 99 b 254 177 205 142 223 170 m -281 -54 b -269 -52 -277 -52 -273 -52 b -223 -73 -253 -52 -235 -59 b -206 -116 -212 -84 -206 -101 b -216 -151 -206 -129 -209 -141 b -269 -179 -228 -170 -249 -179 b -314 -159 -285 -179 -302 -173 b -330 -116 -325 -147 -330 -131 b -281 -54 -330 -88 -313 -61 " },
    "v8d": { "x_min": -1.359375, "x_max": 255.890625, "ha": 261, "o": "m 118 514 b 127 517 121 517 122 517 b 147 505 136 517 142 513 l 148 502 l 148 403 b 148 306 148 351 148 306 b 174 315 149 306 160 310 l 200 324 l 205 323 b 223 312 213 323 220 319 l 225 308 l 225 260 b 225 245 225 255 225 249 b 220 204 225 208 224 209 b 179 188 216 199 215 199 l 148 177 l 148 124 l 148 70 l 189 84 b 236 98 219 94 230 98 b 247 94 240 98 243 97 b 255 52 254 88 255 87 b 255 33 255 47 255 40 l 254 -12 l 253 -15 b 249 -22 253 -18 250 -20 l 245 -24 l 196 -41 l 148 -58 l 148 -108 b 148 -158 148 -136 148 -158 b 174 -148 148 -158 160 -154 b 204 -140 198 -140 200 -140 l 204 -140 b 224 -152 213 -140 221 -145 b 225 -201 224 -155 225 -177 b 224 -254 225 -226 224 -251 b 157 -284 220 -262 220 -262 l 148 -288 l 148 -395 l 148 -503 l 147 -506 b 127 -519 142 -514 134 -519 b 107 -506 119 -519 111 -514 l 106 -503 l 106 -403 b 106 -303 106 -316 106 -303 b 104 -303 104 -303 104 -303 b 88 -310 104 -303 96 -306 l 63 -319 b 51 -322 59 -320 55 -322 b 36 -315 46 -322 40 -319 b 31 -273 32 -309 31 -312 b 31 -258 31 -269 31 -263 l 31 -210 l 34 -206 b 40 -198 35 -204 38 -199 b 74 -186 42 -197 57 -191 l 106 -173 l 106 -123 b 106 -97 106 -112 106 -104 b 106 -72 106 -76 106 -72 b 104 -72 106 -72 106 -72 b 20 -99 89 -79 23 -99 b 0 -84 10 -99 2 -93 b -1 -37 0 -81 -1 -59 b 0 11 -1 -15 0 9 b 58 40 4 22 2 22 l 106 56 l 106 109 b 106 123 106 115 106 119 b 106 162 106 147 106 162 b 81 155 106 162 93 159 b 50 147 65 149 55 147 b 36 152 43 147 40 148 b 31 194 32 158 31 156 b 31 209 31 198 31 204 l 31 256 l 34 260 b 76 281 38 269 38 269 l 106 292 l 106 396 l 106 502 l 107 505 b 118 514 110 509 114 513 " },
    "v8f": { "x_min": -21.78125, "x_max": 362.0625, "ha": 369, "o": "m 302 1031 b 308 1032 304 1032 307 1032 b 330 1016 318 1032 325 1027 b 362 867 351 970 362 920 b 340 738 362 824 353 780 l 336 727 l 340 717 b 362 591 355 677 362 634 b 257 323 362 496 325 401 b 204 272 243 306 227 290 b 20 56 129 206 66 133 b -1 18 12 44 0 22 b -19 4 -4 9 -12 4 l -21 4 l -21 140 l -21 276 l -12 277 b 167 333 61 288 127 309 b 319 598 262 388 319 491 b 311 664 319 620 317 642 l 310 673 l 304 664 b 204 548 279 620 250 587 b 20 333 129 483 66 409 b -1 292 12 320 0 298 b -19 280 -4 285 -12 280 l -21 280 l -21 416 l -21 552 l -12 553 b 167 609 61 564 127 585 b 319 874 264 666 319 770 b 294 992 319 914 311 954 b 288 1011 288 1004 288 1007 b 302 1031 288 1021 294 1028 " },
    "v90": { "x_min": -171.5, "x_max": 483.1875, "ha": 493, "o": "m -8 631 b -1 632 -6 632 -4 632 b 19 620 8 632 16 628 b 20 495 20 616 20 616 b 20 373 20 427 20 373 b 115 410 20 373 63 390 l 210 448 l 210 531 b 212 620 210 614 210 616 b 231 632 215 628 223 632 b 246 627 236 632 242 631 b 251 541 251 620 251 628 l 251 463 l 315 489 b 387 514 368 509 381 514 b 393 513 390 514 392 514 b 406 494 402 510 406 502 b 397 476 406 487 404 480 b 323 446 396 474 363 462 l 251 417 l 251 283 l 251 148 l 254 151 b 370 199 291 183 332 199 b 415 191 385 199 400 197 b 483 84 458 176 483 134 b 461 0 483 58 476 29 b 332 -142 439 -40 411 -72 l 255 -215 b 231 -229 240 -229 239 -229 b 216 -223 224 -229 220 -227 b 210 -158 210 -217 210 -223 b 210 -120 210 -148 210 -136 l 210 -29 l 205 -34 b 100 -142 182 -65 159 -88 l 23 -215 b -1 -229 9 -229 6 -229 b -19 -217 -9 -229 -16 -224 l -20 -215 l -21 48 l -21 310 l -83 287 b -152 262 -133 266 -145 262 b -157 263 -153 262 -155 262 b -171 283 -166 266 -171 274 b -161 301 -171 290 -167 297 b -91 328 -160 302 -129 315 l -21 356 l -21 487 l -20 617 l -19 621 b -8 631 -17 626 -12 630 m 210 288 b 210 401 210 351 210 401 b 114 365 209 401 167 384 l 20 327 l 20 238 l 20 148 l 21 151 b 140 199 59 183 102 199 b 206 180 164 199 187 192 l 209 177 b 209 177 209 177 209 177 b 210 288 210 177 210 199 m 110 131 b 96 133 106 133 100 133 b 89 133 93 133 91 133 b 24 87 63 129 40 113 l 20 80 l 20 -37 l 20 -156 l 23 -152 b 144 81 96 -72 144 20 l 144 83 b 110 131 144 113 134 126 m 341 131 b 328 133 337 133 332 133 b 322 133 326 133 323 133 b 257 87 296 129 273 113 l 251 80 l 251 -37 l 251 -156 l 255 -152 b 375 81 328 -72 375 20 l 375 83 b 341 131 375 113 367 126 " },
    "v92": { "x_min": 0, "x_max": 598.890625, "ha": 611, "o": "m 62 181 b 77 183 66 183 72 183 b 91 181 83 183 88 183 b 202 131 100 180 106 177 l 299 87 l 394 131 b 517 183 499 181 502 183 b 519 183 517 183 518 183 b 598 104 567 183 598 144 b 577 49 598 84 592 65 b 518 15 567 38 563 37 b 484 0 499 6 484 0 b 518 -16 484 -1 499 -8 b 577 -51 563 -38 567 -40 b 598 -105 592 -66 598 -86 b 519 -184 598 -145 567 -184 b 517 -184 518 -184 517 -184 b 394 -133 502 -184 499 -183 l 299 -88 l 202 -133 b 81 -184 99 -183 95 -184 b 77 -184 80 -184 78 -184 b 0 -105 29 -184 0 -145 b 20 -51 0 -86 5 -66 b 80 -16 29 -40 34 -38 b 114 -1 98 -8 114 -1 b 80 15 114 0 98 6 b 20 49 34 37 29 38 b 0 104 6 65 0 84 b 62 181 0 140 23 174 m 88 134 b 74 136 85 134 80 136 b 68 134 72 136 69 136 b 46 104 54 130 46 117 b 55 81 46 95 49 88 b 149 34 59 76 53 80 b 224 -1 190 15 224 0 b 144 -38 224 -1 187 -18 b 54 -84 59 -79 58 -79 b 46 -105 49 -90 46 -98 b 76 -137 46 -122 58 -137 b 78 -137 77 -137 77 -137 b 194 -86 87 -137 76 -141 b 298 -36 250 -58 298 -36 b 298 -36 298 -36 298 -36 b 402 -84 299 -36 345 -58 b 518 -137 522 -141 510 -137 b 521 -137 519 -137 519 -137 b 551 -105 539 -137 551 -122 b 541 -83 551 -98 548 -90 b 447 -36 537 -77 544 -81 b 374 -1 406 -16 374 -1 b 447 34 374 0 406 15 b 541 81 544 80 537 76 b 551 104 548 88 551 97 b 521 136 551 120 539 136 b 518 136 519 136 519 136 b 517 136 518 136 517 136 l 517 136 b 402 83 511 136 511 136 b 298 34 345 56 299 34 b 298 34 298 34 298 34 b 194 84 298 34 250 56 b 88 134 137 111 89 133 " },
    "v93": { "x_min": 0, "x_max": 438.28125, "ha": 447, "o": "m 212 205 b 219 205 213 205 216 205 b 239 183 228 205 231 204 b 421 -163 298 40 363 -83 b 438 -191 434 -180 438 -186 b 436 -197 438 -192 438 -195 b 424 -206 434 -204 431 -206 b 406 -201 420 -206 415 -205 b 216 -156 347 -172 281 -156 b 23 -205 148 -156 80 -173 b 14 -206 20 -206 17 -206 b 0 -191 6 -206 0 -201 b 6 -176 0 -187 1 -183 b 202 192 63 -104 142 45 b 212 205 205 199 208 202 m 264 48 l 249 81 l 243 94 l 242 91 b 89 -126 208 36 137 -66 b 81 -138 85 -133 81 -138 b 81 -138 81 -138 81 -138 b 81 -138 81 -138 81 -138 b 95 -133 81 -138 87 -136 b 280 -94 156 -108 221 -94 b 334 -98 299 -94 317 -95 b 343 -99 338 -99 343 -99 b 343 -99 343 -99 343 -99 b 338 -94 343 -99 341 -97 b 264 48 318 -58 287 1 " },
    "v94": { "x_min": -149.71875, "x_max": 148.359375, "ha": 151, "o": "m -9 215 b 0 217 -6 217 -4 217 b 19 205 8 217 14 213 b 20 142 20 202 20 201 l 20 84 l 23 84 b 144 -27 81 74 129 30 b 148 -66 147 -40 148 -54 b 36 -213 148 -134 103 -197 b 0 -219 24 -217 12 -219 b -145 -104 -68 -219 -129 -173 b -149 -68 -148 -91 -149 -79 b -24 84 -149 6 -98 74 l -21 84 l -21 142 b -19 205 -20 201 -20 202 b -9 215 -17 209 -13 213 m -21 -15 b -23 41 -21 37 -21 41 b -23 41 -23 41 -23 41 b -76 11 -35 40 -62 26 b -108 -65 -98 -11 -108 -38 b -1 -176 -108 -122 -65 -176 b 107 -65 63 -176 107 -122 b 74 11 107 -38 96 -11 b 20 41 61 26 32 41 b 20 -15 20 41 20 15 b 19 -74 20 -72 20 -72 b 0 -87 14 -83 6 -87 b -19 -74 -8 -87 -16 -83 b -21 -15 -20 -72 -20 -72 " },
    "v95": { "x_min": 0, "x_max": 406.96875, "ha": 415, "o": "m 55 181 b 70 183 61 183 66 183 b 111 170 85 183 99 179 b 160 130 115 167 137 149 l 202 95 l 245 130 b 319 181 299 176 302 179 b 334 183 325 183 330 183 b 406 109 375 183 406 148 b 401 81 406 99 405 91 b 348 24 394 65 390 59 b 318 -1 332 11 318 0 b 348 -26 318 -1 332 -12 b 401 -83 390 -61 394 -66 b 406 -111 405 -93 406 -101 b 334 -184 406 -149 375 -184 b 319 -183 330 -184 325 -184 b 245 -131 302 -180 299 -177 l 202 -97 l 160 -131 b 85 -183 107 -177 103 -180 b 70 -184 80 -184 76 -184 b 0 -111 31 -184 0 -149 b 4 -83 0 -101 1 -93 b 58 -26 10 -66 16 -61 b 88 -1 74 -12 88 -1 b 58 24 88 0 74 11 b 10 69 23 54 17 59 b 0 109 2 81 0 95 b 55 181 0 142 21 173 m 83 133 b 72 136 78 136 76 136 b 57 131 66 136 61 134 b 46 109 49 126 46 117 b 50 93 46 104 47 98 b 107 45 51 91 77 70 b 160 0 137 20 160 0 b 107 -47 160 -1 137 -22 b 50 -94 77 -72 51 -93 b 46 -111 47 -99 46 -105 b 59 -134 46 -120 50 -130 b 72 -137 62 -136 68 -137 b 83 -136 76 -137 80 -136 b 144 -84 84 -134 107 -116 b 202 -36 176 -58 202 -36 b 261 -84 202 -36 230 -58 b 323 -136 299 -116 321 -134 b 334 -137 326 -136 330 -137 b 345 -134 338 -137 343 -136 b 360 -111 355 -130 360 -120 b 355 -94 360 -105 359 -99 b 299 -47 353 -93 329 -72 b 245 0 269 -22 245 -1 b 299 45 245 0 269 20 b 355 93 329 70 353 91 b 360 109 359 98 360 104 b 345 133 360 119 355 129 b 334 136 343 134 338 136 b 323 134 330 136 326 134 b 261 83 321 133 299 115 b 202 34 230 56 202 34 b 144 83 202 34 176 56 b 83 133 106 115 84 133 " },
    "v97": { "x_min": -228.671875, "x_max": 227.3125, "ha": 232, "o": "m -217 487 l -213 488 l 0 488 l 212 488 l 216 487 b 225 476 220 484 224 480 l 227 473 l 227 244 l 227 15 l 225 12 b 206 0 223 4 215 0 b 197 1 204 0 200 0 b 187 12 193 4 189 6 l 186 15 l 186 138 l 186 262 l -1 262 l -187 262 l -187 138 l -187 15 l -189 12 b -208 0 -193 4 -200 0 b -227 12 -216 0 -223 4 l -228 15 l -228 244 l -228 473 l -227 476 b -217 487 -225 480 -221 484 " },
    "v9a": { "x_min": -21.78125, "x_max": 367.5, "ha": 375, "o": "m 230 1031 b 238 1032 232 1032 235 1032 b 259 1014 245 1032 251 1027 b 367 662 330 906 367 782 b 364 602 367 641 367 621 b 232 317 352 488 304 384 b 57 120 155 245 103 187 b -1 18 31 84 6 40 b -19 4 -4 11 -12 4 l -21 4 l -21 159 l -21 315 l -16 315 b 96 335 10 315 62 324 b 315 695 227 380 315 527 b 313 738 315 709 314 724 b 224 991 304 825 273 916 b 216 1013 219 999 216 1007 b 230 1031 216 1021 220 1028 " },
    "v9b": { "x_min": -24.5, "x_max": 313.0625, "ha": 319, "o": "m -24 -133 l -24 -5 l -20 -5 b -1 -19 -12 -5 -4 -11 b 142 -213 13 -61 74 -144 b 258 -376 196 -269 230 -315 b 313 -605 295 -449 313 -528 b 292 -742 313 -652 306 -699 b 288 -752 289 -748 288 -752 b 288 -752 288 -752 288 -752 b 292 -764 289 -753 291 -757 b 313 -907 306 -811 313 -860 b 292 -1045 313 -954 306 -1002 b 288 -1054 289 -1050 288 -1054 b 288 -1054 288 -1054 288 -1054 b 292 -1067 289 -1054 291 -1060 b 313 -1210 306 -1113 313 -1161 b 292 -1346 313 -1257 306 -1304 b 288 -1357 289 -1353 288 -1357 b 288 -1357 288 -1357 288 -1357 b 292 -1368 289 -1357 291 -1363 b 313 -1512 306 -1415 313 -1464 b 292 -1648 313 -1560 306 -1605 b 288 -1660 289 -1654 288 -1660 b 288 -1660 288 -1660 288 -1660 b 292 -1671 289 -1660 291 -1665 b 313 -1814 306 -1719 313 -1766 b 250 -2040 313 -1897 291 -1977 b 232 -2062 238 -2057 236 -2059 b 221 -2065 230 -2063 225 -2065 b 200 -2045 210 -2065 201 -2057 b 200 -2043 200 -2044 200 -2044 b 208 -2026 200 -2037 202 -2034 b 269 -1826 249 -1966 269 -1897 b 153 -1544 269 -1726 230 -1625 b -9 -1472 115 -1506 58 -1481 b -21 -1471 -14 -1471 -19 -1471 l -24 -1471 l -24 -1343 l -24 -1215 l -20 -1215 b -1 -1229 -12 -1215 -4 -1221 b 142 -1424 13 -1270 74 -1353 b 257 -1582 196 -1478 228 -1524 b 264 -1594 261 -1589 264 -1594 l 264 -1594 b 265 -1582 264 -1594 264 -1589 b 270 -1525 268 -1562 270 -1544 b 153 -1243 270 -1424 228 -1321 b -9 -1170 115 -1203 58 -1178 b -21 -1168 -14 -1170 -19 -1168 l -24 -1168 l -24 -1041 l -24 -913 l -20 -913 b -1 -927 -12 -913 -4 -918 b 142 -1121 13 -967 74 -1050 b 257 -1281 196 -1175 228 -1221 b 264 -1292 261 -1286 264 -1292 l 264 -1292 b 265 -1279 264 -1292 264 -1286 b 270 -1222 268 -1261 270 -1242 b 153 -941 270 -1121 228 -1018 b -9 -867 115 -900 58 -875 b -21 -866 -14 -867 -19 -866 l -24 -866 l -24 -738 l -24 -610 l -20 -610 b -1 -624 -12 -610 -4 -616 b 142 -818 13 -664 74 -749 b 257 -978 196 -873 228 -918 b 264 -989 261 -984 264 -989 l 264 -989 b 265 -977 264 -989 264 -984 b 270 -920 268 -959 270 -939 b 153 -638 270 -818 228 -716 b -9 -564 115 -598 58 -573 b -21 -563 -14 -564 -19 -563 l -24 -563 l -24 -435 l -24 -308 l -20 -308 b -1 -322 -12 -308 -4 -313 b 142 -516 13 -363 74 -446 b 257 -675 196 -571 228 -616 b 264 -687 261 -681 264 -687 l 264 -687 b 265 -674 264 -687 264 -681 b 270 -617 268 -656 270 -637 b 153 -335 270 -516 228 -413 b -9 -262 115 -295 58 -270 b -21 -260 -14 -262 -19 -260 l -24 -260 l -24 -133 " },
    "v9c": { "x_min": -166.0625, "x_max": -25.859375, "ha": 0, "o": "m -49 369 b -42 370 -46 369 -44 370 b -27 360 -36 370 -29 366 b -25 355 -27 359 -25 358 b -32 335 -25 351 -28 347 b -92 52 -66 248 -87 159 b -93 -1 -93 43 -93 20 b -92 -54 -93 -23 -93 -45 b -32 -337 -85 -162 -66 -251 b -25 -355 -27 -349 -25 -352 b -42 -371 -25 -365 -32 -371 b -61 -353 -50 -371 -51 -369 b -163 -63 -119 -262 -153 -165 b -166 -1 -166 -37 -166 -31 b -163 62 -166 30 -166 36 b -61 352 -153 163 -119 260 b -49 369 -54 365 -51 366 " },
    "v9e": { "x_min": 0, "x_max": 607.0625, "ha": 619, "o": "m 243 631 b 250 632 246 632 249 632 b 270 620 259 632 268 628 l 272 616 l 272 201 l 272 -212 l 270 -216 b 251 -229 268 -224 259 -229 b 227 -215 243 -229 240 -229 l 151 -142 b 32 -16 81 -80 53 -49 b 0 84 9 18 0 52 b 111 199 0 149 42 199 b 137 197 119 199 127 198 b 228 151 168 191 197 177 l 231 148 l 231 383 b 232 620 231 616 231 616 b 243 631 234 624 238 630 m 168 131 b 152 133 163 133 157 133 b 107 102 130 133 111 120 b 106 86 107 97 106 91 b 111 41 106 73 108 56 b 227 -152 125 -13 171 -90 l 231 -156 l 231 -37 l 231 80 l 225 87 b 168 131 210 111 190 126 m 347 631 b 353 632 348 632 351 632 b 374 620 363 632 371 628 b 375 383 375 616 375 616 l 375 148 l 377 151 b 492 199 415 183 454 199 b 537 191 507 199 522 197 b 607 84 582 176 607 134 b 583 0 607 58 598 29 b 455 -142 562 -40 533 -72 l 378 -215 b 355 -229 364 -229 362 -229 b 334 -216 345 -229 337 -224 l 333 -212 l 333 201 l 333 616 l 334 620 b 347 631 337 624 341 630 m 465 131 b 451 133 461 133 455 133 b 445 133 449 133 446 133 b 379 87 419 129 396 113 l 375 80 l 375 -37 l 375 -156 l 378 -152 b 499 81 451 -72 499 20 l 499 83 b 465 131 499 113 490 126 " },
    "va3": { "x_min": 58.53125, "x_max": 228.671875, "ha": 294, "o": "m 138 371 b 142 373 140 371 141 373 b 178 342 149 373 156 366 b 228 251 217 297 228 278 b 228 244 228 248 228 247 b 176 147 227 212 212 184 b 123 73 152 122 132 93 b 121 62 122 70 121 66 b 145 13 121 48 129 31 b 153 -2 151 6 153 1 b 149 -9 153 -5 152 -6 b 144 -11 148 -11 145 -11 b 129 -1 140 -11 136 -8 b 61 87 89 37 68 68 b 58 113 59 95 58 105 b 110 215 58 144 74 177 b 163 287 134 240 155 269 b 166 299 166 291 166 295 b 141 348 166 313 157 330 b 133 360 134 356 133 358 b 133 363 133 362 133 362 b 138 371 133 367 136 370 " },
    "va5": { "x_min": 0, "x_max": 349.8125, "ha": 357, "o": "m 88 302 b 103 303 93 302 98 303 b 202 224 149 303 191 270 b 205 199 204 216 205 208 b 178 129 205 173 196 147 l 175 126 l 182 127 b 307 249 236 142 284 190 b 313 259 308 254 311 258 b 329 267 317 265 323 267 b 349 247 340 267 349 259 b 201 -263 349 242 204 -258 b 182 -273 197 -270 190 -273 b 163 -260 174 -273 166 -269 b 161 -256 161 -259 161 -258 b 217 -59 161 -248 170 -220 b 272 129 247 43 272 127 b 272 129 272 129 272 129 b 264 122 272 129 268 126 b 140 80 227 94 183 80 b 36 115 102 80 65 91 b 0 194 10 136 0 165 b 88 302 0 244 32 292 " },
    "va9": { "x_min": -24.5, "x_max": 314.421875, "ha": 321, "o": "m -24 -145 l -24 -5 l -20 -5 b 0 -23 -9 -5 -2 -12 b 27 -87 4 -38 14 -66 b 138 -220 53 -136 88 -177 b 235 -328 179 -255 208 -288 b 314 -592 287 -409 314 -501 b 292 -732 314 -639 307 -687 l 289 -742 l 294 -756 b 314 -896 307 -802 314 -849 b 292 -1035 314 -943 307 -991 l 289 -1045 l 294 -1057 b 314 -1197 307 -1104 314 -1152 b 292 -1338 314 -1246 307 -1292 l 289 -1347 l 294 -1360 b 314 -1500 307 -1407 314 -1454 b 273 -1689 314 -1565 300 -1628 b 250 -1712 265 -1710 261 -1712 b 228 -1691 236 -1712 228 -1704 l 228 -1685 l 234 -1675 b 270 -1507 258 -1621 270 -1564 b 98 -1193 270 -1381 209 -1261 b 40 -1174 76 -1179 58 -1174 b -10 -1189 24 -1174 8 -1178 b -20 -1192 -14 -1192 -16 -1192 l -24 -1192 l -24 -1052 l -24 -913 l -20 -913 b 0 -931 -9 -913 -2 -920 b 27 -995 4 -946 14 -974 b 138 -1128 53 -1043 88 -1085 b 257 -1275 190 -1172 228 -1220 b 262 -1283 259 -1279 262 -1283 l 262 -1283 b 269 -1249 264 -1282 268 -1260 b 270 -1206 270 -1233 270 -1220 b 98 -891 270 -1075 206 -957 b 40 -871 76 -877 58 -871 b -10 -886 24 -871 8 -875 b -20 -889 -14 -889 -16 -889 l -24 -889 l -24 -749 l -24 -610 l -20 -610 b 0 -628 -9 -610 -2 -617 b 27 -692 4 -644 14 -671 b 138 -825 53 -741 88 -782 b 257 -973 190 -870 228 -917 b 262 -981 259 -977 262 -981 l 262 -981 b 269 -946 264 -979 268 -957 b 270 -903 270 -931 270 -917 b 98 -588 270 -774 206 -655 b 40 -569 76 -574 58 -569 b -10 -584 24 -569 8 -574 b -20 -587 -14 -587 -16 -587 l -24 -587 l -24 -448 l -24 -308 l -20 -308 b 0 -326 -9 -308 -2 -315 b 27 -390 4 -341 14 -369 b 138 -523 53 -438 88 -480 b 257 -670 190 -567 228 -614 b 262 -678 259 -674 262 -678 b 262 -678 262 -678 262 -678 b 269 -644 264 -677 268 -656 b 270 -601 270 -628 270 -614 b 98 -285 270 -471 206 -352 b 40 -266 76 -273 58 -266 b -10 -281 24 -266 8 -272 b -20 -284 -14 -284 -16 -284 l -24 -284 l -24 -145 " },
    "vaa": { "x_min": -1.359375, "x_max": 752.703125, "ha": 768, "o": "m 490 985 b 504 986 495 986 500 986 b 604 907 551 986 593 954 b 607 884 607 900 607 892 b 581 813 607 857 597 831 l 578 810 l 583 811 b 710 932 638 827 687 873 b 714 943 711 936 713 942 b 730 952 720 949 725 952 b 752 931 741 952 752 943 b 200 -946 752 927 204 -941 b 182 -957 197 -953 190 -957 b 163 -945 174 -957 166 -953 b 161 -939 161 -942 161 -942 b 217 -743 161 -931 170 -904 b 272 -555 247 -639 272 -555 b 272 -555 272 -555 272 -555 b 264 -560 272 -555 268 -557 b 140 -603 227 -589 182 -603 b 36 -567 102 -603 65 -592 b -1 -487 12 -548 -1 -517 b 17 -427 -1 -466 5 -445 b 103 -380 38 -395 70 -380 b 191 -433 137 -380 172 -398 b 205 -484 201 -448 205 -466 b 178 -553 205 -509 196 -535 l 175 -557 l 182 -555 b 307 -435 236 -539 284 -494 b 372 -213 308 -430 372 -215 b 372 -213 372 -213 372 -213 b 364 -219 372 -213 368 -216 b 240 -262 328 -247 283 -262 b 137 -226 202 -262 166 -249 b 99 -145 112 -206 99 -176 b 118 -84 99 -124 106 -104 b 204 -38 138 -54 171 -38 b 292 -91 238 -38 273 -56 b 306 -141 302 -106 306 -124 b 279 -212 306 -167 296 -194 l 276 -215 l 281 -213 b 408 -93 336 -198 385 -151 b 473 129 409 -88 473 127 b 473 129 473 129 473 129 b 465 122 473 129 469 126 b 341 80 428 94 383 80 b 236 115 303 80 266 91 b 200 195 213 136 200 165 b 217 256 200 217 206 238 b 304 303 239 287 272 303 b 393 249 338 303 374 285 b 406 199 402 234 406 217 b 379 129 406 173 397 148 l 377 126 l 382 127 b 509 248 436 142 485 190 b 574 470 510 254 574 469 b 574 470 574 470 574 470 b 566 464 574 470 570 467 b 442 421 529 435 484 421 b 337 458 404 421 367 433 b 300 538 314 477 300 508 b 318 598 300 559 306 580 b 404 645 340 630 372 645 b 494 592 439 645 475 627 b 507 541 502 577 507 559 b 480 471 507 516 498 489 l 477 467 l 483 470 b 608 589 537 485 586 531 b 675 811 611 595 675 810 b 675 811 675 811 675 811 b 666 806 675 811 671 809 b 543 763 628 777 585 763 b 438 799 504 763 468 775 b 401 878 412 820 401 849 b 490 985 401 928 434 977 " },
    "vab": { "x_min": 0, "x_max": 272.21875, "ha": 278, "o": "m 243 631 b 250 632 246 632 249 632 b 270 620 259 632 268 628 l 272 616 l 272 201 l 272 -212 l 270 -216 b 251 -229 268 -224 259 -229 b 227 -215 243 -229 240 -229 l 151 -142 b 32 -16 81 -80 53 -49 b 0 84 9 18 0 52 b 111 199 0 149 42 199 b 137 197 119 199 127 198 b 228 151 168 191 197 177 l 231 148 l 231 383 b 232 620 231 616 231 616 b 243 631 234 624 238 630 m 168 131 b 152 133 163 133 157 133 b 107 102 130 133 111 120 b 106 86 107 97 106 91 b 111 41 106 73 108 56 b 227 -152 125 -13 171 -90 l 231 -156 l 231 -37 l 231 80 l 225 87 b 168 131 210 111 190 126 " },
    "vad": { "x_min": 0, "x_max": 873.828125, "ha": 892, "o": "m 0 0 l 0 703 l 81 703 l 164 703 l 164 0 l 164 -705 l 81 -705 l 0 -705 l 0 0 m 225 0 l 225 703 l 246 703 l 268 703 l 268 366 l 268 30 l 274 36 b 314 79 284 44 302 63 b 413 302 357 137 392 213 b 432 327 419 324 421 327 b 449 306 443 327 447 322 b 611 115 457 195 529 115 b 651 122 624 115 638 117 b 728 316 705 140 724 188 b 729 388 728 342 729 366 b 671 635 729 533 711 602 b 581 662 649 652 616 662 b 477 637 545 662 510 653 l 475 635 l 477 634 b 503 627 488 632 495 631 b 545 556 532 612 545 584 b 491 480 545 524 526 491 b 465 474 481 476 473 474 b 379 563 417 474 379 516 b 389 602 379 576 382 588 b 541 691 409 641 479 681 b 582 694 555 692 568 694 b 865 462 714 694 834 598 b 873 392 871 440 873 416 b 865 317 873 367 871 341 b 639 84 839 194 748 101 b 612 83 630 83 620 83 b 511 116 577 83 543 94 b 504 120 509 119 506 120 b 504 120 504 120 504 120 b 469 59 504 120 488 93 l 432 -1 l 469 -61 b 504 -122 488 -94 504 -122 b 504 -122 504 -122 504 -122 b 511 -117 506 -122 509 -120 b 612 -84 543 -95 577 -84 b 665 -91 630 -84 647 -87 b 869 -338 771 -122 850 -216 b 873 -392 872 -356 873 -374 b 798 -595 873 -469 847 -539 b 581 -695 741 -662 660 -695 b 406 -626 517 -695 454 -671 b 381 -563 389 -607 381 -585 b 465 -477 381 -519 413 -477 b 545 -559 514 -477 545 -519 b 503 -628 545 -587 532 -613 b 477 -635 495 -632 488 -634 l 475 -637 l 477 -638 b 581 -663 510 -655 545 -663 b 671 -637 616 -663 649 -653 b 729 -391 711 -603 729 -534 b 728 -317 729 -367 728 -344 b 623 -117 722 -173 698 -124 b 611 -116 619 -116 615 -116 b 449 -308 528 -116 457 -198 b 432 -328 447 -323 443 -328 b 413 -303 421 -328 419 -326 b 314 -80 392 -215 357 -138 b 274 -37 302 -65 284 -45 l 268 -31 l 268 -367 l 268 -705 l 246 -705 l 225 -705 l 225 0 " },
    "vb1": { "x_min": 78.9375, "x_max": 485.921875, "ha": 417, "o": "m 362 378 b 378 380 367 380 372 380 b 472 348 415 380 453 367 b 485 315 481 338 485 327 b 462 273 485 298 477 281 b 439 267 454 269 446 267 b 398 290 424 267 409 274 b 344 319 385 309 364 319 b 281 269 315 319 289 301 b 279 262 280 266 279 262 b 276 256 279 260 277 258 b 274 249 276 254 274 251 b 238 127 273 248 257 192 b 201 4 217 61 201 5 b 166 -1 198 -1 200 -1 b 153 -1 163 -1 157 -1 b 141 -1 148 -1 144 -1 b 104 4 106 -1 107 -1 b 104 6 104 5 104 5 b 142 144 104 13 110 34 b 182 278 164 219 181 276 b 183 288 182 281 182 285 b 185 302 185 292 185 298 b 164 330 185 317 176 328 b 159 330 163 330 161 330 b 102 302 140 330 119 320 b 91 294 95 295 93 294 b 88 294 91 294 89 294 b 78 303 83 294 78 298 b 81 312 78 306 78 309 b 200 373 106 347 160 373 b 215 371 205 373 209 371 b 266 335 235 367 254 353 b 269 331 268 333 269 331 b 269 331 269 331 269 331 b 273 335 269 331 270 334 b 362 378 298 359 330 376 " },
    "vb3": { "x_min": 0, "x_max": 227.3125, "ha": 232, "o": "m 91 213 b 100 215 93 215 96 215 b 227 58 167 215 224 144 b 227 52 227 56 227 54 b 61 -201 227 -43 164 -138 b 29 -216 44 -212 36 -216 b 23 -210 27 -216 24 -213 b 21 -205 21 -208 21 -206 b 34 -192 21 -201 25 -197 b 122 -55 89 -161 122 -106 b 104 6 122 -33 117 -12 l 103 9 l 96 9 b 4 79 57 9 17 38 b 0 112 1 90 0 101 b 91 213 0 163 36 209 " },
    "vb4": { "x_min": -597.53125, "x_max": 596.171875, "ha": 608, "o": "m -533 324 b -525 327 -530 326 -528 327 b -504 305 -514 327 -504 317 b -504 305 -504 305 -504 305 b -513 284 -504 299 -504 299 b -556 112 -541 226 -556 167 b -545 33 -556 84 -552 58 b -524 -20 -541 15 -532 -9 l -522 -23 l -491 15 l -413 111 b -355 174 -367 169 -363 174 b -351 174 -353 174 -352 174 b -254 86 -343 174 -348 179 b -168 -1 -208 37 -168 -1 b -100 84 -168 -1 -137 37 b -23 173 -28 173 -29 172 b -19 174 -21 174 -20 174 b -8 173 -14 174 -10 173 b 80 86 -5 172 13 151 b 166 -1 127 37 166 -1 b 235 84 166 -1 197 37 b 311 173 306 173 304 172 b 317 174 313 174 314 174 b 326 173 319 174 323 173 b 490 11 329 172 366 134 l 502 -1 l 530 34 b 568 76 560 72 563 74 b 575 77 570 77 573 77 b 596 56 586 77 596 68 b 594 48 596 54 596 51 b 417 -172 592 41 424 -166 b 405 -176 415 -174 409 -176 b 396 -174 401 -176 398 -176 b 307 -87 393 -173 372 -152 b 221 -1 259 -38 221 -1 b 152 -86 221 -1 190 -38 b 76 -176 81 -174 83 -173 b 70 -176 74 -176 73 -176 b 61 -174 66 -176 62 -174 b -27 -87 58 -173 38 -152 b -114 -1 -74 -38 -112 -1 b -182 -86 -114 -1 -145 -38 b -258 -176 -253 -174 -253 -173 b -264 -176 -259 -176 -262 -176 b -274 -174 -268 -176 -272 -174 b -438 -11 -277 -173 -348 -102 l -449 0 l -479 -37 b -524 -80 -513 -80 -514 -80 l -524 -80 b -553 -52 -534 -80 -540 -74 b -597 109 -583 -8 -597 48 b -560 280 -597 165 -585 224 b -533 324 -548 310 -540 322 " },
    "vb6": { "x_min": 0, "x_max": 556.6875, "ha": 568, "o": "m 289 545 b 298 546 292 545 295 546 b 318 533 306 546 315 541 b 319 428 319 530 319 528 l 319 327 l 334 327 b 526 223 412 326 485 285 b 543 172 537 206 543 190 b 447 76 543 122 503 76 b 445 76 446 76 446 76 b 359 165 394 77 359 119 b 368 205 359 179 362 192 b 441 251 382 233 412 251 b 455 249 446 251 451 251 b 460 248 458 249 460 248 b 460 248 460 248 460 248 b 454 254 460 249 458 251 b 334 295 419 280 378 294 l 319 295 l 319 4 l 319 -287 l 321 -285 b 328 -285 322 -285 325 -285 b 524 -99 424 -277 507 -198 b 541 -79 526 -84 530 -79 b 556 -97 551 -79 556 -84 b 548 -133 556 -105 553 -117 b 334 -317 521 -233 434 -306 b 322 -319 329 -317 323 -317 l 319 -319 l 319 -424 b 319 -471 319 -444 319 -459 b 313 -541 319 -544 318 -535 b 298 -548 308 -545 303 -548 b 279 -534 289 -548 281 -542 b 277 -424 277 -531 277 -530 l 277 -317 l 273 -317 b 13 -95 153 -305 51 -217 b 0 2 4 -62 0 -29 b 182 295 0 126 66 238 b 274 324 210 309 249 320 l 277 324 l 277 427 b 279 533 277 528 277 530 b 289 545 281 538 285 542 m 277 2 b 277 291 277 161 277 291 b 268 288 277 291 273 290 b 144 1 179 265 144 184 b 276 -284 144 -199 175 -267 l 277 -285 l 277 2 " },
    "vb7": { "x_min": -176.9375, "x_max": 251.8125, "ha": 257, "o": "m -8 631 b -1 632 -6 632 -4 632 b 19 620 8 632 16 628 b 20 503 20 616 20 614 b 20 391 20 442 20 391 b 84 424 20 391 49 406 l 147 456 l 152 456 b 153 456 153 456 153 456 b 175 435 166 456 175 446 b 172 427 175 433 174 430 b 92 380 170 420 172 421 l 20 342 l 20 245 l 20 148 l 21 151 b 137 199 59 183 99 199 b 182 191 152 199 167 197 b 251 84 227 176 251 134 b 228 0 251 58 243 29 b 100 -142 206 -40 178 -72 l 23 -215 b 0 -229 9 -229 6 -229 b -20 -216 -9 -229 -17 -224 b -21 54 -21 -212 -21 -212 b -21 322 -21 201 -21 322 b -85 290 -21 322 -50 308 l -148 256 l -153 256 b -155 256 -155 256 -155 256 b -176 277 -167 256 -176 266 b -174 285 -176 280 -175 283 b -93 333 -171 294 -174 292 l -21 370 l -21 494 b -20 620 -21 616 -21 616 b -8 631 -17 624 -13 630 m 110 131 b 96 133 106 133 100 133 b 89 133 93 133 91 133 b 24 87 63 129 40 113 l 20 80 l 20 -37 l 20 -156 l 23 -152 b 144 81 96 -72 144 20 l 144 83 b 110 131 144 113 134 126 " },
    "vb9": { "x_min": -122.5, "x_max": 121.140625, "ha": 124, "o": "m -16 145 b 0 147 -10 147 -5 147 b 121 -1 66 147 121 77 b 114 -49 121 -16 118 -33 b -1 -148 95 -112 47 -148 b -85 -106 -31 -148 -61 -134 b -122 -1 -110 -76 -122 -38 b -16 145 -122 68 -81 134 m 12 111 b 0 113 8 113 4 113 b -68 22 -29 113 -61 73 b -70 0 -69 15 -70 6 b -13 -113 -70 -49 -47 -98 b -1 -115 -9 -115 -5 -115 b 63 -40 24 -115 53 -83 b 68 -1 66 -27 68 -15 b 12 111 68 48 46 97 " },
    "vba": { "x_min": -118.421875, "x_max": 597.53125, "ha": 381, "o": "m 460 574 b 464 574 461 574 462 574 b 488 574 470 574 481 574 b 500 573 491 574 498 574 b 594 503 543 570 588 538 b 597 488 596 498 597 494 b 528 417 597 449 564 417 b 502 423 519 417 510 419 b 465 481 477 434 465 458 b 488 528 465 499 472 516 b 490 530 490 530 490 530 b 490 530 490 530 490 530 b 468 517 488 530 475 523 b 349 340 419 485 377 420 b 347 330 348 334 347 330 b 383 328 347 328 363 328 b 428 326 423 328 424 328 b 442 302 438 320 442 312 b 430 281 442 294 438 285 b 385 276 424 277 426 276 l 377 276 l 332 276 l 330 269 b 178 -117 303 126 250 -9 b 1 -249 129 -194 69 -237 b -20 -251 -6 -251 -13 -251 b -114 -187 -65 -251 -100 -227 b -118 -156 -117 -177 -118 -166 b -51 -84 -118 -116 -91 -84 b -31 -87 -46 -84 -39 -86 b 16 -152 0 -95 16 -124 b -12 -205 16 -173 8 -194 b -16 -208 -14 -206 -16 -208 b -14 -208 -16 -208 -14 -208 b -9 -206 -14 -208 -12 -208 b 74 -124 23 -197 54 -166 b 172 224 98 -79 125 22 b 185 276 178 252 183 274 b 185 276 185 276 185 276 b 141 276 185 276 181 276 b 91 280 96 276 96 276 b 77 302 83 285 77 294 b 91 326 77 312 83 320 b 148 328 95 328 96 328 l 198 330 l 202 341 b 460 574 249 473 351 566 " },
    "vbf": { "x_min": -53.078125, "x_max": 513.140625, "ha": 485, "o": "m 185 383 b 196 384 187 383 191 384 b 277 334 230 384 259 365 b 288 301 281 324 288 306 b 288 297 288 298 288 297 b 294 302 289 297 291 299 b 394 370 323 338 367 367 b 404 371 398 370 401 371 b 510 272 453 371 498 328 b 513 237 513 262 513 251 b 507 172 513 217 511 192 b 326 -34 487 59 412 -26 b 314 -36 322 -36 318 -36 b 274 -24 298 -36 283 -31 l 265 -16 b 224 44 246 -1 232 20 b 223 49 224 47 223 49 b 223 49 223 49 223 49 b 149 -197 221 48 149 -194 b 149 -198 149 -197 149 -198 b 170 -210 149 -202 155 -205 b 187 -215 174 -210 175 -212 b 204 -231 201 -219 204 -222 b 197 -245 204 -240 202 -242 l 194 -248 l 76 -248 l -42 -248 l -46 -245 b -53 -231 -51 -242 -53 -240 b -35 -215 -53 -222 -49 -217 b -13 -210 -21 -212 -20 -212 b -6 -208 -10 -209 -8 -208 b 0 -206 -6 -208 -2 -206 b 25 -188 13 -201 21 -195 b 163 280 28 -183 163 276 b 166 291 163 283 164 287 b 167 302 167 295 167 299 b 155 324 167 315 161 324 b 155 324 155 324 155 324 b 65 230 125 322 85 280 b 53 215 61 217 58 215 b 51 215 53 215 51 215 b 42 224 46 215 42 217 b 57 263 42 231 47 244 b 140 360 77 305 104 337 b 152 370 144 365 149 369 b 185 383 157 376 172 381 m 374 306 b 366 308 371 308 368 308 b 300 273 348 308 321 294 b 284 254 288 262 287 259 b 280 242 283 249 281 245 b 257 169 279 240 270 213 l 236 98 l 236 93 b 251 48 238 77 243 61 b 279 27 258 37 272 27 b 281 27 279 27 280 27 b 291 31 281 27 287 30 b 396 170 334 52 378 109 b 406 247 402 197 406 224 b 401 277 406 259 405 270 b 374 306 397 290 383 303 " },
    "vc3": { "x_min": -10.890625, "x_max": 299.4375, "ha": 294, "o": "m 136 460 b 142 462 137 462 140 462 b 166 449 152 462 161 456 b 171 428 168 446 168 445 b 288 131 194 322 238 209 b 298 115 295 120 296 117 b 299 106 298 112 299 109 b 273 81 299 91 287 81 b 255 86 268 81 261 83 b 155 116 225 104 183 116 l 152 116 l 149 108 b 141 83 148 102 144 91 b 134 48 137 69 134 58 b 149 9 134 34 140 24 b 153 -1 152 5 153 1 b 149 -9 153 -5 152 -6 b 144 -11 148 -11 147 -11 b 122 2 138 -11 133 -6 b 95 61 104 20 95 38 b 107 108 95 74 99 90 b 108 113 107 111 108 112 b 107 113 108 113 108 113 b 102 113 106 113 104 113 b 31 86 76 108 53 98 b 14 80 24 81 20 80 b -10 106 0 80 -10 91 b 0 131 -10 115 -9 116 b 115 430 49 209 91 317 b 136 460 119 451 123 456 " },
    "vd0": { "x_min": -10.890625, "x_max": 299.4375, "ha": 294, "o": "m 44 174 b 51 174 47 174 49 174 b 68 173 55 174 61 174 l 287 112 l 551 40 b 615 20 617 22 609 23 b 626 0 622 16 626 8 b 615 -22 626 -9 622 -18 b 613 -23 613 -23 613 -23 b 613 -23 613 -23 613 -23 b 287 -113 613 -24 597 -29 l 68 -174 b 53 -176 61 -176 57 -176 b 39 -172 47 -176 43 -174 b 27 -151 31 -167 27 -159 b 39 -129 27 -141 31 -133 b 230 -74 43 -124 20 -131 l 370 -36 l 468 -9 b 498 0 484 -4 498 -1 b 468 8 498 0 484 2 l 370 34 l 230 73 b 40 126 28 129 43 124 b 27 149 31 131 27 140 b 44 174 27 161 34 170 m 205 110 l 205 300 b 205 330 245 330 245 300 l 245 300 l 245 -300 b 245 -330 205 -330 205 -300 l 205 -300 l 205 110 l 345 90 m 345 90 l 345 330 b 345 360 385 360 385 330 l 385 330 l 385 -270 b 385 -300 345 -300 345 -270 l 345 -270 l 345 90 " },
    "vd1": { "x_min": -20, "x_max": 320, "ha": 257, "o": "m -8 200 b -8 210 8 200 16 200 l 20 148 -199 l 23 -615 b 0 -629 9 -629 6 -629 l -21 -612 l -21 -201 l -21 216 l -20 200 m 16 200 l 310 0 l 240 0 l 16 140 l 16 -120 l 240 0 l 310 0 l 16 -200 " }
  },
  "cssFontWeight": "normal", "ascender": 1903, "underlinePosition": -125, "cssFontStyle": "normal", "boundingBox": { "yMin": -2065.375, "xMin": -695.53125, "yMax": 1901.578125, "xMax": 1159.671875 },
  "resolution": 1000, "descender": -2066, "familyName": "VexFlow-18", "lineHeight": 4093, "underlineThickness": 50
};

/***/ }),
/* 41 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NoteHead = undefined;

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _note = __webpack_require__(6);

var _stem = __webpack_require__(9);

var _stavenote = __webpack_require__(5);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements `NoteHeads`. `NoteHeads` are typically not manipulated
// directly, but used internally in `StaveNote`.
//
// See `tests/notehead_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.NoteHead.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (NoteHead.DEBUG) _vex.Vex.L('Vex.Flow.NoteHead', args);
}

// Draw slashnote head manually. No glyph exists for this.
//
// Parameters:
// * `ctx`: the Canvas context
// * `duration`: the duration of the note. ex: "4"
// * `x`: the x coordinate to draw at
// * `y`: the y coordinate to draw at
// * `stem_direction`: the direction of the stem
function drawSlashNoteHead(ctx, duration, x, y, stem_direction, staveSpace) {
  var width = _tables.Flow.SLASH_NOTEHEAD_WIDTH;
  ctx.save();
  ctx.setLineWidth(_tables.Flow.STEM_WIDTH);

  var fill = false;

  if (_tables.Flow.durationToNumber(duration) > 2) {
    fill = true;
  }

  if (!fill) x -= _tables.Flow.STEM_WIDTH / 2 * stem_direction;

  ctx.beginPath();
  ctx.moveTo(x, y + staveSpace);
  ctx.lineTo(x, y + 1);
  ctx.lineTo(x + width, y - staveSpace);
  ctx.lineTo(x + width, y);
  ctx.lineTo(x, y + staveSpace);
  ctx.closePath();

  if (fill) {
    ctx.fill();
  } else {
    ctx.stroke();
  }

  if (_tables.Flow.durationToFraction(duration).equals(0.5)) {
    var breve_lines = [-3, -1, width + 1, width + 3];
    for (var i = 0; i < breve_lines.length; i++) {
      ctx.beginPath();
      ctx.moveTo(x + breve_lines[i], y - 10);
      ctx.lineTo(x + breve_lines[i], y + 11);
      ctx.stroke();
    }
  }

  ctx.restore();
}

var NoteHead = exports.NoteHead = function (_Note) {
  _inherits(NoteHead, _Note);

  _createClass(NoteHead, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'notehead';
    }
  }]);

  function NoteHead(head_options) {
    _classCallCheck(this, NoteHead);

    var _this = _possibleConstructorReturn(this, (NoteHead.__proto__ || Object.getPrototypeOf(NoteHead)).call(this, head_options));

    _this.setAttribute('type', 'NoteHead');

    _this.index = head_options.index;
    _this.x = head_options.x || 0;
    _this.y = head_options.y || 0;
    _this.note_type = head_options.note_type;
    _this.duration = head_options.duration;
    _this.displaced = head_options.displaced || false;
    _this.stem_direction = head_options.stem_direction || _stavenote.StaveNote.STEM_UP;
    _this.line = head_options.line;

    // Get glyph code based on duration and note type. This could be
    // regular notes, rests, or other custom codes.
    _this.glyph = _tables.Flow.durationToGlyph(_this.duration, _this.note_type);
    if (!_this.glyph) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'No glyph found for duration \'' + _this.duration + '\' and type \'' + _this.note_type + '\'');
    }

    _this.glyph_code = _this.glyph.code_head;
    _this.x_shift = head_options.x_shift;
    if (head_options.custom_glyph_code) {
      _this.custom_glyph = true;
      _this.glyph_code = head_options.custom_glyph_code;
    }

    _this.style = head_options.style;
    _this.slashed = head_options.slashed;

    _vex.Vex.Merge(_this.render_options, {
      // font size for note heads
      glyph_font_scale: head_options.glyph_font_scale || _tables.Flow.DEFAULT_NOTATION_FONT_SCALE,
      // number of stroke px to the left and right of head
      stroke_px: 3
    });

    _this.setWidth(_this.glyph.getWidth(_this.render_options.glyph_font_scale));
    return _this;
  }

  _createClass(NoteHead, [{
    key: 'getCategory',
    value: function getCategory() {
      return NoteHead.CATEGORY;
    }

    // Get the width of the notehead

  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }

    // Determine if the notehead is displaced

  }, {
    key: 'isDisplaced',
    value: function isDisplaced() {
      return this.displaced === true;
    }

    // Get the glyph data

  }, {
    key: 'getGlyph',
    value: function getGlyph() {
      return this.glyph;
    }

    // Set the X coordinate

  }, {
    key: 'setX',
    value: function setX(x) {
      this.x = x;return this;
    }

    // get/set the Y coordinate

  }, {
    key: 'getY',
    value: function getY() {
      return this.y;
    }
  }, {
    key: 'setY',
    value: function setY(y) {
      this.y = y;return this;
    }

    // Get/set the stave line the notehead is placed on

  }, {
    key: 'getLine',
    value: function getLine() {
      return this.line;
    }
  }, {
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;return this;
    }

    // Get the canvas `x` coordinate position of the notehead.

  }, {
    key: 'getAbsoluteX',
    value: function getAbsoluteX() {
      // If the note has not been preformatted, then get the static x value
      // Otherwise, it's been formatted and we should use it's x value relative
      // to its tick context
      var x = !this.preFormatted ? this.x : _get(NoteHead.prototype.__proto__ || Object.getPrototypeOf(NoteHead.prototype), 'getAbsoluteX', this).call(this);

      // For a more natural displaced notehead, we adjust the displacement amount
      // by half the stem width in order to maintain a slight overlap with the stem
      var displacementStemAdjustment = _stem.Stem.WIDTH / 2;

      return x + (this.displaced ? (this.width - displacementStemAdjustment) * this.stem_direction : 0);
    }

    // Get the `BoundingBox` for the `NoteHead`

  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      if (!this.preFormatted) {
        throw new _vex.Vex.RERR('UnformattedNote', "Can't call getBoundingBox on an unformatted note.");
      }

      var spacing = this.stave.getSpacingBetweenLines();
      var half_spacing = spacing / 2;
      var min_y = this.y - half_spacing;

      return new _tables.Flow.BoundingBox(this.getAbsoluteX(), min_y, this.width, spacing);
    }

    // Set notehead to a provided `stave`

  }, {
    key: 'setStave',
    value: function setStave(stave) {
      var line = this.getLine();

      this.stave = stave;
      this.setY(stave.getYForNote(line));
      this.context = this.stave.context;
      return this;
    }

    // Pre-render formatting

  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return this;

      var width = this.getWidth() + this.extraLeftPx + this.extraRightPx;

      this.setWidth(width);
      this.setPreFormatted(true);
      return this;
    }

    // Draw the notehead

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      var ctx = this.context;
      var head_x = this.getAbsoluteX();
      var y = this.y;

      L("Drawing note head '", this.note_type, this.duration, "' at", head_x, y);

      // Begin and end positions for head.
      var stem_direction = this.stem_direction;
      var glyph_font_scale = this.render_options.glyph_font_scale;

      if (this.style) {
        this.applyStyle(ctx);
      }

      if (this.note_type === 's') {
        var staveSpace = this.stave.getSpacingBetweenLines();
        drawSlashNoteHead(ctx, this.duration, head_x, y, stem_direction, staveSpace);
      } else {
        _glyph.Glyph.renderGlyph(ctx, head_x, y, glyph_font_scale, this.glyph_code);
      }

      if (this.style) {
        this.restoreStyle(ctx);
      }
    }
  }]);

  return NoteHead;
}(_note.Note);

/***/ }),
/* 42 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Tickable = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _tables = __webpack_require__(1);

var _fraction = __webpack_require__(8);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// The tickable interface. Tickables are things that sit on a score and
// have a duration, i.e., they occupy space in the musical rendering dimension.

var Tickable = exports.Tickable = function (_Element) {
  _inherits(Tickable, _Element);

  function Tickable() {
    _classCallCheck(this, Tickable);

    var _this = _possibleConstructorReturn(this, (Tickable.__proto__ || Object.getPrototypeOf(Tickable)).call(this));

    _this.setAttribute('type', 'Tickable');

    // These properties represent the duration of
    // this tickable element.
    _this.ticks = new _fraction.Fraction(0, 1);
    _this.intrinsicTicks = 0;
    _this.tickMultiplier = new _fraction.Fraction(1, 1);

    _this.width = 0;
    _this.x_shift = 0; // Shift from tick context
    _this.voice = null;
    _this.tickContext = null;
    _this.modifierContext = null;
    _this.modifiers = [];
    _this.preFormatted = false;
    _this.postFormatted = false;
    _this.tuplet = null;
    _this.tupletStack = [];

    _this.align_center = false;
    _this.center_x_shift = 0; // Shift from tick context if center aligned

    // This flag tells the formatter to ignore this tickable during
    // formatting and justification. It is set by tickables such as BarNote.
    _this.ignore_ticks = false;

    // This is a space for an external formatting class or function to maintain
    // metrics.
    _this.formatterMetrics = {
      // The freedom of a tickable is the distance it can move without colliding
      // with neighboring elements. A formatter can set these values during its
      // formatting pass, which a different formatter can then use to fine tune.
      freedom: { left: 0, right: 0 },

      // The simplified rational duration of this tick as a string. It can be
      // used as an index to a map or hashtable.
      duration: '',

      // The number of formatting iterations undergone.
      iterations: 0,

      // The space in pixels allocated by this formatter, along with the mean space
      // for tickables of this duration, and the deviation from the mean.
      space: {
        used: 0,
        mean: 0,
        deviation: 0
      }
    };
    return _this;
  }

  _createClass(Tickable, [{
    key: 'reset',
    value: function reset() {
      return this;
    }
  }, {
    key: 'getTicks',
    value: function getTicks() {
      return this.ticks;
    }
  }, {
    key: 'shouldIgnoreTicks',
    value: function shouldIgnoreTicks() {
      return this.ignore_ticks;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      return this.width;
    }
  }, {
    key: 'getFormatterMetrics',
    value: function getFormatterMetrics() {
      return this.formatterMetrics;
    }
  }, {
    key: 'setXShift',
    value: function setXShift(x) {
      this.x_shift = x;
    }
  }, {
    key: 'getCenterXShift',
    value: function getCenterXShift() {
      if (this.isCenterAligned()) {
        return this.center_x_shift;
      }

      return 0;
    }
  }, {
    key: 'isCenterAligned',
    value: function isCenterAligned() {
      return this.align_center;
    }
  }, {
    key: 'setCenterAlignment',
    value: function setCenterAlignment(align_center) {
      this.align_center = align_center;
      return this;
    }

    // Every tickable must be associated with a voice. This allows formatters
    // and preFormatter to associate them with the right modifierContexts.

  }, {
    key: 'getVoice',
    value: function getVoice() {
      if (!this.voice) throw new _vex.Vex.RERR('NoVoice', 'Tickable has no voice.');
      return this.voice;
    }
  }, {
    key: 'setVoice',
    value: function setVoice(voice) {
      this.voice = voice;
    }
  }, {
    key: 'getTuplet',
    value: function getTuplet() {
      return this.tuplet;
    }

    /*
     * resetTuplet
     * @param tuplet -- the specific tuplet to reset
     *   if this is not provided, all tuplets are reset.
     * @returns this
     *
     * Removes any prior tuplets from the tick calculation and
     * resets the intrinsic tick value to
     */

  }, {
    key: 'resetTuplet',
    value: function resetTuplet(tuplet) {
      var noteCount = void 0;
      var notesOccupied = void 0;
      if (tuplet) {
        var i = this.tupletStack.indexOf(tuplet);
        if (i !== -1) {
          this.tupletStack.splice(i, 1);
          noteCount = tuplet.getNoteCount();
          notesOccupied = tuplet.getNotesOccupied();

          // Revert old multiplier by inverting numerator & denom.:
          this.applyTickMultiplier(noteCount, notesOccupied);
        }
        return this;
      }

      while (this.tupletStack.length) {
        tuplet = this.tupletStack.pop();
        noteCount = tuplet.getNoteCount();
        notesOccupied = tuplet.getNotesOccupied();

        // Revert old multiplier by inverting numerator & denom.:
        this.applyTickMultiplier(noteCount, notesOccupied);
      }
      return this;
    }
  }, {
    key: 'setTuplet',
    value: function setTuplet(tuplet) {
      // Attach to new tuplet

      if (tuplet) {
        this.tupletStack.push(tuplet);

        var noteCount = tuplet.getNoteCount();
        var notesOccupied = tuplet.getNotesOccupied();

        this.applyTickMultiplier(notesOccupied, noteCount);
      }

      this.tuplet = tuplet;

      return this;
    }

    /** optional, if tickable has modifiers **/

  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext(mc) {
      this.modifierContext = mc;
      // Add modifiers to modifier context (if any)
      this.preFormatted = false;
    }

    /** optional, if tickable has modifiers **/

  }, {
    key: 'addModifier',
    value: function addModifier(mod) {
      this.modifiers.push(mod);
      this.preFormatted = false;
      return this;
    }
  }, {
    key: 'getModifiers',
    value: function getModifiers() {
      return this.modifiers;
    }
  }, {
    key: 'setTickContext',
    value: function setTickContext(tc) {
      this.tickContext = tc;
      this.preFormatted = false;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      if (this.preFormatted) return;

      this.width = 0;
      if (this.modifierContext) {
        this.modifierContext.preFormat();
        this.width += this.modifierContext.getWidth();
      }
    }
  }, {
    key: 'postFormat',
    value: function postFormat() {
      if (this.postFormatted) return this;
      this.postFormatted = true;
      return this;
    }
  }, {
    key: 'getIntrinsicTicks',
    value: function getIntrinsicTicks() {
      return this.intrinsicTicks;
    }
  }, {
    key: 'setIntrinsicTicks',
    value: function setIntrinsicTicks(intrinsicTicks) {
      this.intrinsicTicks = intrinsicTicks;
      this.ticks = this.tickMultiplier.clone().multiply(this.intrinsicTicks);
    }
  }, {
    key: 'getTickMultiplier',
    value: function getTickMultiplier() {
      return this.tickMultiplier;
    }
  }, {
    key: 'applyTickMultiplier',
    value: function applyTickMultiplier(numerator, denominator) {
      this.tickMultiplier.multiply(numerator, denominator);
      this.ticks = this.tickMultiplier.clone().multiply(this.intrinsicTicks);
    }
  }, {
    key: 'setDuration',
    value: function setDuration(duration) {
      var ticks = duration.numerator * (_tables.Flow.RESOLUTION / duration.denominator);
      this.ticks = this.tickMultiplier.clone().multiply(ticks);
      this.intrinsicTicks = this.ticks.value();
    }
  }]);

  return Tickable;
}(_element.Element);

/***/ }),
/* 43 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Stroke = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _modifier = __webpack_require__(4);

var _stavenote = __webpack_require__(5);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Larry Kuhns
//
// ## Description
// This file implements the `Stroke` class which renders chord strokes
// that can be arpeggiated, brushed, rasquedo, etc.

var Stroke = exports.Stroke = function (_Modifier) {
  _inherits(Stroke, _Modifier);

  _createClass(Stroke, null, [{
    key: 'format',


    // Arrange strokes inside `ModifierContext`
    value: function format(strokes, state) {
      var left_shift = state.left_shift;
      var stroke_spacing = 0;

      if (!strokes || strokes.length === 0) return this;

      var strokeList = strokes.map(function (stroke) {
        var note = stroke.getNote();
        if (note instanceof _stavenote.StaveNote) {
          var _note$getKeyProps$str = note.getKeyProps()[stroke.getIndex()],
              line = _note$getKeyProps$str.line,
              displaced = _note$getKeyProps$str.displaced;

          var shift = displaced ? note.getExtraLeftPx() : 0;
          return { line: line, shift: shift, stroke: stroke };
        } else {
          var string = note.getPositions()[stroke.getIndex()].str;

          return { line: string, shift: 0, stroke: stroke };
        }
      });

      var strokeShift = left_shift;

      // There can only be one stroke .. if more than one, they overlay each other
      var xShift = strokeList.reduce(function (xShift, _ref) {
        var stroke = _ref.stroke,
            shift = _ref.shift;

        stroke.setXShift(strokeShift + shift);
        return Math.max(stroke.getWidth() + stroke_spacing, xShift);
      }, 0);

      state.left_shift += xShift;
      return true;
    }
  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'strokes';
    }
  }, {
    key: 'Type',
    get: function get() {
      return {
        BRUSH_DOWN: 1,
        BRUSH_UP: 2,
        ROLL_DOWN: 3, // Arpegiated chord
        ROLL_UP: 4, // Arpegiated chord
        RASQUEDO_DOWN: 5,
        RASQUEDO_UP: 6
      };
    }
  }]);

  function Stroke(type, options) {
    _classCallCheck(this, Stroke);

    var _this = _possibleConstructorReturn(this, (Stroke.__proto__ || Object.getPrototypeOf(Stroke)).call(this));

    _this.setAttribute('type', 'Stroke');

    _this.note = null;
    _this.options = _vex.Vex.Merge({}, options);

    // multi voice - span stroke across all voices if true
    _this.all_voices = 'all_voices' in _this.options ? _this.options.all_voices : true;

    // multi voice - end note of stroke, set in draw()
    _this.note_end = null;
    _this.index = null;
    _this.type = type;
    _this.position = _modifier.Modifier.Position.LEFT;

    _this.render_options = {
      font_scale: 38,
      stroke_px: 3,
      stroke_spacing: 10
    };

    _this.font = {
      family: 'serif',
      size: 10,
      weight: 'bold italic'
    };

    _this.setXShift(0);
    _this.setWidth(10);
    return _this;
  }

  _createClass(Stroke, [{
    key: 'getCategory',
    value: function getCategory() {
      return Stroke.CATEGORY;
    }
  }, {
    key: 'getPosition',
    value: function getPosition() {
      return this.position;
    }
  }, {
    key: 'addEndNote',
    value: function addEndNote(note) {
      this.note_end = note;return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      if (!(this.note && this.index != null)) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw stroke without a note and index.");
      }

      var start = this.note.getModifierStartXY(this.position, this.index);
      var ys = this.note.getYs();
      var topY = start.y;
      var botY = start.y;
      var x = start.x - 5;
      var line_space = this.note.stave.options.spacing_between_lines_px;

      var notes = this.getModifierContext().getModifiers(this.note.getCategory());
      for (var i = 0; i < notes.length; i++) {
        ys = notes[i].getYs();
        for (var n = 0; n < ys.length; n++) {
          if (this.note === notes[i] || this.all_voices) {
            topY = _vex.Vex.Min(topY, ys[n]);
            botY = _vex.Vex.Max(botY, ys[n]);
          }
        }
      }

      var arrow = void 0;
      var arrow_shift_x = void 0;
      var arrow_y = void 0;
      var text_shift_x = void 0;
      var text_y = void 0;
      switch (this.type) {
        case Stroke.Type.BRUSH_DOWN:
          arrow = 'vc3';
          arrow_shift_x = -3;
          arrow_y = topY - line_space / 2 + 10;
          botY += line_space / 2;
          break;
        case Stroke.Type.BRUSH_UP:
          arrow = 'v11';
          arrow_shift_x = 0.5;
          arrow_y = botY + line_space / 2;
          topY -= line_space / 2;
          break;
        case Stroke.Type.ROLL_DOWN:
        case Stroke.Type.RASQUEDO_DOWN:
          arrow = 'vc3';
          arrow_shift_x = -3;
          text_shift_x = this.x_shift + arrow_shift_x - 2;
          if (this.note instanceof _stavenote.StaveNote) {
            topY += 1.5 * line_space;
            if ((botY - topY) % 2 !== 0) {
              botY += 0.5 * line_space;
            } else {
              botY += line_space;
            }
            arrow_y = topY - line_space;
            text_y = botY + line_space + 2;
          } else {
            topY += 1.5 * line_space;
            botY += line_space;
            arrow_y = topY - 0.75 * line_space;
            text_y = botY + 0.25 * line_space;
          }
          break;
        case Stroke.Type.ROLL_UP:
        case Stroke.Type.RASQUEDO_UP:
          arrow = 'v52';
          arrow_shift_x = -4;
          text_shift_x = this.x_shift + arrow_shift_x - 1;
          if (this.note instanceof _stavenote.StaveNote) {
            arrow_y = line_space / 2;
            topY += 0.5 * line_space;
            if ((botY - topY) % 2 === 0) {
              botY += line_space / 2;
            }
            arrow_y = botY + 0.5 * line_space;
            text_y = topY - 1.25 * line_space;
          } else {
            topY += 0.25 * line_space;
            botY += 0.5 * line_space;
            arrow_y = botY + 0.25 * line_space;
            text_y = topY - line_space;
          }
          break;
        default:
          throw new _vex.Vex.RERR('InvalidType', 'The stroke type ' + this.type + ' does not exist');
      }

      // Draw the stroke
      if (this.type === Stroke.Type.BRUSH_DOWN || this.type === Stroke.Type.BRUSH_UP) {
        this.context.fillRect(x + this.x_shift, topY, 1, botY - topY);
      } else {
        if (this.note instanceof _stavenote.StaveNote) {
          for (var _i = topY; _i <= botY; _i += line_space) {
            _glyph.Glyph.renderGlyph(this.context, x + this.x_shift - 4, _i, this.render_options.font_scale, 'va3');
          }
        } else {
          var _i2 = void 0;
          for (_i2 = topY; _i2 <= botY; _i2 += 10) {
            _glyph.Glyph.renderGlyph(this.context, x + this.x_shift - 4, _i2, this.render_options.font_scale, 'va3');
          }
          if (this.type === Stroke.Type.RASQUEDO_DOWN) {
            text_y = _i2 + 0.25 * line_space;
          }
        }
      }

      // Draw the arrow head
      _glyph.Glyph.renderGlyph(this.context, x + this.x_shift + arrow_shift_x, arrow_y, this.render_options.font_scale, arrow);

      // Draw the rasquedo "R"
      if (this.type === Stroke.Type.RASQUEDO_DOWN || this.type === Stroke.Type.RASQUEDO_UP) {
        this.context.save();
        this.context.setFont(this.font.family, this.font.size, this.font.weight);
        this.context.fillText('R', x + text_shift_x, text_y);
        this.context.restore();
      }
    }
  }]);

  return Stroke;
}(_modifier.Modifier);

/***/ }),
/* 44 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Ornament = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

var _tickcontext = __webpack_require__(13);

var _stavenote = __webpack_require__(5);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Cyril Silverman
//
// ## Description
//
// This file implements ornaments as modifiers that can be
// attached to notes. The complete list of ornaments is available in
// `tables.js` under `Vex.Flow.ornamentCodes`.
//
// See `tests/ornament_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.Ornament.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Ornament.DEBUG) _vex.Vex.L('Vex.Flow.Ornament', args);
}

var Ornament = exports.Ornament = function (_Modifier) {
  _inherits(Ornament, _Modifier);

  _createClass(Ornament, null, [{
    key: 'format',


    // ## Static Methods
    // Arrange ornaments inside `ModifierContext`
    value: function format(ornaments, state) {
      if (!ornaments || ornaments.length === 0) return false;

      var width = 0;
      for (var i = 0; i < ornaments.length; ++i) {
        var ornament = ornaments[i];
        var increment = 2;

        width = Math.max(ornament.getWidth(), width);

        if (ornament.getPosition() === _modifier.Modifier.Position.ABOVE) {
          ornament.setTextLine(state.top_text_line);
          state.top_text_line += increment;
        } else {
          ornament.setTextLine(state.text_line);
          state.text_line += increment;
        }
      }

      state.left_shift += width / 2;
      state.right_shift += width / 2;
      return true;
    }

    // Create a new ornament of type `type`, which is an entry in
    // `Vex.Flow.ornamentCodes` in `tables.js`.

  }, {
    key: 'CATEGORY',
    get: function get() {
      return 'ornaments';
    }
  }]);

  function Ornament(type) {
    _classCallCheck(this, Ornament);

    var _this = _possibleConstructorReturn(this, (Ornament.__proto__ || Object.getPrototypeOf(Ornament)).call(this));

    _this.setAttribute('type', 'Ornament');

    _this.note = null;
    _this.index = null;
    _this.type = type;
    _this.position = _modifier.Modifier.Position.ABOVE;
    _this.delayed = false;

    _this.accidentalUpper = null;
    _this.accidentalLower = null;

    _this.render_options = {
      font_scale: 38,
      accidentalLowerPadding: 3,
      accidentalUpperPadding: 3
    };

    _this.ornament = _tables.Flow.ornamentCodes(_this.type);
    if (!_this.ornament) {
      throw new _vex.Vex.RERR('ArgumentError', 'Ornament not found: \'' + _this.type + '\'');
    }

    _this.glyph = new _glyph.Glyph(_this.ornament.code, _this.render_options.font_scale);
    _this.glyph.setOrigin(0.5, 1.0); // FIXME: SMuFL won't require a vertical origin shift
    return _this;
  }

  _createClass(Ornament, [{
    key: 'getCategory',
    value: function getCategory() {
      return Ornament.CATEGORY;
    }

    // Set whether the ornament is to be delayed

  }, {
    key: 'setDelayed',
    value: function setDelayed(delayed) {
      this.delayed = delayed;return this;
    }

    // Set the upper accidental for the ornament

  }, {
    key: 'setUpperAccidental',
    value: function setUpperAccidental(accid) {
      var scale = this.render_options.font_scale / 1.3;
      this.accidentalUpper = new _glyph.Glyph(_tables.Flow.accidentalCodes(accid).code, scale);
      this.accidentalUpper.setOrigin(0.5, 1.0);
      return this;
    }

    // Set the lower accidental for the ornament

  }, {
    key: 'setLowerAccidental',
    value: function setLowerAccidental(accid) {
      var scale = this.render_options.font_scale / 1.3;
      this.accidentalLower = new _glyph.Glyph(_tables.Flow.accidentalCodes(accid).code, scale);
      this.accidentalLower.setOrigin(0.5, 1.0);
      return this;
    }

    // Render ornament in position next to note.

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();

      if (!this.note || this.index == null) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw Ornament without a note and index.");
      }

      this.setRendered();

      var ctx = this.context;
      var stemDir = this.note.getStemDirection();
      var stave = this.note.getStave();

      // Get stem extents
      var stemExtents = this.note.getStem().getExtents();
      var y = stemDir === _stavenote.StaveNote.STEM_DOWN ? stemExtents.baseY : stemExtents.topY;

      // TabNotes don't have stems attached to them. Tab stems are rendered
      // outside the stave.
      if (this.note.getCategory() === 'tabnotes') {
        if (this.note.hasStem()) {
          if (stemDir === _stavenote.StaveNote.STEM_DOWN) {
            y = stave.getYForTopText(this.text_line);
          }
        } else {
          // Without a stem
          y = stave.getYForTopText(this.text_line);
        }
      }

      var isPlacedOnNoteheadSide = stemDir === _stavenote.StaveNote.STEM_DOWN;
      var spacing = stave.getSpacingBetweenLines();
      var lineSpacing = 1;

      // Beamed stems are longer than quarter note stems, adjust accordingly
      if (!isPlacedOnNoteheadSide && this.note.beam) {
        lineSpacing += 0.5;
      }

      var totalSpacing = spacing * (this.text_line + lineSpacing);
      var glyphYBetweenLines = y - totalSpacing;

      // Get initial coordinates for the modifier position
      var start = this.note.getModifierStartXY(this.position, this.index);
      var glyphX = start.x;
      var glyphY = Math.min(stave.getYForTopText(this.text_line), glyphYBetweenLines);
      glyphY += this.y_shift;

      // Ajdust x position if ornament is delayed
      if (this.delayed) {
        var delayXShift = 0;
        if (this.delayXShift !== undefined) {
          delayXShift = this.delayXShift;
        } else {
          delayXShift += this.glyph.getMetrics().width / 2;
          var nextContext = _tickcontext.TickContext.getNextContext(this.note.getTickContext());
          if (nextContext) {
            delayXShift += (nextContext.getX() - glyphX) * 0.5;
          } else {
            delayXShift += (stave.x + stave.width - glyphX) * 0.5;
          }
          this.delayXShift = delayXShift;
        }
        glyphX += delayXShift;
      }

      L('Rendering ornament: ', this.ornament, glyphX, glyphY);

      if (this.accidentalLower) {
        this.accidentalLower.render(ctx, glyphX, glyphY);
        glyphY -= this.accidentalLower.getMetrics().height;
        glyphY -= this.render_options.accidentalLowerPadding;
      }

      this.glyph.render(ctx, glyphX, glyphY);
      glyphY -= this.glyph.getMetrics().height;

      if (this.accidentalUpper) {
        glyphY -= this.render_options.accidentalUpperPadding;
        this.accidentalUpper.render(ctx, glyphX, glyphY);
      }
    }
  }]);

  return Ornament;
}(_modifier.Modifier);

/***/ }),
/* 45 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Repetition = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _stavemodifier = __webpack_require__(7);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Larry Kuhns 2011

var Repetition = exports.Repetition = function (_StaveModifier) {
  _inherits(Repetition, _StaveModifier);

  _createClass(Repetition, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'repetitions';
    }
  }, {
    key: 'type',
    get: function get() {
      return {
        NONE: 1, // no coda or segno
        CODA_LEFT: 2, // coda at beginning of stave
        CODA_RIGHT: 3, // coda at end of stave
        SEGNO_LEFT: 4, // segno at beginning of stave
        SEGNO_RIGHT: 5, // segno at end of stave
        DC: 6, // D.C. at end of stave
        DC_AL_CODA: 7, // D.C. al coda at end of stave
        DC_AL_FINE: 8, // D.C. al Fine end of stave
        DS: 9, // D.S. at end of stave
        DS_AL_CODA: 10, // D.S. al coda at end of stave
        DS_AL_FINE: 11, // D.S. al Fine at end of stave
        FINE: 12 // Fine at end of stave
      };
    }
  }]);

  function Repetition(type, x, y_shift) {
    _classCallCheck(this, Repetition);

    var _this = _possibleConstructorReturn(this, (Repetition.__proto__ || Object.getPrototypeOf(Repetition)).call(this));

    _this.setAttribute('type', 'Repetition');

    _this.symbol_type = type;
    _this.x = x;
    _this.x_shift = 0;
    _this.y_shift = y_shift;
    _this.font = {
      family: 'times',
      size: 12,
      weight: 'bold italic'
    };
    return _this;
  }

  _createClass(Repetition, [{
    key: 'getCategory',
    value: function getCategory() {
      return Repetition.CATEGORY;
    }
  }, {
    key: 'setShiftX',
    value: function setShiftX(x) {
      this.x_shift = x;return this;
    }
  }, {
    key: 'setShiftY',
    value: function setShiftY(y) {
      this.y_shift = y;return this;
    }
  }, {
    key: 'draw',
    value: function draw(stave, x) {
      this.setRendered();

      switch (this.symbol_type) {
        case Repetition.type.CODA_RIGHT:
          this.drawCodaFixed(stave, x + stave.width);
          break;
        case Repetition.type.CODA_LEFT:
          this.drawSymbolText(stave, x, 'Coda', true);
          break;
        case Repetition.type.SEGNO_LEFT:
          this.drawSignoFixed(stave, x);
          break;
        case Repetition.type.SEGNO_RIGHT:
          this.drawSignoFixed(stave, x + stave.width);
          break;
        case Repetition.type.DC:
          this.drawSymbolText(stave, x, 'D.C.', false);
          break;
        case Repetition.type.DC_AL_CODA:
          this.drawSymbolText(stave, x, 'D.C. al', true);
          break;
        case Repetition.type.DC_AL_FINE:
          this.drawSymbolText(stave, x, 'D.C. al Fine', false);
          break;
        case Repetition.type.DS:
          this.drawSymbolText(stave, x, 'D.S.', false);
          break;
        case Repetition.type.DS_AL_CODA:
          this.drawSymbolText(stave, x, 'D.S. al', true);
          break;
        case Repetition.type.DS_AL_FINE:
          this.drawSymbolText(stave, x, 'D.S. al Fine', false);
          break;
        case Repetition.type.FINE:
          this.drawSymbolText(stave, x, 'Fine', false);
          break;
        default:
          break;
      }

      return this;
    }
  }, {
    key: 'drawCodaFixed',
    value: function drawCodaFixed(stave, x) {
      var y = stave.getYForTopText(stave.options.num_lines) + this.y_shift;
      _glyph.Glyph.renderGlyph(stave.context, this.x + x + this.x_shift, y + 25, 40, 'v4d', true);
      return this;
    }
  }, {
    key: 'drawSignoFixed',
    value: function drawSignoFixed(stave, x) {
      var y = stave.getYForTopText(stave.options.num_lines) + this.y_shift;
      _glyph.Glyph.renderGlyph(stave.context, this.x + x + this.x_shift, y + 25, 30, 'v8c', true);
      return this;
    }
  }, {
    key: 'drawSymbolText',
    value: function drawSymbolText(stave, x, text, draw_coda) {
      var ctx = stave.checkContext();

      ctx.save();
      ctx.setFont(this.font.family, this.font.size, this.font.weight);
      // Default to right symbol
      var text_x = 0 + this.x_shift;
      var symbol_x = x + this.x_shift;
      if (this.symbol_type === Repetition.type.CODA_LEFT) {
        // Offset Coda text to right of stave beginning
        text_x = this.x + stave.options.vertical_bar_width;
        symbol_x = text_x + ctx.measureText(text).width + 12;
      } else {
        // Offset Signo text to left stave end
        symbol_x = this.x + x + stave.width - 5 + this.x_shift;
        text_x = symbol_x - +ctx.measureText(text).width - 12;
      }

      var y = stave.getYForTopText(stave.options.num_lines) + this.y_shift;
      if (draw_coda) {
        _glyph.Glyph.renderGlyph(ctx, symbol_x, y, 40, 'v4d', true);
      }

      ctx.fillText(text, text_x, y + 5);
      ctx.restore();

      return this;
    }
  }]);

  return Repetition;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 46 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.KeySignature = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _stavemodifier = __webpack_require__(7);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Cyril Silverman
//
// ## Description
//
// This file implements key signatures. A key signature sits on a stave
// and indicates the notes with implicit accidentals.

var KeySignature = exports.KeySignature = function (_StaveModifier) {
  _inherits(KeySignature, _StaveModifier);

  _createClass(KeySignature, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'keysignatures';
    }

    // Space between natural and following accidental depending
    // on vertical position

  }, {
    key: 'accidentalSpacing',
    get: function get() {
      return {
        '#': {
          above: 6,
          below: 4
        },
        'b': {
          above: 4,
          below: 7
        },
        'n': {
          above: 4,
          below: 1
        },
        '##': {
          above: 6,
          below: 4
        },
        'bb': {
          above: 4,
          below: 7
        },
        'db': {
          above: 4,
          below: 7
        },
        'd': {
          above: 4,
          below: 7
        },
        'bbs': {
          above: 4,
          below: 7
        },
        '++': {
          above: 6,
          below: 4
        },
        '+': {
          above: 6,
          below: 4
        },
        '+-': {
          above: 6,
          below: 4
        },
        '++-': {
          above: 6,
          below: 4
        },
        'bs': {
          above: 4,
          below: 10
        },
        'bss': {
          above: 4,
          below: 10
        }
      };
    }

    // Create a new Key Signature based on a `key_spec`

  }]);

  function KeySignature(keySpec, cancelKeySpec, alterKeySpec) {
    _classCallCheck(this, KeySignature);

    var _this = _possibleConstructorReturn(this, (KeySignature.__proto__ || Object.getPrototypeOf(KeySignature)).call(this));

    _this.setAttribute('type', 'KeySignature');

    _this.setKeySig(keySpec, cancelKeySpec, alterKeySpec);
    _this.setPosition(_stavemodifier.StaveModifier.Position.BEGIN);
    _this.glyphFontScale = 38; // TODO(0xFE): Should this match StaveNote?
    _this.glyphs = [];
    _this.xPositions = []; // relative to this.x
    _this.paddingForced = false;
    return _this;
  }

  _createClass(KeySignature, [{
    key: 'getCategory',
    value: function getCategory() {
      return KeySignature.CATEGORY;
    }

    // Add an accidental glyph to the `KeySignature` instance which represents
    // the provided `acc`. If `nextAcc` is also provided, the appropriate
    // spacing will be included in the glyph's position

  }, {
    key: 'convertToGlyph',
    value: function convertToGlyph(acc, nextAcc) {
      var accGlyphData = _tables.Flow.accidentalCodes(acc.type);
      var glyph = new _glyph.Glyph(accGlyphData.code, this.glyphFontScale);

      // Determine spacing between current accidental and the next accidental
      var extraWidth = 1;
      if (acc.type === 'n' && nextAcc) {
        var spacing = KeySignature.accidentalSpacing[nextAcc.type];
        if (spacing) {
          var isAbove = nextAcc.line >= acc.line;
          extraWidth = isAbove ? spacing.above : spacing.below;
        }
      }

      // Place the glyph on the stave
      this.placeGlyphOnLine(glyph, this.stave, acc.line);
      this.glyphs.push(glyph);

      var xPosition = this.xPositions[this.xPositions.length - 1];
      var glyphWidth = glyph.getMetrics().width + extraWidth;
      // Store the next accidental's x position
      this.xPositions.push(xPosition + glyphWidth);
      // Expand size of key signature
      this.width += glyphWidth;
    }

    // Cancel out a key signature provided in the `spec` parameter. This will
    // place appropriate natural accidentals before the key signature.

  }, {
    key: 'cancelKey',
    value: function cancelKey(spec) {
      this.formatted = false;
      this.cancelKeySpec = spec;

      return this;
    }
  }, {
    key: 'convertToCancelAccList',
    value: function convertToCancelAccList(spec) {
      // Get the accidental list for the cancelled key signature
      var cancel_accList = _tables.Flow.keySignature(spec);

      // If the cancelled key has a different accidental type, ie: # vs b
      var different_types = this.accList.length > 0 && cancel_accList.length > 0 && cancel_accList[0].type !== this.accList[0].type;

      // Determine how many naturals needed to add
      var naturals = different_types ? cancel_accList.length : cancel_accList.length - this.accList.length;

      // Return if no naturals needed
      if (naturals < 1) return;

      // Get the line position for each natural
      var cancelled = [];
      for (var i = 0; i < naturals; i++) {
        var index = i;
        if (!different_types) {
          index = cancel_accList.length - naturals + i;
        }

        var acc = cancel_accList[index];
        cancelled.push({ type: 'n', line: acc.line });
      }

      // Combine naturals with main accidental list for the key signature
      this.accList = cancelled.concat(this.accList);
    }

    // Deprecated

  }, {
    key: 'addToStave',
    value: function addToStave(stave) {
      this.paddingForced = true;
      stave.addModifier(this);

      return this;
    }

    // Apply the accidental staff line placement based on the `clef` and
    // the  accidental `type` for the key signature ('# or 'b').

  }, {
    key: 'convertAccLines',
    value: function convertAccLines(clef, type) {
      var offset = 0.0; // if clef === "treble"
      var customLines = void 0; // when clef doesn't follow treble key sig shape

      switch (clef) {
        // Treble & Subbass both have offsets of 0, so are not included.
        case 'soprano':
          if (type === '#') customLines = [2.5, 0.5, 2, 0, 1.5, -0.5, 1];else offset = -1;
          break;
        case 'mezzo-soprano':
          if (type === 'b') customLines = [0, 2, 0.5, 2.5, 1, 3, 1.5];else offset = 1.5;
          break;
        case 'alto':
          offset = 0.5;
          break;
        case 'tenor':
          if (type === '#') customLines = [3, 1, 2.5, 0.5, 2, 0, 1.5];else offset = -0.5;
          break;
        case 'baritone-f':
        case 'baritone-c':
          if (type === 'b') customLines = [0.5, 2.5, 1, 3, 1.5, 3.5, 2];else offset = 2;
          break;
        case 'bass':
        case 'french':
          offset = 1;
          break;
        default:
          break;
      }

      // If there's a special case, assign those lines/spaces:
      var i = void 0;
      if (typeof customLines !== 'undefined') {
        for (i = 0; i < this.accList.length; ++i) {
          this.accList[i].line = customLines[i];
        }
      } else if (offset !== 0) {
        for (i = 0; i < this.accList.length; ++i) {
          this.accList[i].line += offset;
        }
      }
    }
  }, {
    key: 'getPadding',
    value: function getPadding(index) {
      if (!this.formatted) this.format();

      return this.glyphs.length === 0 || !this.paddingForced && index < 2 ? 0 : this.padding;
    }
  }, {
    key: 'getWidth',
    value: function getWidth() {
      if (!this.formatted) this.format();

      return this.width;
    }
  }, {
    key: 'setKeySig',
    value: function setKeySig(keySpec, cancelKeySpec, alterKeySpec) {
      this.formatted = false;
      this.keySpec = keySpec;
      this.cancelKeySpec = cancelKeySpec;
      this.alterKeySpec = alterKeySpec;

      return this;
    }

    // Alter the accidentals of a key spec one by one.
    // Each alteration is a new accidental that replaces the
    // original accidental (or the canceled one).

  }, {
    key: 'alterKey',
    value: function alterKey(alterKeySpec) {
      this.formatted = false;
      this.alterKeySpec = alterKeySpec;

      return this;
    }
  }, {
    key: 'convertToAlterAccList',
    value: function convertToAlterAccList(alterKeySpec) {
      var max = Math.min(alterKeySpec.length, this.accList.length);
      for (var i = 0; i < max; ++i) {
        if (alterKeySpec[i]) {
          this.accList[i].type = alterKeySpec[i];
        }
      }
    }
  }, {
    key: 'format',
    value: function format() {
      if (!this.stave) {
        throw new _vex.Vex.RERR('KeySignatureError', "Can't draw key signature without stave.");
      }

      this.width = 0;
      this.glyphs = [];
      this.xPositions = [0]; // initialize with initial x position
      this.accList = _tables.Flow.keySignature(this.keySpec);
      if (this.cancelKeySpec) {
        this.convertToCancelAccList(this.cancelKeySpec);
      }
      var firstAccidentalType = this.accList.length > 0 ? this.accList[0].type : null;
      if (this.alterKeySpec) {
        this.convertToAlterAccList(this.alterKeySpec);
      }

      if (this.accList.length > 0) {
        this.convertAccLines(this.stave.clef, firstAccidentalType);
        for (var i = 0; i < this.accList.length; ++i) {
          this.convertToGlyph(this.accList[i], this.accList[i + 1]);
        }
      }

      this.formatted = true;
    }
  }, {
    key: 'draw',
    value: function draw() {
      if (!this.x) {
        throw new _vex.Vex.RERR('KeySignatureError', "Can't draw key signature without x.");
      }

      if (!this.stave) {
        throw new _vex.Vex.RERR('KeySignatureError', "Can't draw key signature without stave.");
      }

      if (!this.formatted) this.format();
      this.setRendered();

      for (var i = 0; i < this.glyphs.length; i++) {
        var glyph = this.glyphs[i];
        var x = this.x + this.xPositions[i];
        glyph.setStave(this.stave);
        glyph.setContext(this.stave.context);
        glyph.renderToStave(x);
      }
    }
  }]);

  return KeySignature;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 47 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Volta = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _stavemodifier = __webpack_require__(7);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Larry Kuhns 2011

var Volta = exports.Volta = function (_StaveModifier) {
  _inherits(Volta, _StaveModifier);

  _createClass(Volta, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'voltas';
    }
  }, {
    key: 'type',
    get: function get() {
      return {
        NONE: 1,
        BEGIN: 2,
        MID: 3,
        END: 4,
        BEGIN_END: 5
      };
    }
  }]);

  function Volta(type, number, x, y_shift) {
    _classCallCheck(this, Volta);

    var _this = _possibleConstructorReturn(this, (Volta.__proto__ || Object.getPrototypeOf(Volta)).call(this));

    _this.setAttribute('type', 'Volta');
    _this.volta = type;
    _this.x = x;
    _this.y_shift = y_shift;
    _this.number = number;
    _this.font = {
      family: 'sans-serif',
      size: 9,
      weight: 'bold'
    };
    return _this;
  }

  _createClass(Volta, [{
    key: 'getCategory',
    value: function getCategory() {
      return Volta.CATEGORY;
    }
  }, {
    key: 'setShiftY',
    value: function setShiftY(y) {
      this.y_shift = y;return this;
    }
  }, {
    key: 'draw',
    value: function draw(stave, x) {
      var ctx = stave.checkContext();
      this.setRendered();

      var width = stave.width;
      var top_y = stave.getYForTopText(stave.options.num_lines) + this.y_shift;
      var vert_height = 1.5 * stave.options.spacing_between_lines_px;
      switch (this.volta) {
        case Volta.type.BEGIN:
          ctx.fillRect(this.x + x, top_y, 1, vert_height);
          break;
        case Volta.type.END:
          width -= 5;
          ctx.fillRect(this.x + x + width, top_y, 1, vert_height);
          break;
        case Volta.type.BEGIN_END:
          width -= 3;
          ctx.fillRect(this.x + x, top_y, 1, vert_height);
          ctx.fillRect(this.x + x + width, top_y, 1, vert_height);
          break;
        default:
          break;
      }
      // If the beginning of a volta, draw measure number
      if (this.volta === Volta.type.BEGIN || this.volta === Volta.type.BEGIN_END) {
        ctx.save();
        ctx.setFont(this.font.family, this.font.size, this.font.weight);
        ctx.fillText(this.number, this.x + x + 5, top_y + 15);
        ctx.restore();
      }

      ctx.fillRect(this.x + x, top_y, width, 1);
      return this;
    }
  }]);

  return Volta;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 48 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TabStave = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _stave = __webpack_require__(33);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

var TabStave = exports.TabStave = function (_Stave) {
  _inherits(TabStave, _Stave);

  function TabStave(x, y, width, options) {
    _classCallCheck(this, TabStave);

    var tab_options = {
      spacing_between_lines_px: 13,
      num_lines: 6,
      top_text_position: 1
    };

    _vex.Vex.Merge(tab_options, options);

    var _this = _possibleConstructorReturn(this, (TabStave.__proto__ || Object.getPrototypeOf(TabStave)).call(this, x, y, width, tab_options));

    _this.setAttribute('type', 'TabStave');
    return _this;
  }

  _createClass(TabStave, [{
    key: 'getYForGlyphs',
    value: function getYForGlyphs() {
      return this.getYForLine(2.5);
    }

    // Deprecated

  }, {
    key: 'addTabGlyph',
    value: function addTabGlyph() {
      this.addClef('tab');
      return this;
    }
  }]);

  return TabStave;
}(_stave.Stave);

/***/ }),
/* 49 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.VibratoBracket = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _vibrato = __webpack_require__(32);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Balazs Forian-Szabo
//
// ## Description
//
// This file implements `VibratoBrackets`
// that renders vibrato effect between two notes.

// To enable logging for this class. Set `Vex.Flow.VibratoBracket.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (VibratoBracket.DEBUG) _vex.Vex.L('Vex.Flow.VibratoBracket', args);
}

var VibratoBracket = exports.VibratoBracket = function (_Element) {
  _inherits(VibratoBracket, _Element);

  // bracket_data = {
  //   start: Vex.Flow.Note (optional)
  //   stop: Vex.Flow.Note (optional)
  // };
  // Either the stop or start note must be set, or both of them.
  // A null value for the start or stop note indicates that the vibrato
  // is drawn from the beginning or until the end of the stave accordingly.
  function VibratoBracket(bracket_data) {
    _classCallCheck(this, VibratoBracket);

    var _this = _possibleConstructorReturn(this, (VibratoBracket.__proto__ || Object.getPrototypeOf(VibratoBracket)).call(this));

    _this.setAttribute('type', 'VibratoBracket');

    _this.start = bracket_data.start;
    _this.stop = bracket_data.stop;

    _this.line = 1;

    _this.render_options = {
      harsh: false,
      wave_height: 6,
      wave_width: 4,
      wave_girth: 2
    };
    return _this;
  }

  // Set line position of the vibrato bracket


  _createClass(VibratoBracket, [{
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;return this;
    }
  }, {
    key: 'setHarsh',
    value: function setHarsh(harsh) {
      this.render_options.harsh = harsh;return this;
    }

    // Draw the vibrato bracket on the rendering context

  }, {
    key: 'draw',
    value: function draw() {
      var ctx = this.context;
      this.setRendered();

      var y = this.start ? this.start.getStave().getYForTopText(this.line) : this.stop.getStave().getYForTopText(this.line);

      // If start note is not set then vibrato will be drawn
      // from the beginning of the stave
      var start_x = this.start ? this.start.getAbsoluteX() : this.stop.getStave().getTieStartX();

      // If stop note is not set then vibrato will be drawn
      // until the end of the stave
      var stop_x = this.stop ? this.stop.getAbsoluteX() - this.stop.getWidth() - 5 : this.start.getStave().getTieEndX() - 10;

      this.render_options.vibrato_width = stop_x - start_x;

      L('Rendering VibratoBracket: start_x:', start_x, 'stop_x:', stop_x, 'y:', y);

      _vibrato.Vibrato.renderVibrato(ctx, start_x, y, this.render_options);
    }
  }]);

  return VibratoBracket;
}(_element.Element);

/***/ }),
/* 50 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ClefNote = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _boundingbox = __webpack_require__(10);

var _note = __webpack_require__(6);

var _clef = __webpack_require__(36);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Copyright Mohit Muthanna 2010
//
// Author Taehoon Moon 2014

/** @constructor */
var ClefNote = exports.ClefNote = function (_Note) {
  _inherits(ClefNote, _Note);

  _createClass(ClefNote, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'clefnote';
    }
  }]);

  function ClefNote(type, size, annotation) {
    _classCallCheck(this, ClefNote);

    var _this = _possibleConstructorReturn(this, (ClefNote.__proto__ || Object.getPrototypeOf(ClefNote)).call(this, { duration: 'b' }));

    _this.setAttribute('type', 'ClefNote');

    _this.setType(type, size, annotation);

    // Note properties
    _this.ignore_ticks = true;
    return _this;
  }

  _createClass(ClefNote, [{
    key: 'setType',
    value: function setType(type, size, annotation) {
      this.type = type;
      this.clef_obj = new _clef.Clef(type, size, annotation);
      this.clef = this.clef_obj.clef;
      this.glyph = new _glyph.Glyph(this.clef.code, this.clef.point);
      this.setWidth(this.glyph.getMetrics().width);
      return this;
    }
  }, {
    key: 'getClef',
    value: function getClef() {
      return this.clef;
    }
  }, {
    key: 'setContext',
    value: function setContext(context) {
      this.context = context;
      this.glyph.setContext(this.context);
      return this;
    }
  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      return new _boundingbox.BoundingBox(0, 0, 0, 0);
    }
  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext() {
      /* overridden to ignore */
      return this;
    }
  }, {
    key: 'getCategory',
    value: function getCategory() {
      return ClefNote.CATEGORY;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      this.setPreFormatted(true);
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      if (!this.stave) throw new _vex.Vex.RERR('NoStave', "Can't draw without a stave.");

      if (!this.glyph.getContext()) {
        this.glyph.setContext(this.context);
      }

      this.setRendered();
      var abs_x = this.getAbsoluteX();

      this.glyph.setStave(this.stave);
      this.glyph.setYShift(this.stave.getYForLine(this.clef.line) - this.stave.getYForGlyphs());
      this.glyph.renderToStave(abs_x);

      // If the Vex.Flow.Clef has an annotation, such as 8va, draw it.
      if (this.clef_obj.annotation !== undefined) {
        var attachment = new _glyph.Glyph(this.clef_obj.annotation.code, this.clef_obj.annotation.point);
        if (!attachment.getContext()) {
          attachment.setContext(this.context);
        }
        attachment.setStave(this.stave);
        attachment.setYShift(this.stave.getYForLine(this.clef_obj.annotation.line) - this.stave.getYForGlyphs());
        attachment.setXShift(this.clef_obj.annotation.x_shift);
        attachment.renderToStave(abs_x);
      }
    }
  }]);

  return ClefNote;
}(_note.Note);

/***/ }),
/* 51 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TimeSigNote = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _boundingbox = __webpack_require__(10);

var _note = __webpack_require__(6);

var _timesignature = __webpack_require__(37);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Taehoon Moon 2014

var TimeSigNote = exports.TimeSigNote = function (_Note) {
  _inherits(TimeSigNote, _Note);

  function TimeSigNote(timeSpec, customPadding) {
    _classCallCheck(this, TimeSigNote);

    var _this = _possibleConstructorReturn(this, (TimeSigNote.__proto__ || Object.getPrototypeOf(TimeSigNote)).call(this, { duration: 'b' }));

    _this.setAttribute('type', 'TimeSigNote');

    var timeSignature = new _timesignature.TimeSignature(timeSpec, customPadding);
    _this.timeSig = timeSignature.getTimeSig();
    _this.setWidth(_this.timeSig.glyph.getMetrics().width);

    // Note properties
    _this.ignore_ticks = true;
    return _this;
  }

  _createClass(TimeSigNote, [{
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      return new _boundingbox.BoundingBox(0, 0, 0, 0);
    }
  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext() {
      /* overridden to ignore */
      return this;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      this.setPreFormatted(true);
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.stave.checkContext();
      this.setRendered();

      if (!this.timeSig.glyph.getContext()) {
        this.timeSig.glyph.setContext(this.context);
      }

      this.timeSig.glyph.setStave(this.stave);
      this.timeSig.glyph.setYShift(this.stave.getYForLine(this.timeSig.line) - this.stave.getYForGlyphs());
      this.timeSig.glyph.renderToStave(this.getAbsoluteX());
    }
  }]);

  return TimeSigNote;
}(_note.Note);

/***/ }),
/* 52 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.GraceNote = undefined;

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _stavenote = __webpack_require__(5);

var _tables = __webpack_require__(1);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

var GraceNote = exports.GraceNote = function (_StaveNote) {
  _inherits(GraceNote, _StaveNote);

  _createClass(GraceNote, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'gracenotes';
    }
  }, {
    key: 'LEDGER_LINE_OFFSET',
    get: function get() {
      return 2;
    }
  }, {
    key: 'SCALE',
    get: function get() {
      return 0.66;
    }
  }]);

  function GraceNote(note_struct) {
    _classCallCheck(this, GraceNote);

    var _this = _possibleConstructorReturn(this, (GraceNote.__proto__ || Object.getPrototypeOf(GraceNote)).call(this, _extends(note_struct, {
      glyph_font_scale: _tables.Flow.DEFAULT_NOTATION_FONT_SCALE * GraceNote.SCALE,
      stroke_px: GraceNote.LEDGER_LINE_OFFSET
    })));

    _this.setAttribute('type', 'GraceNote');

    _this.slash = note_struct.slash;
    _this.slur = true;

    _this.buildNoteHeads();

    _this.width = 3;
    return _this;
  }

  _createClass(GraceNote, [{
    key: 'getStemExtension',
    value: function getStemExtension() {
      var glyph = this.getGlyph();

      if (this.stem_extension_override != null) {
        return this.stem_extension_override;
      }

      if (glyph) {
        return this.getStemDirection() === 1 ? glyph.gracenote_stem_up_extension : glyph.gracenote_stem_down_extension;
      }

      return 0;
    }
  }, {
    key: 'getCategory',
    value: function getCategory() {
      return GraceNote.CATEGORY;
    }
  }, {
    key: 'draw',
    value: function draw() {
      _get(GraceNote.prototype.__proto__ || Object.getPrototypeOf(GraceNote.prototype), 'draw', this).call(this);
      this.setRendered();
      var ctx = this.context;
      var stem_direction = this.getStemDirection();

      if (this.slash) {
        ctx.beginPath();

        var x = this.getAbsoluteX();
        var y = this.getYs()[0] - this.stem.getHeight() / 2.8;
        if (stem_direction === 1) {
          x += 1;
          ctx.moveTo(x, y);
          ctx.lineTo(x + 13, y - 9);
        } else if (stem_direction === -1) {
          x -= 4;
          y += 1;
          ctx.moveTo(x, y);
          ctx.lineTo(x + 13, y + 9);
        }

        ctx.closePath();
        ctx.stroke();
      }
    }
  }]);

  return GraceNote;
}(_stavenote.StaveNote);

/***/ }),
/* 53 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Curve = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // VexFlow - Music Engraving for HTML5
// Copyright Mohit Muthanna 2010
//
// This class implements curves (for slurs)

var Curve = exports.Curve = function (_Element) {
  _inherits(Curve, _Element);

  _createClass(Curve, null, [{
    key: 'Position',
    get: function get() {
      return {
        NEAR_HEAD: 1,
        NEAR_TOP: 2
      };
    }
  }, {
    key: 'PositionString',
    get: function get() {
      return {
        nearHead: Curve.Position.NEAR_HEAD,
        nearTop: Curve.Position.NEAR_TOP
      };
    }

    // from: Start note
    // to: End note
    // options:
    //    cps: List of control points
    //    x_shift: pixels to shift
    //    y_shift: pixels to shift

  }]);

  function Curve(from, to, options) {
    _classCallCheck(this, Curve);

    var _this = _possibleConstructorReturn(this, (Curve.__proto__ || Object.getPrototypeOf(Curve)).call(this));

    _this.setAttribute('type', 'Curve');

    _this.render_options = {
      spacing: 2,
      thickness: 2,
      x_shift: 0,
      y_shift: 10,
      position: Curve.Position.NEAR_HEAD,
      position_end: Curve.Position.NEAR_HEAD,
      invert: false,
      cps: [{ x: 0, y: 10 }, { x: 0, y: 10 }]
    };

    _vex.Vex.Merge(_this.render_options, options);
    _this.setNotes(from, to);
    return _this;
  }

  _createClass(Curve, [{
    key: 'setNotes',
    value: function setNotes(from, to) {
      if (!from && !to) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'Curve needs to have either first_note or last_note set.');
      }

      this.from = from;
      this.to = to;
      return this;
    }

    /**
     * @return {boolean} Returns true if this is a partial bar.
     */

  }, {
    key: 'isPartial',
    value: function isPartial() {
      return !this.from || !this.to;
    }
  }, {
    key: 'renderCurve',
    value: function renderCurve(params) {
      var ctx = this.context;
      var cps = this.render_options.cps;

      var x_shift = this.render_options.x_shift;
      var y_shift = this.render_options.y_shift * params.direction;

      var first_x = params.first_x + x_shift;
      var first_y = params.first_y + y_shift;
      var last_x = params.last_x - x_shift;
      var last_y = params.last_y + y_shift;
      var thickness = this.render_options.thickness;

      var cp_spacing = (last_x - first_x) / (cps.length + 2);

      ctx.beginPath();
      ctx.moveTo(first_x, first_y);
      ctx.bezierCurveTo(first_x + cp_spacing + cps[0].x, first_y + cps[0].y * params.direction, last_x - cp_spacing + cps[1].x, last_y + cps[1].y * params.direction, last_x, last_y);
      ctx.bezierCurveTo(last_x - cp_spacing + cps[1].x, last_y + (cps[1].y + thickness) * params.direction, first_x + cp_spacing + cps[0].x, first_y + (cps[0].y + thickness) * params.direction, first_x, first_y);
      ctx.stroke();
      ctx.closePath();
      ctx.fill();
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      var first_note = this.from;
      var last_note = this.to;
      var first_x = void 0;
      var last_x = void 0;
      var first_y = void 0;
      var last_y = void 0;
      var stem_direction = void 0;

      var metric = 'baseY';
      var end_metric = 'baseY';

      function getPosition(position) {
        return typeof position === 'string' ? Curve.PositionString[position] : position;
      }
      var position = getPosition(this.render_options.position);
      var position_end = getPosition(this.render_options.position_end);

      if (position === Curve.Position.NEAR_TOP) {
        metric = 'topY';
        end_metric = 'topY';
      }

      if (position_end === Curve.Position.NEAR_HEAD) {
        end_metric = 'baseY';
      } else if (position_end === Curve.Position.NEAR_TOP) {
        end_metric = 'topY';
      }

      if (first_note) {
        first_x = first_note.getTieRightX();
        stem_direction = first_note.getStemDirection();
        first_y = first_note.getStemExtents()[metric];
      } else {
        first_x = last_note.getStave().getTieStartX();
        first_y = last_note.getStemExtents()[metric];
      }

      if (last_note) {
        last_x = last_note.getTieLeftX();
        stem_direction = last_note.getStemDirection();
        last_y = last_note.getStemExtents()[end_metric];
      } else {
        last_x = first_note.getStave().getTieEndX();
        last_y = first_note.getStemExtents()[end_metric];
      }

      this.renderCurve({
        first_x: first_x,
        last_x: last_x,
        first_y: first_y,
        last_y: last_y,
        direction: stem_direction * (this.render_options.invert === true ? -1 : 1)
      });
      return true;
    }
  }]);

  return Curve;
}(_element.Element);

/***/ }),
/* 54 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TextDynamics = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _note = __webpack_require__(6);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This file implements the `TextDynamics` which renders traditional
// text dynamics markings, **ie: p, f, sfz, rfz, ppp**
//
// You can render any dynamics string that contains a combination of
// the following letters:  P, M, F, Z, R, S

// To enable logging for this class. Set `Vex.Flow.TextDynamics.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (TextDynamics.DEBUG) _vex.Vex.L('Vex.Flow.TextDynamics', args);
}

var TextDynamics = exports.TextDynamics = function (_Note) {
  _inherits(TextDynamics, _Note);

  _createClass(TextDynamics, null, [{
    key: 'GLYPHS',

    // The glyph data for each dynamics letter
    get: function get() {
      return {
        'f': {
          code: 'vba',
          width: 12
        },
        'p': {
          code: 'vbf',
          width: 14
        },
        'm': {
          code: 'v62',
          width: 17
        },
        's': {
          code: 'v4a',
          width: 10
        },
        'z': {
          code: 'v80',
          width: 12
        },
        'r': {
          code: 'vb1',
          width: 12
        }
      };
    }

    // A `TextDynamics` object inherits from `Note` so that it can be formatted
    // within a `Voice`.
    // Create the dynamics marking. `text_struct` is an object
    // that contains a `duration` property and a `sequence` of
    // letters that represents the letters to render

  }]);

  function TextDynamics(text_struct) {
    _classCallCheck(this, TextDynamics);

    var _this = _possibleConstructorReturn(this, (TextDynamics.__proto__ || Object.getPrototypeOf(TextDynamics)).call(this, text_struct));

    _this.setAttribute('type', 'TextDynamics');

    _this.sequence = text_struct.text.toLowerCase();
    _this.line = text_struct.line || 0;
    _this.glyphs = [];

    _vex.Vex.Merge(_this.render_options, {
      glyph_font_size: 40
    });

    L('New Dynamics Text: ', _this.sequence);
    return _this;
  }

  // Set the Stave line on which the note should be placed


  _createClass(TextDynamics, [{
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;
      return this;
    }

    // Preformat the dynamics text

  }, {
    key: 'preFormat',
    value: function preFormat() {
      var _this2 = this;

      var total_width = 0;
      // Iterate through each letter
      this.sequence.split('').forEach(function (letter) {
        // Get the glyph data for the letter
        var glyph_data = TextDynamics.GLYPHS[letter];
        if (!glyph_data) throw new _vex.Vex.RERR('Invalid dynamics character: ' + letter);

        var size = _this2.render_options.glyph_font_size;
        var glyph = new _glyph.Glyph(glyph_data.code, size);

        // Add the glyph
        _this2.glyphs.push(glyph);

        total_width += glyph_data.width;
      });

      // Store the width of the text
      this.setWidth(total_width);
      this.preFormatted = true;
      return this;
    }

    // Draw the dynamics text on the rendering context

  }, {
    key: 'draw',
    value: function draw() {
      var _this3 = this;

      this.setRendered();
      var x = this.getAbsoluteX();
      var y = this.stave.getYForLine(this.line + -3);

      L('Rendering Dynamics: ', this.sequence);

      var letter_x = x;
      this.glyphs.forEach(function (glyph, index) {
        var current_letter = _this3.sequence[index];
        glyph.render(_this3.context, letter_x, y);
        letter_x += TextDynamics.GLYPHS[current_letter].width;
      });
    }
  }]);

  return TextDynamics;
}(_note.Note);

/***/ }),
/* 55 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveLine = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _tables = __webpack_require__(1);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements `StaveLine` which are simply lines that connect
// two notes. This object is highly configurable, see the `render_options`.
// A simple line is often used for notating glissando articulations, but you
// can format a `StaveLine` with arrows or colors for more pedagogical
// purposes, such as diagrams.


// Attribution: Arrow rendering implementations based off of
// Patrick Horgan's article, "Drawing lines and arcs with
// arrow heads on  HTML5 Canvas"
//
// Draw an arrow head that connects between 3 coordinates
function drawArrowHead(ctx, x0, y0, x1, y1, x2, y2) {
  // all cases do this.
  ctx.beginPath();
  ctx.moveTo(x0, y0);
  ctx.lineTo(x1, y1);
  ctx.lineTo(x2, y2);
  ctx.lineTo(x0, y0);
  ctx.closePath();

  ctx.fill();
}

// Helper function to draw a line with arrow heads
function drawArrowLine(ctx, point1, point2, config) {
  var both_arrows = config.draw_start_arrow && config.draw_end_arrow;

  var x1 = point1.x;
  var y1 = point1.y;
  var x2 = point2.x;
  var y2 = point2.y;

  // For ends with arrow we actually want to stop before we get to the arrow
  // so that wide lines won't put a flat end on the arrow.
  var distance = Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
  var ratio = (distance - config.arrowhead_length / 3) / distance;
  var end_x = void 0;
  var end_y = void 0;
  var start_x = void 0;
  var start_y = void 0;
  if (config.draw_end_arrow || both_arrows) {
    end_x = Math.round(x1 + (x2 - x1) * ratio);
    end_y = Math.round(y1 + (y2 - y1) * ratio);
  } else {
    end_x = x2;
    end_y = y2;
  }

  if (config.draw_start_arrow || both_arrows) {
    start_x = x1 + (x2 - x1) * (1 - ratio);
    start_y = y1 + (y2 - y1) * (1 - ratio);
  } else {
    start_x = x1;
    start_y = y1;
  }

  if (config.color) {
    ctx.setStrokeStyle(config.color);
    ctx.setFillStyle(config.color);
  }

  // Draw the shaft of the arrow
  ctx.beginPath();
  ctx.moveTo(start_x, start_y);
  ctx.lineTo(end_x, end_y);
  ctx.stroke();
  ctx.closePath();

  // calculate the angle of the line
  var line_angle = Math.atan2(y2 - y1, x2 - x1);
  // h is the line length of a side of the arrow head
  var h = Math.abs(config.arrowhead_length / Math.cos(config.arrowhead_angle));

  var angle1 = void 0;
  var angle2 = void 0;
  var top_x = void 0;
  var top_y = void 0;
  var bottom_x = void 0;
  var bottom_y = void 0;

  if (config.draw_end_arrow || both_arrows) {
    angle1 = line_angle + Math.PI + config.arrowhead_angle;
    top_x = x2 + Math.cos(angle1) * h;
    top_y = y2 + Math.sin(angle1) * h;

    angle2 = line_angle + Math.PI - config.arrowhead_angle;
    bottom_x = x2 + Math.cos(angle2) * h;
    bottom_y = y2 + Math.sin(angle2) * h;

    drawArrowHead(ctx, top_x, top_y, x2, y2, bottom_x, bottom_y);
  }

  if (config.draw_start_arrow || both_arrows) {
    angle1 = line_angle + config.arrowhead_angle;
    top_x = x1 + Math.cos(angle1) * h;
    top_y = y1 + Math.sin(angle1) * h;

    angle2 = line_angle - config.arrowhead_angle;
    bottom_x = x1 + Math.cos(angle2) * h;
    bottom_y = y1 + Math.sin(angle2) * h;

    drawArrowHead(ctx, top_x, top_y, x1, y1, bottom_x, bottom_y);
  }
}

var StaveLine = exports.StaveLine = function (_Element) {
  _inherits(StaveLine, _Element);

  _createClass(StaveLine, null, [{
    key: 'TextVerticalPosition',

    // Text Positioning
    get: function get() {
      return {
        TOP: 1,
        BOTTOM: 2
      };
    }
  }, {
    key: 'TextJustification',
    get: function get() {
      return {
        LEFT: 1,
        CENTER: 2,
        RIGHT: 3
      };
    }

    // Initialize the StaveLine with the given `notes`.
    //
    // `notes` is a struct that has:
    //
    //  ```
    //  {
    //    first_note: Note,
    //    last_note: Note,
    //    first_indices: [n1, n2, n3],
    //    last_indices: [n1, n2, n3]
    //  }
    //  ```

  }]);

  function StaveLine(notes) {
    _classCallCheck(this, StaveLine);

    var _this = _possibleConstructorReturn(this, (StaveLine.__proto__ || Object.getPrototypeOf(StaveLine)).call(this));

    _this.setAttribute('type', 'StaveLine');

    _this.notes = notes;

    _this.text = '';

    _this.font = {
      family: 'Arial',
      size: 10,
      weight: ''
    };

    _this.render_options = {
      // Space to add to the left or the right
      padding_left: 4,
      padding_right: 3,

      // The width of the line in pixels
      line_width: 1,
      // An array of line/space lengths. Unsupported with Raphael (SVG)
      line_dash: null,
      // Can draw rounded line end, instead of a square. Unsupported with Raphael (SVG)
      rounded_end: true,
      // The color of the line and arrowheads
      color: null,

      // Flags to draw arrows on each end of the line
      draw_start_arrow: false,
      draw_end_arrow: false,

      // The length of the arrowhead sides
      arrowhead_length: 10,
      // The angle of the arrowhead
      arrowhead_angle: Math.PI / 8,

      // The position of the text
      text_position_vertical: StaveLine.TextVerticalPosition.TOP,
      text_justification: StaveLine.TextJustification.CENTER
    };

    _this.setNotes(notes);
    return _this;
  }

  // Set the font for the `StaveLine` text


  _createClass(StaveLine, [{
    key: 'setFont',
    value: function setFont(font) {
      this.font = font;return this;
    }
    // The the annotation for the `StaveLine`

  }, {
    key: 'setText',
    value: function setText(text) {
      this.text = text;return this;
    }

    // Set the notes for the `StaveLine`

  }, {
    key: 'setNotes',
    value: function setNotes(notes) {
      if (!notes.first_note && !notes.last_note) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'Notes needs to have either first_note or last_note set.');
      }

      if (!notes.first_indices) notes.first_indices = [0];
      if (!notes.last_indices) notes.last_indices = [0];

      if (notes.first_indices.length !== notes.last_indices.length) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'Connected notes must have similar index sizes');
      }

      // Success. Lets grab 'em notes.
      this.first_note = notes.first_note;
      this.first_indices = notes.first_indices;
      this.last_note = notes.last_note;
      this.last_indices = notes.last_indices;
      return this;
    }

    // Apply the style of the `StaveLine` to the context

  }, {
    key: 'applyLineStyle',
    value: function applyLineStyle() {
      var ctx = this.checkContext();
      var render_options = this.render_options;

      if (render_options.line_dash) {
        ctx.setLineDash(render_options.line_dash);
      }

      if (render_options.line_width) {
        ctx.setLineWidth(render_options.line_width);
      }

      if (render_options.rounded_end) {
        ctx.setLineCap('round');
      } else {
        ctx.setLineCap('square');
      }
    }

    // Apply the text styling to the context

  }, {
    key: 'applyFontStyle',
    value: function applyFontStyle() {
      var ctx = this.checkContext();

      if (this.font) {
        ctx.setFont(this.font.family, this.font.size, this.font.weight);
      }

      if (this.render_options.color) {
        ctx.setStrokeStyle(this.render_options.color);
        ctx.setFillStyle(this.render_options.color);
      }
    }

    // Renders the `StaveLine` on the context

  }, {
    key: 'draw',
    value: function draw() {
      var _this2 = this;

      var ctx = this.checkContext();
      this.setRendered();

      var first_note = this.first_note;
      var last_note = this.last_note;
      var render_options = this.render_options;

      ctx.save();
      this.applyLineStyle();

      // Cycle through each set of indices and draw lines
      var start_position = void 0;
      var end_position = void 0;
      this.first_indices.forEach(function (first_index, i) {
        var last_index = _this2.last_indices[i];

        // Get initial coordinates for the start/end of the line
        start_position = first_note.getModifierStartXY(2, first_index);
        end_position = last_note.getModifierStartXY(1, last_index);
        var upwards_slope = start_position.y > end_position.y;

        // Adjust `x` coordinates for modifiers
        start_position.x += first_note.getMetrics().modRightPx + render_options.padding_left;
        end_position.x -= last_note.getMetrics().modLeftPx + render_options.padding_right;

        // Adjust first `x` coordinates for displacements
        var notehead_width = first_note.getGlyph().getWidth();
        var first_displaced = first_note.getKeyProps()[first_index].displaced;
        if (first_displaced && first_note.getStemDirection() === 1) {
          start_position.x += notehead_width + render_options.padding_left;
        }

        // Adjust last `x` coordinates for displacements
        var last_displaced = last_note.getKeyProps()[last_index].displaced;
        if (last_displaced && last_note.getStemDirection() === -1) {
          end_position.x -= notehead_width + render_options.padding_right;
        }

        // Adjust y position better if it's not coming from the center of the note
        start_position.y += upwards_slope ? -3 : 1;
        end_position.y += upwards_slope ? 2 : 0;

        drawArrowLine(ctx, start_position, end_position, _this2.render_options);
      });

      ctx.restore();

      // Determine the x coordinate where to start the text
      var text_width = ctx.measureText(this.text).width;
      var justification = render_options.text_justification;
      var x = 0;
      if (justification === StaveLine.TextJustification.LEFT) {
        x = start_position.x;
      } else if (justification === StaveLine.TextJustification.CENTER) {
        var delta_x = end_position.x - start_position.x;
        var center_x = delta_x / 2 + start_position.x;
        x = center_x - text_width / 2;
      } else if (justification === StaveLine.TextJustification.RIGHT) {
        x = end_position.x - text_width;
      }

      // Determine the y value to start the text
      var y = void 0;
      var vertical_position = render_options.text_position_vertical;
      if (vertical_position === StaveLine.TextVerticalPosition.TOP) {
        y = first_note.getStave().getYForTopText();
      } else if (vertical_position === StaveLine.TextVerticalPosition.BOTTOM) {
        y = first_note.getStave().getYForBottomText(_tables.Flow.TEXT_HEIGHT_OFFSET_HACK);
      }

      // Draw the text
      ctx.save();
      this.applyFontStyle();
      ctx.fillText(this.text, x, y);
      ctx.restore();

      return this;
    }
  }]);

  return StaveLine;
}(_element.Element);

/***/ }),
/* 56 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.PedalMarking = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements different types of pedal markings. These notation
// elements indicate to the performer when to depress and release the a pedal.
//
// In order to create "Sostenuto", and "una corda" markings, you must set
// custom text for the release/depress pedal markings.

// To enable logging for this class. Set `Vex.Flow.PedalMarking.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (PedalMarking.DEBUG) _vex.Vex.L('Vex.Flow.PedalMarking', args);
}

// Draws a pedal glyph with the provided `name` on a rendering `context`
// at the coordinates `x` and `y. Takes into account the glyph data
// coordinate shifts.
function drawPedalGlyph(name, context, x, y, point) {
  var glyph_data = PedalMarking.GLYPHS[name];
  var glyph = new _glyph.Glyph(glyph_data.code, point);
  glyph.render(context, x + glyph_data.x_shift, y + glyph_data.y_shift);
}

var PedalMarking = exports.PedalMarking = function (_Element) {
  _inherits(PedalMarking, _Element);

  _createClass(PedalMarking, null, [{
    key: 'createSustain',


    // Create a sustain pedal marking. Returns the defaults PedalMarking.
    // Which uses the traditional "Ped" and "*"" markings.
    value: function createSustain(notes) {
      var pedal = new PedalMarking(notes);
      return pedal;
    }

    // Create a sostenuto pedal marking

  }, {
    key: 'createSostenuto',
    value: function createSostenuto(notes) {
      var pedal = new PedalMarking(notes);
      pedal.setStyle(PedalMarking.Styles.MIXED);
      pedal.setCustomText('Sost. Ped.');
      return pedal;
    }

    // Create an una corda pedal marking

  }, {
    key: 'createUnaCorda',
    value: function createUnaCorda(notes) {
      var pedal = new PedalMarking(notes);
      pedal.setStyle(PedalMarking.Styles.TEXT);
      pedal.setCustomText('una corda', 'tre corda');
      return pedal;
    }

    // ## Prototype Methods

  }, {
    key: 'GLYPHS',

    // Glyph data
    get: function get() {
      return {
        'pedal_depress': {
          code: 'v36',
          x_shift: -10,
          y_shift: 0
        },
        'pedal_release': {
          code: 'v5d',
          x_shift: -2,
          y_shift: 3
        }
      };
    }
  }, {
    key: 'Styles',
    get: function get() {
      return {
        TEXT: 1,
        BRACKET: 2,
        MIXED: 3
      };
    }
  }, {
    key: 'StylesString',
    get: function get() {
      return {
        text: PedalMarking.Styles.TEXT,
        bracket: PedalMarking.Styles.BRACKET,
        mixed: PedalMarking.Styles.MIXED
      };
    }
  }]);

  function PedalMarking(notes) {
    _classCallCheck(this, PedalMarking);

    var _this = _possibleConstructorReturn(this, (PedalMarking.__proto__ || Object.getPrototypeOf(PedalMarking)).call(this));

    _this.setAttribute('type', 'PedalMarking');

    _this.notes = notes;
    _this.style = PedalMarking.TEXT;
    _this.line = 0;

    // Custom text for the release/depress markings
    _this.custom_depress_text = '';
    _this.custom_release_text = '';

    _this.font = {
      family: 'Times New Roman',
      size: 12,
      weight: 'italic bold'
    };

    _this.render_options = {
      bracket_height: 10,
      text_margin_right: 6,
      bracket_line_width: 1,
      glyph_point_size: 40,
      color: 'black'
    };
    return _this;
  }

  // Set custom text for the `depress`/`release` pedal markings. No text is
  // set if the parameter is falsy.


  _createClass(PedalMarking, [{
    key: 'setCustomText',
    value: function setCustomText(depress, release) {
      this.custom_depress_text = depress || '';
      this.custom_release_text = release || '';
      return this;
    }

    // Set the pedal marking style

  }, {
    key: 'setStyle',
    value: function setStyle(style) {
      if (style < 1 && style > 3) {
        throw new _vex.Vex.RERR('InvalidParameter', 'The style must be one found in PedalMarking.Styles');
      }

      this.style = style;
      return this;
    }

    // Set the staff line to render the markings on

  }, {
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;return this;
    }

    // Draw the bracket based pedal markings

  }, {
    key: 'drawBracketed',
    value: function drawBracketed() {
      var ctx = this.context;
      var is_pedal_depressed = false;
      var prev_x = void 0;
      var prev_y = void 0;
      var pedal = this;

      // Iterate through each note
      this.notes.forEach(function (note, index, notes) {
        // Each note triggers the opposite pedal action
        is_pedal_depressed = !is_pedal_depressed;

        // Get the initial coordinates for the note
        var x = note.getAbsoluteX();
        var y = note.getStave().getYForBottomText(pedal.line + 3);

        // Throw if current note is positioned before the previous note
        if (x < prev_x) {
          throw new _vex.Vex.RERR('InvalidConfiguration', 'The notes provided must be in order of ascending x positions');
        }

        // Determine if the previous or next note are the same
        // as the current note. We need to keep track of this for
        // when adjustments are made for the release+depress action
        var next_is_same = notes[index + 1] === note;
        var prev_is_same = notes[index - 1] === note;

        var x_shift = 0;
        if (is_pedal_depressed) {
          // Adjustment for release+depress
          x_shift = prev_is_same ? 5 : 0;

          if (pedal.style === PedalMarking.Styles.MIXED && !prev_is_same) {
            // For MIXED style, start with text instead of bracket
            if (pedal.custom_depress_text) {
              // If we have custom text, use instead of the default "Ped" glyph
              var text_width = ctx.measureText(pedal.custom_depress_text).width;
              ctx.fillText(pedal.custom_depress_text, x - text_width / 2, y);
              x_shift = text_width / 2 + pedal.render_options.text_margin_right;
            } else {
              // Render the Ped glyph in position
              drawPedalGlyph('pedal_depress', ctx, x, y, pedal.render_options.glyph_point_size);
              x_shift = 20 + pedal.render_options.text_margin_right;
            }
          } else {
            // Draw start bracket
            ctx.beginPath();
            ctx.moveTo(x, y - pedal.render_options.bracket_height);
            ctx.lineTo(x + x_shift, y);
            ctx.stroke();
            ctx.closePath();
          }
        } else {
          // Adjustment for release+depress
          x_shift = next_is_same ? -5 : 0;

          // Draw end bracket
          ctx.beginPath();
          ctx.moveTo(prev_x, prev_y);
          ctx.lineTo(x + x_shift, y);
          ctx.lineTo(x, y - pedal.render_options.bracket_height);
          ctx.stroke();
          ctx.closePath();
        }

        // Store previous coordinates
        prev_x = x + x_shift;
        prev_y = y;
      });
    }

    // Draw the text based pedal markings. This defaults to the traditional
    // "Ped" and "*"" symbols if no custom text has been provided.

  }, {
    key: 'drawText',
    value: function drawText() {
      var ctx = this.context;
      var is_pedal_depressed = false;
      var pedal = this;

      // The glyph point size
      var point = pedal.render_options.glyph_point_size;

      // Iterate through each note, placing glyphs or custom text accordingly
      this.notes.forEach(function (note) {
        is_pedal_depressed = !is_pedal_depressed;
        var stave = note.getStave();
        var x = note.getAbsoluteX();
        var y = stave.getYForBottomText(pedal.line + 3);

        var text_width = 0;
        if (is_pedal_depressed) {
          if (pedal.custom_depress_text) {
            text_width = ctx.measureText(pedal.custom_depress_text).width;
            ctx.fillText(pedal.custom_depress_text, x - text_width / 2, y);
          } else {
            drawPedalGlyph('pedal_depress', ctx, x, y, point);
          }
        } else {
          if (pedal.custom_release_text) {
            text_width = ctx.measureText(pedal.custom_release_text).width;
            ctx.fillText(pedal.custom_release_text, x - text_width / 2, y);
          } else {
            drawPedalGlyph('pedal_release', ctx, x, y, point);
          }
        }
      });
    }

    // Render the pedal marking in position on the rendering context

  }, {
    key: 'draw',
    value: function draw() {
      var ctx = this.checkContext();
      this.setRendered();

      ctx.save();
      ctx.setStrokeStyle(this.render_options.color);
      ctx.setFillStyle(this.render_options.color);
      ctx.setFont(this.font.family, this.font.size, this.font.weight);

      L('Rendering Pedal Marking');

      if (this.style === PedalMarking.Styles.BRACKET || this.style === PedalMarking.Styles.MIXED) {
        ctx.setLineWidth(this.render_options.bracket_line_width);
        this.drawBracketed();
      } else if (this.style === PedalMarking.Styles.TEXT) {
        this.drawText();
      }

      ctx.restore();
    }
  }]);

  return PedalMarking;
}(_element.Element);

/***/ }),
/* 57 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TextBracket = undefined;

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _element = __webpack_require__(3);

var _renderer = __webpack_require__(14);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Cyril Silverman
//
// ## Description
//
// This file implement `TextBrackets` which extend between two notes.
// The octave transposition markings (8va, 8vb, 15va, 15vb) can be created
// using this class.

// To enable logging for this class. Set `Vex.Flow.TextBracket.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (TextBracket.DEBUG) _vex.Vex.L('Vex.Flow.TextBracket', args);
}

var TextBracket = exports.TextBracket = function (_Element) {
  _inherits(TextBracket, _Element);

  _createClass(TextBracket, null, [{
    key: 'Positions',

    // FIXME: Modifier.Position is singular while this is plural, make consistent
    get: function get() {
      return {
        TOP: 1,
        BOTTOM: -1
      };
    }
  }, {
    key: 'PositionString',
    get: function get() {
      return {
        top: TextBracket.Positions.TOP,
        bottom: TextBracket.Positions.BOTTOM
      };
    }
  }]);

  function TextBracket(_ref) {
    var start = _ref.start,
        stop = _ref.stop,
        _ref$text = _ref.text,
        text = _ref$text === undefined ? '' : _ref$text,
        _ref$superscript = _ref.superscript,
        superscript = _ref$superscript === undefined ? '' : _ref$superscript,
        _ref$position = _ref.position,
        position = _ref$position === undefined ? TextBracket.Positions.TOP : _ref$position;

    _classCallCheck(this, TextBracket);

    var _this = _possibleConstructorReturn(this, (TextBracket.__proto__ || Object.getPrototypeOf(TextBracket)).call(this));

    _this.setAttribute('type', 'TextBracket');

    _this.start = start;
    _this.stop = stop;

    _this.text = text;
    _this.superscript = superscript;

    _this.position = typeof position === 'string' ? TextBracket.PositionString[position] : position;

    _this.line = 1;

    _this.font = {
      family: 'Serif',
      size: 15,
      weight: 'italic'
    };

    _this.render_options = {
      dashed: true,
      dash: [5],
      color: 'black',
      line_width: 1,
      show_bracket: true,
      bracket_height: 8,

      // In the BOTTOM position, the bracket line can extend
      // under the superscript.
      underline_superscript: true
    };
    return _this;
  }

  // Apply the text backet styling to the provided `context`


  _createClass(TextBracket, [{
    key: 'applyStyle',
    value: function applyStyle(context) {
      // Apply style for the octave bracket
      context.setFont(this.font.family, this.font.size, this.font.weight);
      context.setStrokeStyle(this.render_options.color);
      context.setFillStyle(this.render_options.color);
      context.setLineWidth(this.render_options.line_width);

      return this;
    }

    // Set whether the bracket line should be `dashed`. You can also
    // optionally set the `dash` pattern by passing in an array of numbers

  }, {
    key: 'setDashed',
    value: function setDashed(dashed, dash) {
      this.render_options.dashed = dashed;
      if (dash) this.render_options.dash = dash;
      return this;
    }

    // Set the font for the text

  }, {
    key: 'setFont',
    value: function setFont(font) {
      // We use Object.assign to support partial updates to the font object
      this.font = _extends({}, this.font, font);
      return this;
    }
    // Set the rendering `context` for the octave bracket

  }, {
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;return this;
    }

    // Draw the octave bracket on the rendering context

  }, {
    key: 'draw',
    value: function draw() {
      var ctx = this.context;
      this.setRendered();

      var y = 0;
      switch (this.position) {
        case TextBracket.Positions.TOP:
          y = this.start.getStave().getYForTopText(this.line);
          break;
        case TextBracket.Positions.BOTTOM:
          y = this.start.getStave().getYForBottomText(this.line + _tables.Flow.TEXT_HEIGHT_OFFSET_HACK);
          break;
        default:
          throw new _vex.Vex.RERR('InvalidPosition', 'The position ' + this.position + ' is invalid');
      }

      // Get the preliminary start and stop coordintates for the bracket
      var start = { x: this.start.getAbsoluteX(), y: y };
      var stop = { x: this.stop.getAbsoluteX(), y: y };

      L('Rendering TextBracket: start:', start, 'stop:', stop, 'y:', y);

      var bracket_height = this.render_options.bracket_height * this.position;

      ctx.save();
      this.applyStyle(ctx);

      // Draw text
      ctx.fillText(this.text, start.x, start.y);

      // Get the width and height for the octave number
      var main_width = ctx.measureText(this.text).width;
      var main_height = ctx.measureText('M').width;

      // Calculate the y position for the super script
      var super_y = start.y - main_height / 2.5;

      // Draw the superscript
      ctx.setFont(this.font.family, this.font.size / 1.4, this.font.weight);
      ctx.fillText(this.superscript, start.x + main_width + 1, super_y);

      // Determine width and height of the superscript
      var superscript_width = ctx.measureText(this.superscript).width;
      var super_height = ctx.measureText('M').width;

      // Setup initial coordinates for the bracket line
      var start_x = start.x;
      var line_y = super_y;
      var end_x = stop.x + this.stop.getGlyph().getWidth();

      // Adjust x and y coordinates based on position
      if (this.position === TextBracket.Positions.TOP) {
        start_x += main_width + superscript_width + 5;
        line_y -= super_height / 2.7;
      } else if (this.position === TextBracket.Positions.BOTTOM) {
        line_y += super_height / 2.7;
        start_x += main_width + 2;

        if (!this.render_options.underline_superscript) {
          start_x += superscript_width;
        }
      }

      if (this.render_options.dashed) {
        // Main line
        _renderer.Renderer.drawDashedLine(ctx, start_x, line_y, end_x, line_y, this.render_options.dash);
        // Ending Bracket
        if (this.render_options.show_bracket) {
          _renderer.Renderer.drawDashedLine(ctx, end_x, line_y + 1 * this.position, end_x, line_y + bracket_height, this.render_options.dash);
        }
      } else {
        ctx.beginPath();
        ctx.moveTo(start_x, line_y);
        // Main line
        ctx.lineTo(end_x, line_y);
        if (this.render_options.show_bracket) {
          // Ending bracket
          ctx.lineTo(end_x, line_y + bracket_height);
        }
        ctx.stroke();
        ctx.closePath();
      }

      ctx.restore();
    }
  }]);

  return TextBracket;
}(_element.Element);

/***/ }),
/* 58 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.BarNote = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _note = __webpack_require__(6);

var _stavebarline = __webpack_require__(34);

var _boundingbox = __webpack_require__(10);

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// A `BarNote` is used to render bar lines (from `barline.js`). `BarNote`s can
// be added to a voice and rendered in the middle of a stave. Since it has no
// duration, it consumes no `tick`s, and is dealt with appropriately by the formatter.
//
// See `tests/barnote_tests.js` for usage examples.

// To enable logging for this class. Set `Vex.Flow.BarNote.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (BarNote.DEBUG) _vex.Vex.L('Vex.Flow.BarNote', args);
}

var BarNote = exports.BarNote = function (_Note) {
  _inherits(BarNote, _Note);

  function BarNote() {
    var _this$metrics$widths;

    var type = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : _stavebarline.Barline.type.SINGLE;

    _classCallCheck(this, BarNote);

    var _this = _possibleConstructorReturn(this, (BarNote.__proto__ || Object.getPrototypeOf(BarNote)).call(this, { duration: 'b' }));

    _this.setAttribute('type', 'BarNote');

    _this.metrics = {
      widths: {}
    };

    var TYPE = _stavebarline.Barline.type;
    _this.metrics.widths = (_this$metrics$widths = {}, _defineProperty(_this$metrics$widths, TYPE.SINGLE, 8), _defineProperty(_this$metrics$widths, TYPE.DOUBLE, 12), _defineProperty(_this$metrics$widths, TYPE.END, 15), _defineProperty(_this$metrics$widths, TYPE.REPEAT_BEGIN, 14), _defineProperty(_this$metrics$widths, TYPE.REPEAT_END, 14), _defineProperty(_this$metrics$widths, TYPE.REPEAT_BOTH, 18), _defineProperty(_this$metrics$widths, TYPE.NONE, 0), _this$metrics$widths);

    // Tell the formatter that bar notes have no duration.
    _this.ignore_ticks = true;
    _this.setType(type);
    return _this;
  }

  // Get and set the type of Bar note. `type` must be one of `Vex.Flow.Barline.type`.


  _createClass(BarNote, [{
    key: 'getType',
    value: function getType() {
      return this.type;
    }
  }, {
    key: 'setType',
    value: function setType(type) {
      this.type = typeof type === 'string' ? _stavebarline.Barline.typeString[type] : type;

      // Set width to width of relevant `Barline`.
      this.setWidth(this.metrics.widths[this.type]);
      return this;
    }
  }, {
    key: 'getBoundingBox',
    value: function getBoundingBox() {
      return new _boundingbox.BoundingBox(0, 0, 0, 0);
    }
  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext() {
      /* overridden to ignore */
      return this;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      /* overridden to ignore */
      this.setPreFormatted(true);
      return this;
    }

    // Render note to stave.

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      if (!this.stave) throw new _vex.Vex.RERR('NoStave', "Can't draw without a stave.");
      L('Rendering bar line at: ', this.getAbsoluteX());
      var barline = new _stavebarline.Barline(this.type);
      barline.setX(this.getAbsoluteX());
      barline.draw(this.stave);
      this.setRendered();
    }
  }]);

  return BarNote;
}(_note.Note);

/***/ }),
/* 59 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.GhostNote = undefined;

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _vex = __webpack_require__(0);

var _stemmablenote = __webpack_require__(20);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description

var GhostNote = exports.GhostNote = function (_StemmableNote) {
  _inherits(GhostNote, _StemmableNote);

  /** @constructor */
  function GhostNote(parameter) {
    _classCallCheck(this, GhostNote);

    // Sanity check
    if (!parameter) {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Ghost note must have valid initialization data to identify ' + 'duration.');
    }

    var note_struct = void 0;

    // Preserve backwards-compatibility
    if (typeof parameter === 'string') {
      note_struct = { duration: parameter };
    } else if ((typeof parameter === 'undefined' ? 'undefined' : _typeof(parameter)) === 'object') {
      note_struct = parameter;
    } else {
      throw new _vex.Vex.RuntimeError('BadArguments', 'Ghost note must have valid initialization data to identify ' + 'duration.');
    }

    var _this = _possibleConstructorReturn(this, (GhostNote.__proto__ || Object.getPrototypeOf(GhostNote)).call(this, note_struct));

    _this.setAttribute('type', 'GhostNote');

    // Note properties
    _this.setWidth(0);
    return _this;
  }

  _createClass(GhostNote, [{
    key: 'isRest',
    value: function isRest() {
      return true;
    }
  }, {
    key: 'setStave',
    value: function setStave(stave) {
      _get(GhostNote.prototype.__proto__ || Object.getPrototypeOf(GhostNote.prototype), 'setStave', this).call(this, stave);
    }
  }, {
    key: 'addToModifierContext',
    value: function addToModifierContext() {
      /* intentionally overridden */return this;
    }
  }, {
    key: 'preFormat',
    value: function preFormat() {
      this.setPreFormatted(true);
      return this;
    }
  }, {
    key: 'draw',
    value: function draw() {
      if (!this.stave) throw new _vex.Vex.RERR('NoStave', "Can't draw without a stave.");

      // Draw the modifiers
      this.setRendered();
      for (var i = 0; i < this.modifiers.length; ++i) {
        var modifier = this.modifiers[i];
        modifier.setContext(this.context);
        modifier.draw();
      }
    }
  }]);

  return GhostNote;
}(_stemmablenote.StemmableNote);

/***/ }),
/* 60 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.System = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }; // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// This class implements a musical system, which is a collection of staves,
// each which can have one or more voices. All voices across all staves in
// the system are formatted together.

var _element = __webpack_require__(3);

var _factory = __webpack_require__(61);

var _formatter = __webpack_require__(11);

var _note = __webpack_require__(6);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

function setDefaults(params, defaults) {
  var default_options = defaults.options;
  params = _extends(defaults, params);
  params.options = _extends(default_options, params.options);
  return params;
}

var System = exports.System = function (_Element) {
  _inherits(System, _Element);

  function System() {
    var params = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

    _classCallCheck(this, System);

    var _this = _possibleConstructorReturn(this, (System.__proto__ || Object.getPrototypeOf(System)).call(this));

    _this.setAttribute('type', 'System');
    _this.setOptions(params);
    _this.parts = [];
    return _this;
  }

  _createClass(System, [{
    key: 'setOptions',
    value: function setOptions() {
      var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      this.options = setDefaults(options, {
        x: 10,
        y: 10,
        width: 500,
        connector: null,
        spaceBetweenStaves: 12, // stave spaces
        factory: null,
        debugFormatter: false,
        formatIterations: 0, // number of formatter tuning steps
        options: {}
      });

      this.factory = this.options.factory || new _factory.Factory({ renderer: { el: null } });
    }
  }, {
    key: 'setContext',
    value: function setContext(context) {
      _get(System.prototype.__proto__ || Object.getPrototypeOf(System.prototype), 'setContext', this).call(this, context);
      this.factory.setContext(context);
      return this;
    }
  }, {
    key: 'addConnector',
    value: function addConnector() {
      var type = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 'double';

      this.connector = this.factory.StaveConnector({
        top_stave: this.parts[0].stave,
        bottom_stave: this.parts[this.parts.length - 1].stave,
        type: type
      });
      return this.connector;
    }
  }, {
    key: 'addStave',
    value: function addStave(params) {
      var _this2 = this;

      params = setDefaults(params, {
        stave: null,
        voices: [],
        spaceAbove: 0, // stave spaces
        spaceBelow: 0, // stave spaces
        debugNoteMetrics: false,
        options: {}
      });

      if (!params.stave) {
        var options = { left_bar: false };
        params.stave = this.factory.Stave({
          x: this.options.x,
          y: this.options.y,
          width: this.options.width,
          options: options
        });
      }

      params.voices.forEach(function (voice) {
        return voice.setContext(_this2.context).setStave(params.stave).getTickables().forEach(function (tickable) {
          return tickable.setStave(params.stave);
        });
      });

      this.parts.push(params);
      return params.stave;
    }
  }, {
    key: 'format',
    value: function format() {
      var _this3 = this;

      var formatter = new _formatter.Formatter();
      this.formatter = formatter;

      var y = this.options.y;
      var startX = 0;
      var allVoices = [];
      var debugNoteMetricsYs = [];

      // Join the voices for each stave.
      this.parts.forEach(function (part) {
        y = y + part.stave.space(part.spaceAbove);
        part.stave.setY(y);
        formatter.joinVoices(part.voices);
        y = y + part.stave.space(part.spaceBelow);
        y = y + part.stave.space(_this3.options.spaceBetweenStaves);
        if (part.debugNoteMetrics) {
          debugNoteMetricsYs.push({ y: y, voice: part.voices[0] });
          y += 15;
        }
        allVoices = allVoices.concat(part.voices);

        startX = Math.max(startX, part.stave.getNoteStartX());
      });

      // Update the start position of all staves.
      this.parts.forEach(function (part) {
        return part.stave.setNoteStartX(startX);
      });
      var justifyWidth = this.options.width - (startX - this.options.x) - _note.Note.STAVEPADDING;
      formatter.format(allVoices, justifyWidth);

      for (var i = 0; i < this.options.formatIterations; i++) {
        formatter.tune();
      }

      this.startX = startX;
      this.debugNoteMetricsYs = debugNoteMetricsYs;
      this.lastY = y;
    }
  }, {
    key: 'draw',
    value: function draw() {
      // Render debugging information, if requested.
      var ctx = this.checkContext();
      this.setRendered();

      if (this.options.debugFormatter) {
        _formatter.Formatter.plotDebugging(ctx, this.formatter, this.startX, this.options.y, this.lastY);
      }

      this.debugNoteMetricsYs.forEach(function (d) {
        d.voice.getTickables().forEach(function (note) {
          return _note.Note.plotMetrics(ctx, note, d.y);
        });
      });
    }
  }]);

  return System;
}(_element.Element);

/***/ }),
/* 61 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Factory = exports.X = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; }; // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// @author Mohit Cheppudira
//
// ## Description
//
// This file implements a high level API around VexFlow. It will eventually
// become the canonical way to use VexFlow.
//
// *This API is currently DRAFT*

var _vex = __webpack_require__(0);

var _accidental = __webpack_require__(24);

var _articulation = __webpack_require__(17);

var _annotation = __webpack_require__(30);

var _formatter = __webpack_require__(11);

var _frethandfinger = __webpack_require__(23);

var _stringnumber = __webpack_require__(29);

var _textdynamics = __webpack_require__(54);

var _modifiercontext = __webpack_require__(22);

var _renderer = __webpack_require__(14);

var _stave = __webpack_require__(33);

var _stavetie = __webpack_require__(16);

var _staveline = __webpack_require__(55);

var _stavenote = __webpack_require__(5);

var _staveconnector = __webpack_require__(19);

var _system = __webpack_require__(60);

var _tickcontext = __webpack_require__(13);

var _tuplet = __webpack_require__(18);

var _voice = __webpack_require__(12);

var _beam = __webpack_require__(15);

var _curve = __webpack_require__(53);

var _gracenote = __webpack_require__(52);

var _gracenotegroup = __webpack_require__(27);

var _notesubgroup = __webpack_require__(26);

var _easyscore = __webpack_require__(62);

var _timesignote = __webpack_require__(51);

var _clefnote = __webpack_require__(50);

var _pedalmarking = __webpack_require__(56);

var _textbracket = __webpack_require__(57);

var _vibratobracket = __webpack_require__(49);

var _ghostnote = __webpack_require__(59);

var _barnote = __webpack_require__(58);

var _tabnote = __webpack_require__(38);

var _tabstave = __webpack_require__(48);

var _textnote = __webpack_require__(35);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// To enable logging for this class. Set `Vex.Flow.Factory.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Factory.DEBUG) _vex.Vex.L('Vex.Flow.Factory', args);
}

var X = exports.X = _vex.Vex.MakeException('FactoryError');

function setDefaults() {
  var params = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var defaults = arguments[1];

  var default_options = defaults.options;
  params = _extends(defaults, params);
  params.options = _extends(default_options, params.options);
  return params;
}

var Factory = exports.Factory = function () {
  function Factory(options) {
    _classCallCheck(this, Factory);

    L('New factory: ', options);
    var defaults = {
      stave: {
        space: 10
      },
      renderer: {
        context: null,
        elementId: '',
        backend: _renderer.Renderer.Backends.SVG,
        width: 500,
        height: 200,
        background: '#FFF'
      },
      font: {
        face: 'Arial',
        point: 10,
        style: ''
      }
    };

    this.options = defaults;
    this.setOptions(options);
  }

  _createClass(Factory, [{
    key: 'reset',
    value: function reset() {
      this.renderQ = [];
      this.systems = [];
      this.staves = [];
      this.voices = [];
      this.stave = null; // current stave
    }
  }, {
    key: 'getOptions',
    value: function getOptions() {
      return this.options;
    }
  }, {
    key: 'setOptions',
    value: function setOptions(options) {
      var _arr = ['stave', 'renderer', 'font'];

      for (var _i = 0; _i < _arr.length; _i++) {
        var key = _arr[_i];
        _extends(this.options[key], options[key]);
      }
      if (this.options.renderer.elementId !== null || this.options.renderer.context) {
        this.initRenderer();
      }

      this.reset();
    }
  }, {
    key: 'initRenderer',
    value: function initRenderer() {
      var _options$renderer = this.options.renderer,
          elementId = _options$renderer.elementId,
          backend = _options$renderer.backend,
          width = _options$renderer.width,
          height = _options$renderer.height,
          background = _options$renderer.background;

      if (elementId === '') {
        throw new X('HTML DOM element not set in Factory');
      }

      this.context = _renderer.Renderer.buildContext(elementId, backend, width, height, background);
    }
  }, {
    key: 'getContext',
    value: function getContext() {
      return this.context;
    }
  }, {
    key: 'setContext',
    value: function setContext(context) {
      this.context = context;return this;
    }
  }, {
    key: 'getStave',
    value: function getStave() {
      return this.stave;
    }
  }, {
    key: 'getVoices',
    value: function getVoices() {
      return this.voices;
    }

    // Returns pixels from current stave spacing.

  }, {
    key: 'space',
    value: function space(spacing) {
      return this.options.stave.space * spacing;
    }
  }, {
    key: 'Stave',
    value: function Stave(params) {
      params = setDefaults(params, {
        x: 0,
        y: 0,
        width: this.options.renderer.width - this.space(1),
        options: {
          spacing_between_lines_px: this.options.stave.space
        }
      });

      var stave = new _stave.Stave(params.x, params.y, params.width, params.options);
      this.staves.push(stave);
      stave.setContext(this.context);
      this.stave = stave;
      return stave;
    }
  }, {
    key: 'TabStave',
    value: function TabStave(params) {
      params = setDefaults(params, {
        x: 0,
        y: 0,
        width: this.options.renderer.width - this.space(1),
        options: {
          spacing_between_lines_px: this.options.stave.space * 1.3
        }
      });

      var stave = new _tabstave.TabStave(params.x, params.y, params.width, params.options);
      this.staves.push(stave);
      stave.setContext(this.context);
      this.stave = stave;
      return stave;
    }
  }, {
    key: 'StaveNote',
    value: function StaveNote(noteStruct) {
      var note = new _stavenote.StaveNote(noteStruct);
      if (this.stave) note.setStave(this.stave);
      note.setContext(this.context);
      this.renderQ.push(note);
      return note;
    }
  }, {
    key: 'GhostNote',
    value: function GhostNote(noteStruct) {
      var ghostNote = new _ghostnote.GhostNote(noteStruct);
      if (this.stave) ghostNote.setStave(this.stave);
      ghostNote.setContext(this.context);
      this.renderQ.push(ghostNote);
      return ghostNote;
    }
  }, {
    key: 'TextNote',
    value: function TextNote(textNoteStruct) {
      var textNote = new _textnote.TextNote(textNoteStruct);
      if (this.stave) textNote.setStave(this.stave);
      textNote.setContext(this.context);
      this.renderQ.push(textNote);
      return textNote;
    }
  }, {
    key: 'BarNote',
    value: function BarNote(params) {
      params = setDefaults(params, {
        type: 'single',
        options: {}
      });

      var barNote = new _barnote.BarNote(params.type);
      if (this.stave) barNote.setStave(this.stave);
      barNote.setContext(this.context);
      this.renderQ.push(barNote);
      return barNote;
    }
  }, {
    key: 'ClefNote',
    value: function ClefNote(params) {
      params = setDefaults(params, {
        type: 'treble',
        options: {
          size: 'default'
        }
      });

      var clefNote = new _clefnote.ClefNote(params.type, params.options.size, params.options.annotation);
      if (this.stave) clefNote.setStave(this.stave);
      clefNote.setContext(this.context);
      this.renderQ.push(clefNote);
      return clefNote;
    }
  }, {
    key: 'TimeSigNote',
    value: function TimeSigNote(params) {
      params = setDefaults(params, {
        time: '4/4',
        options: {}
      });

      var timeSigNote = new _timesignote.TimeSigNote(params.time);
      if (this.stave) timeSigNote.setStave(this.stave);
      timeSigNote.setContext(this.context);
      this.renderQ.push(timeSigNote);
      return timeSigNote;
    }
  }, {
    key: 'TabNote',
    value: function TabNote(noteStruct) {
      var note = new _tabnote.TabNote(noteStruct);
      if (this.stave) note.setStave(this.stave);
      note.setContext(this.context);
      this.renderQ.push(note);
      return note;
    }
  }, {
    key: 'GraceNote',
    value: function GraceNote(noteStruct) {
      var note = new _gracenote.GraceNote(noteStruct);
      if (this.stave) note.setStave(this.stave);
      note.setContext(this.context);
      return note;
    }
  }, {
    key: 'GraceNoteGroup',
    value: function GraceNoteGroup(params) {
      var group = new _gracenotegroup.GraceNoteGroup(params.notes, params.slur);
      group.setContext(this.context);
      return group;
    }
  }, {
    key: 'Accidental',
    value: function Accidental(params) {
      params = setDefaults(params, {
        type: null,
        options: {}
      });

      var accid = new _accidental.Accidental(params.type);
      accid.setContext(this.context);
      return accid;
    }
  }, {
    key: 'Annotation',
    value: function Annotation(params) {
      params = setDefaults(params, {
        text: 'p',
        vJustify: 'below',
        hJustify: 'center',
        fontFamily: 'Times',
        fontSize: 14,
        fontWeight: 'bold italic',
        options: {}
      });

      var annotation = new _annotation.Annotation(params.text);
      annotation.setJustification(params.hJustify);
      annotation.setVerticalJustification(params.vJustify);
      annotation.setFont(params.fontFamily, params.fontSize, params.fontWeight);
      annotation.setContext(this.context);
      return annotation;
    }
  }, {
    key: 'Articulation',
    value: function Articulation(params) {
      params = setDefaults(params, {
        type: 'a.',
        position: 'above',
        options: {}
      });

      var articulation = new _articulation.Articulation(params.type);
      articulation.setPosition(params.position);
      articulation.setContext(this.context);
      return articulation;
    }
  }, {
    key: 'TextDynamics',
    value: function TextDynamics(params) {
      params = setDefaults(params, {
        text: 'p',
        duration: 'q',
        dots: 0,
        line: 0,
        options: {}
      });

      var text = new _textdynamics.TextDynamics({
        text: params.text,
        line: params.line,
        duration: params.duration,
        dots: params.dots
      });

      if (this.stave) text.setStave(this.stave);
      text.setContext(this.context);
      this.renderQ.push(text);
      return text;
    }
  }, {
    key: 'Fingering',
    value: function Fingering(params) {
      params = setDefaults(params, {
        number: '0',
        position: 'left',
        options: {}
      });

      var fingering = new _frethandfinger.FretHandFinger(params.number);
      fingering.setPosition(params.position);
      fingering.setContext(this.context);
      return fingering;
    }
  }, {
    key: 'StringNumber',
    value: function StringNumber(params) {
      params = setDefaults(params, {
        number: '0',
        position: 'left',
        options: {}
      });

      var stringNumber = new _stringnumber.StringNumber(params.number);
      stringNumber.setPosition(params.position);
      stringNumber.setContext(this.context);
      return stringNumber;
    }
  }, {
    key: 'TickContext',
    value: function TickContext() {
      return new _tickcontext.TickContext().setContext(this.context);
    }
  }, {
    key: 'ModifierContext',
    value: function ModifierContext() {
      return new _modifiercontext.ModifierContext();
    }
  }, {
    key: 'Voice',
    value: function Voice(params) {
      params = setDefaults(params, {
        time: '4/4',
        options: {}
      });
      var voice = new _voice.Voice(params.time);
      this.voices.push(voice);
      return voice;
    }
  }, {
    key: 'StaveConnector',
    value: function StaveConnector(params) {
      params = setDefaults(params, {
        top_stave: null,
        bottom_stave: null,
        type: 'double',
        options: {}
      });
      var connector = new _staveconnector.StaveConnector(params.top_stave, params.bottom_stave);
      connector.setType(params.type).setContext(this.context);
      this.renderQ.push(connector);
      return connector;
    }
  }, {
    key: 'Formatter',
    value: function Formatter() {
      return new _formatter.Formatter();
    }
  }, {
    key: 'Tuplet',
    value: function Tuplet(params) {
      params = setDefaults(params, {
        notes: [],
        options: {}
      });

      var tuplet = new _tuplet.Tuplet(params.notes, params.options).setContext(this.context);
      this.renderQ.push(tuplet);
      return tuplet;
    }
  }, {
    key: 'Beam',
    value: function Beam(params) {
      params = setDefaults(params, {
        notes: [],
        options: {
          autoStem: false,
          secondaryBeamBreaks: []
        }
      });

      var beam = new _beam.Beam(params.notes, params.options.autoStem).setContext(this.context);
      beam.breakSecondaryAt(params.options.secondaryBeamBreaks);
      this.renderQ.push(beam);
      return beam;
    }
  }, {
    key: 'Curve',
    value: function Curve(params) {
      params = setDefaults(params, {
        from: null,
        to: null,
        options: {}
      });

      var curve = new _curve.Curve(params.from, params.to, params.options).setContext(this.context);
      this.renderQ.push(curve);
      return curve;
    }
  }, {
    key: 'StaveTie',
    value: function StaveTie(params) {
      params = setDefaults(params, {
        from: null,
        to: null,
        first_indices: [0],
        last_indices: [0],
        text: null,
        options: {
          direction: undefined
        }
      });

      var tie = new _stavetie.StaveTie({
        first_note: params.from,
        last_note: params.to,
        first_indices: params.first_indices,
        last_indices: params.last_indices
      }, params.text);

      if (params.options.direction) tie.setDirection(params.options.direction);
      tie.setContext(this.context);
      this.renderQ.push(tie);
      return tie;
    }
  }, {
    key: 'StaveLine',
    value: function StaveLine(params) {
      params = setDefaults(params, {
        from: null,
        to: null,
        first_indices: [0],
        last_indices: [0],
        options: {}
      });

      var line = new _staveline.StaveLine({
        first_note: params.from,
        last_note: params.to,
        first_indices: params.first_indices,
        last_indices: params.last_indices
      });

      if (params.options.text) line.setText(params.options.text);
      if (params.options.font) line.setFont(params.options.font);

      line.setContext(this.context);
      this.renderQ.push(line);
      return line;
    }
  }, {
    key: 'VibratoBracket',
    value: function VibratoBracket(params) {
      params = setDefaults(params, {
        from: null,
        to: null,
        options: {
          harsh: false
        }
      });

      var vibratoBracket = new _vibratobracket.VibratoBracket({
        start: params.from,
        stop: params.to
      });

      if (params.options.line) vibratoBracket.setLine(params.options.line);
      if (params.options.harsh) vibratoBracket.setHarsh(params.options.harsh);

      vibratoBracket.setContext(this.context);
      this.renderQ.push(vibratoBracket);

      return vibratoBracket;
    }
  }, {
    key: 'TextBracket',
    value: function TextBracket(params) {
      params = setDefaults(params, {
        from: null,
        to: null,
        text: '',
        options: {
          superscript: '',
          position: 1
        }
      });

      var textBracket = new _textbracket.TextBracket({
        start: params.from,
        stop: params.to,
        text: params.text,
        superscript: params.options.superscript,
        position: params.options.position
      });

      if (params.options.line) textBracket.setLine(params.options.line);
      if (params.options.font) textBracket.setFont(params.options.font);

      textBracket.setContext(this.context);
      this.renderQ.push(textBracket);
      return textBracket;
    }
  }, {
    key: 'System',
    value: function System() {
      var params = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      params.factory = this;
      var system = new _system.System(params).setContext(this.context);
      this.systems.push(system);
      return system;
    }
  }, {
    key: 'EasyScore',
    value: function EasyScore() {
      var params = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      params.factory = this;
      return new _easyscore.EasyScore(params);
    }
  }, {
    key: 'PedalMarking',
    value: function PedalMarking() {
      var params = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      params = setDefaults(params, {
        notes: [],
        options: {
          style: 'mixed'
        }
      });

      var pedal = new _pedalmarking.PedalMarking(params.notes);
      pedal.setStyle(_pedalmarking.PedalMarking.StylesString[params.options.style]);
      pedal.setContext(this.context);
      this.renderQ.push(pedal);
      return pedal;
    }
  }, {
    key: 'NoteSubGroup',
    value: function NoteSubGroup() {
      var params = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      params = setDefaults(params, {
        notes: [],
        options: {}
      });

      var group = new _notesubgroup.NoteSubGroup(params.notes);
      group.setContext(this.context);
      return group;
    }
  }, {
    key: 'draw',
    value: function draw() {
      var _this = this;

      this.systems.forEach(function (i) {
        return i.setContext(_this.context).format();
      });
      this.staves.forEach(function (i) {
        return i.setContext(_this.context).draw();
      });
      this.voices.forEach(function (i) {
        return i.setContext(_this.context).draw();
      });
      this.renderQ.forEach(function (i) {
        if (!i.isRendered()) i.setContext(_this.context).draw();
      });
      this.systems.forEach(function (i) {
        return i.setContext(_this.context).draw();
      });
      this.reset();
    }
  }], [{
    key: 'newFromElementId',
    value: function newFromElementId(elementId) {
      var width = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 500;
      var height = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 200;

      return new Factory({ renderer: { elementId: elementId, width: width, height: height } });
    }
  }]);

  return Factory;
}();

/***/ }),
/* 62 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.EasyScore = exports.X = undefined;

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// This class implements a parser for a simple language to generate
// VexFlow objects.

var _vex = __webpack_require__(0);

var _stavenote = __webpack_require__(5);

var _parser = __webpack_require__(63);

var _articulation = __webpack_require__(17);

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// To enable logging for this class. Set `Vex.Flow.EasyScore.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (EasyScore.DEBUG) _vex.Vex.L('Vex.Flow.EasyScore', args);
}

var X = exports.X = _vex.Vex.MakeException('EasyScoreError');

var Grammar = function () {
  function Grammar(builder) {
    _classCallCheck(this, Grammar);

    this.builder = builder;
  }

  _createClass(Grammar, [{
    key: 'begin',
    value: function begin() {
      return this.LINE;
    }
  }, {
    key: 'LINE',
    value: function LINE() {
      return {
        expect: [this.PIECE, this.PIECES, this.EOL]
      };
    }
  }, {
    key: 'PIECE',
    value: function PIECE() {
      var _this = this;

      return {
        expect: [this.CHORDORNOTE, this.PARAMS],
        run: function run() {
          return _this.builder.commitPiece();
        }
      };
    }
  }, {
    key: 'PIECES',
    value: function PIECES() {
      return {
        expect: [this.COMMA, this.PIECE],
        zeroOrMore: true
      };
    }
  }, {
    key: 'PARAMS',
    value: function PARAMS() {
      return {
        expect: [this.DURATION, this.TYPE, this.DOTS, this.OPTS]
      };
    }
  }, {
    key: 'CHORDORNOTE',
    value: function CHORDORNOTE() {
      return {
        expect: [this.CHORD, this.SINGLENOTE],
        or: true
      };
    }
  }, {
    key: 'CHORD',
    value: function CHORD() {
      var _this2 = this;

      return {
        expect: [this.LPAREN, this.NOTES, this.RPAREN],
        run: function run(state) {
          return _this2.builder.addChord(state.matches[1]);
        }
      };
    }
  }, {
    key: 'NOTES',
    value: function NOTES() {
      return {
        expect: [this.NOTE],
        oneOrMore: true
      };
    }
  }, {
    key: 'NOTE',
    value: function NOTE() {
      return {
        expect: [this.NOTENAME, this.ACCIDENTAL, this.OCTAVE]
      };
    }
  }, {
    key: 'SINGLENOTE',
    value: function SINGLENOTE() {
      var _this3 = this;

      return {
        expect: [this.NOTENAME, this.ACCIDENTAL, this.OCTAVE],
        run: function run(state) {
          return _this3.builder.addSingleNote(state.matches[0], state.matches[1], state.matches[2]);
        }
      };
    }
  }, {
    key: 'ACCIDENTAL',
    value: function ACCIDENTAL() {
      return {
        expect: [this.ACCIDENTALS],
        maybe: true
      };
    }
  }, {
    key: 'DOTS',
    value: function DOTS() {
      var _this4 = this;

      return {
        expect: [this.DOT],
        zeroOrMore: true,
        run: function run(state) {
          return _this4.builder.setNoteDots(state.matches[0]);
        }
      };
    }
  }, {
    key: 'TYPE',
    value: function TYPE() {
      var _this5 = this;

      return {
        expect: [this.SLASH, this.MAYBESLASH, this.TYPES],
        maybe: true,
        run: function run(state) {
          return _this5.builder.setNoteType(state.matches[2]);
        }
      };
    }
  }, {
    key: 'DURATION',
    value: function DURATION() {
      var _this6 = this;

      return {
        expect: [this.SLASH, this.DURATIONS],
        maybe: true,
        run: function run(state) {
          return _this6.builder.setNoteDuration(state.matches[1]);
        }
      };
    }
  }, {
    key: 'OPTS',
    value: function OPTS() {
      return {
        expect: [this.LBRACKET, this.KEYVAL, this.KEYVALS, this.RBRACKET],
        maybe: true
      };
    }
  }, {
    key: 'KEYVALS',
    value: function KEYVALS() {
      return {
        expect: [this.COMMA, this.KEYVAL],
        zeroOrMore: true
      };
    }
  }, {
    key: 'KEYVAL',
    value: function KEYVAL() {
      var _this7 = this;

      var unquote = function unquote(str) {
        return str.slice(1, -1);
      };

      return {
        expect: [this.KEY, this.EQUALS, this.VAL],
        run: function run(state) {
          return _this7.builder.addNoteOption(state.matches[0], unquote(state.matches[2]));
        }
      };
    }
  }, {
    key: 'VAL',
    value: function VAL() {
      return {
        expect: [this.SVAL, this.DVAL],
        or: true
      };
    }
  }, {
    key: 'KEY',
    value: function KEY() {
      return { token: '[a-zA-Z][a-zA-Z0-9]*' };
    }
  }, {
    key: 'DVAL',
    value: function DVAL() {
      return { token: '["][^"]*["]' };
    }
  }, {
    key: 'SVAL',
    value: function SVAL() {
      return { token: "['][^']*[']" };
    }
  }, {
    key: 'NOTENAME',
    value: function NOTENAME() {
      return { token: '[a-gA-G]' };
    }
  }, {
    key: 'OCTAVE',
    value: function OCTAVE() {
      return { token: '[0-9]+' };
    }
  }, {
    key: 'ACCIDENTALS',
    value: function ACCIDENTALS() {
      return { token: 'bbs|bb|bss|bs|b|db|d|##|#|n|\\+\\+-|\\+-|\\+\\+|\\+|k|o' };
    }
  }, {
    key: 'DURATIONS',
    value: function DURATIONS() {
      return { token: '[0-9whq]+' };
    }
  }, {
    key: 'TYPES',
    value: function TYPES() {
      return { token: '[rRsSxX]' };
    }
  }, {
    key: 'LPAREN',
    value: function LPAREN() {
      return { token: '[(]' };
    }
  }, {
    key: 'RPAREN',
    value: function RPAREN() {
      return { token: '[)]' };
    }
  }, {
    key: 'COMMA',
    value: function COMMA() {
      return { token: '[,]' };
    }
  }, {
    key: 'DOT',
    value: function DOT() {
      return { token: '[.]' };
    }
  }, {
    key: 'SLASH',
    value: function SLASH() {
      return { token: '[/]' };
    }
  }, {
    key: 'MAYBESLASH',
    value: function MAYBESLASH() {
      return { token: '[/]?' };
    }
  }, {
    key: 'EQUALS',
    value: function EQUALS() {
      return { token: '[=]' };
    }
  }, {
    key: 'LBRACKET',
    value: function LBRACKET() {
      return { token: '\\[' };
    }
  }, {
    key: 'RBRACKET',
    value: function RBRACKET() {
      return { token: '\\]' };
    }
  }, {
    key: 'EOL',
    value: function EOL() {
      return { token: '$' };
    }
  }]);

  return Grammar;
}();

var Builder = function () {
  function Builder(factory) {
    _classCallCheck(this, Builder);

    this.factory = factory;
    this.commitHooks = [];
    this.reset();
  }

  _createClass(Builder, [{
    key: 'reset',
    value: function reset() {
      var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      this.options = {
        stem: 'auto',
        clef: 'treble'
      };
      this.elements = {
        notes: [],
        accidentals: []
      };
      this.rollingDuration = '8';
      this.resetPiece();
      _extends(this.options, options);
    }
  }, {
    key: 'getFactory',
    value: function getFactory() {
      return this.factory;
    }
  }, {
    key: 'getElements',
    value: function getElements() {
      return this.elements;
    }
  }, {
    key: 'addCommitHook',
    value: function addCommitHook(commitHook) {
      this.commitHooks.push(commitHook);
    }
  }, {
    key: 'resetPiece',
    value: function resetPiece() {
      L('resetPiece');
      this.piece = {
        chord: [],
        duration: this.rollingDuration,
        dots: 0,
        type: undefined,
        options: {}
      };
    }
  }, {
    key: 'setNoteDots',
    value: function setNoteDots(dots) {
      L('setNoteDots:', dots);
      if (dots) this.piece.dots = dots.length;
    }
  }, {
    key: 'setNoteDuration',
    value: function setNoteDuration(duration) {
      L('setNoteDuration:', duration);
      this.rollingDuration = this.piece.duration = duration || this.rollingDuration;
    }
  }, {
    key: 'setNoteType',
    value: function setNoteType(type) {
      L('setNoteType:', type);
      if (type) this.piece.type = type;
    }
  }, {
    key: 'addNoteOption',
    value: function addNoteOption(key, value) {
      L('addNoteOption: key:', key, 'value:', value);
      this.piece.options[key] = value;
    }
  }, {
    key: 'addNote',
    value: function addNote(key, accid, octave) {
      L('addNote:', key, accid, octave);
      this.piece.chord.push({ key: key, accid: accid, octave: octave });
    }
  }, {
    key: 'addSingleNote',
    value: function addSingleNote(key, accid, octave) {
      L('addSingleNote:', key, accid, octave);
      this.addNote(key, accid, octave);
    }
  }, {
    key: 'addChord',
    value: function addChord(notes) {
      var _this8 = this;

      L('startChord');
      if (_typeof(notes[0]) !== 'object') {
        this.addSingleNote(notes[0]);
      } else {
        notes.forEach(function (n) {
          if (n) _this8.addNote.apply(_this8, _toConsumableArray(n));
        });
      }
      L('endChord');
    }
  }, {
    key: 'commitPiece',
    value: function commitPiece() {
      var _this9 = this;

      L('commitPiece');
      var factory = this.factory;


      if (!factory) return;

      var options = _extends({}, this.options, this.piece.options);
      var stem = options.stem,
          clef = options.clef;

      var autoStem = stem.toLowerCase() === 'auto';
      var stemDirection = !autoStem && stem.toLowerCase() === 'up' ? _stavenote.StaveNote.STEM_UP : _stavenote.StaveNote.STEM_DOWN;

      // Build StaveNotes.
      var _piece = this.piece,
          chord = _piece.chord,
          duration = _piece.duration,
          dots = _piece.dots,
          type = _piece.type;

      var keys = chord.map(function (note) {
        return note.key + '/' + note.octave;
      });
      var note = factory.StaveNote({
        keys: keys,
        duration: duration,
        dots: dots,
        type: type,
        clef: clef,
        auto_stem: autoStem
      });
      if (!autoStem) note.setStemDirection(stemDirection);

      // Attach accidentals.
      var accids = chord.map(function (note) {
        return note.accid || null;
      });
      accids.forEach(function (accid, i) {
        if (accid) note.addAccidental(i, factory.Accidental({ type: accid }));
      });

      // Attach dots.
      for (var i = 0; i < dots; i++) {
        note.addDotToAll();
      }this.commitHooks.forEach(function (fn) {
        return fn(options, note, _this9);
      });

      this.elements.notes.push(note);
      this.elements.accidentals.concat(accids);
      this.resetPiece();
    }
  }]);

  return Builder;
}();

function setId(_ref, note) {
  var id = _ref.id;

  if (id === undefined) return;

  note.setAttribute('id', id);
}

function setClass(options, note) {
  if (!options.class) return;

  var commaSeparatedRegex = /\s*,\s*/;

  options.class.split(commaSeparatedRegex).forEach(function (className) {
    return note.addClass(className);
  });
}

var EasyScore = exports.EasyScore = function () {
  function EasyScore() {
    var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

    _classCallCheck(this, EasyScore);

    this.setOptions(options);
    this.defaults = {
      clef: 'treble',
      time: '4/4',
      stem: 'auto'
    };
  }

  _createClass(EasyScore, [{
    key: 'set',
    value: function set(defaults) {
      _extends(this.defaults, defaults);
      return this;
    }
  }, {
    key: 'setOptions',
    value: function setOptions(options) {
      var _this10 = this;

      this.options = _extends({
        factory: null,
        builder: null,
        commitHooks: [setId, setClass, _articulation.Articulation.easyScoreHook],
        throwOnError: false
      }, options);

      this.factory = this.options.factory;
      this.builder = this.options.builder || new Builder(this.factory);
      this.grammar = new Grammar(this.builder);
      this.parser = new _parser.Parser(this.grammar);
      this.options.commitHooks.forEach(function (commitHook) {
        return _this10.addCommitHook(commitHook);
      });
      return this;
    }
  }, {
    key: 'setContext',
    value: function setContext(context) {
      if (this.factory) this.factory.setContext(context);
      return this;
    }
  }, {
    key: 'parse',
    value: function parse(line) {
      var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

      this.builder.reset(options);
      var result = this.parser.parse(line);
      if (!result.success && this.options.throwOnError) {
        throw new X('Error parsing line: ' + line, result);
      }
      return result;
    }
  }, {
    key: 'beam',
    value: function beam(notes) {
      var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

      this.factory.Beam({ notes: notes, options: options });
      return notes;
    }
  }, {
    key: 'tuplet',
    value: function tuplet(notes) {
      var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

      this.factory.Tuplet({ notes: notes, options: options });
      return notes;
    }
  }, {
    key: 'notes',
    value: function notes(line) {
      var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

      options = _extends({ clef: this.defaults.clef, stem: this.defaults.stem }, options);
      this.parse(line, options);
      return this.builder.getElements().notes;
    }
  }, {
    key: 'voice',
    value: function voice(notes, voiceOptions) {
      voiceOptions = _extends({ time: this.defaults.time }, voiceOptions);
      return this.factory.Voice(voiceOptions).addTickables(notes);
    }
  }, {
    key: 'addCommitHook',
    value: function addCommitHook(commitHook) {
      return this.builder.addCommitHook(commitHook);
    }
  }]);

  return EasyScore;
}();

/***/ }),
/* 63 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Parser = exports.X = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// A generic text parsing class for VexFlow.

var _vex = __webpack_require__(0);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// To enable logging for this class. Set `Vex.Flow.Parser.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Parser.DEBUG) _vex.Vex.L('Vex.Flow.Parser', args);
}

var X = exports.X = _vex.Vex.MakeException('ParserError');

// Converts parser results into an easy to reference list that can be
// used in triggers.
function flattenMatches(results) {
  if (results.matchedString !== undefined) return results.matchedString;
  if (results.results) return flattenMatches(results.results);
  if (results.length === 1) return flattenMatches(results[0]);
  if (results.length === 0) return null;
  return results.map(flattenMatches);
}

// This is the base parser class. Given an arbitrary context-free grammar, it
// can parse any line and execute code when specific rules are met (e.g.,
// when a string is terminated.)

var Parser = exports.Parser = function () {
  // For an example of a simple grammar, take a look at tests/parser_tests.js or
  // the EasyScore grammar in easyscore.js.
  function Parser(grammar) {
    _classCallCheck(this, Parser);

    this.grammar = grammar;
  }

  // Parse `line` using current grammar. Returns {success: true} if the
  // line parsed correctly, otherwise returns `{success: false, errorPos: N}`
  // where `errorPos` is the location of the error in the string.


  _createClass(Parser, [{
    key: 'parse',
    value: function parse(line) {
      this.line = line;
      this.pos = 0;
      this.errorPos = -1;
      var results = this.expect(this.grammar.begin());
      results.errorPos = this.errorPos;
      return results;
    }
  }, {
    key: 'matchFail',
    value: function matchFail(returnPos) {
      if (this.errorPos === -1) this.errorPos = this.pos;
      this.pos = returnPos;
    }
  }, {
    key: 'matchSuccess',
    value: function matchSuccess() {
      this.errorPos = -1;
    }

    // Look for `token` in this.line[this.pos], and return success
    // if one is found. `token` is specified as a regular expression.

  }, {
    key: 'matchToken',
    value: function matchToken(token) {
      var noSpace = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      var regexp = noSpace ? new RegExp('^((' + token + '))') : new RegExp('^((' + token + ')\\s*)');
      var workingLine = this.line.slice(this.pos);
      var result = workingLine.match(regexp);
      if (result !== null) {
        return {
          success: true,
          matchedString: result[2],
          incrementPos: result[1].length,
          pos: this.pos
        };
      } else {
        return {
          success: false,
          pos: this.pos
        };
      }
    }

    // Execute rule to match a sequence of tokens (or rules). If `maybe` is
    // set, then return success even if the token is not found, but reset
    // the position before exiting.

  }, {
    key: 'expectOne',
    value: function expectOne(rule) {
      var maybe = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      var results = [];
      var pos = this.pos;

      var allMatches = true;
      var oneMatch = false;
      maybe = maybe === true || rule.maybe === true;

      // Execute all sub rules in sequence.
      for (var i = 0; i < rule.expect.length; i++) {
        var next = rule.expect[i];
        var localPos = this.pos;
        var result = this.expect(next);

        // If `rule.or` is set, then return success if any one
        // of the subrules match, else all subrules must match.
        if (result.success) {
          results.push(result);
          oneMatch = true;
          if (rule.or) break;
        } else {
          allMatches = false;
          if (!rule.or) {
            this.pos = localPos;
            break;
          }
        }
      }

      var gotOne = rule.or && oneMatch || allMatches;
      var success = gotOne || maybe === true;
      if (maybe && !gotOne) this.pos = pos;
      if (success) this.matchSuccess();else this.matchFail(pos);
      return { success: success, results: results, numMatches: gotOne ? 1 : 0 };
    }

    // Try to match multiple (one or more) instances of the rule. If `maybe` is set,
    // then a failed match is also a success (but the position is reset).

  }, {
    key: 'expectOneOrMore',
    value: function expectOneOrMore(rule) {
      var maybe = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      var results = [];
      var pos = this.pos;
      var numMatches = 0;
      var more = true;

      do {
        var result = this.expectOne(rule);
        if (result.success) {
          numMatches++;
          results.push(result.results);
        } else {
          more = false;
        }
      } while (more);

      var success = numMatches > 0 || maybe === true;
      if (maybe && !(numMatches > 0)) this.pos = pos;
      if (success) this.matchSuccess();else this.matchFail(pos);
      return { success: success, results: results, numMatches: numMatches };
    }

    // Match zero or more instances of `rule`. Offloads to `expectOneOrMore`.

  }, {
    key: 'expectZeroOrMore',
    value: function expectZeroOrMore(rule) {
      return this.expectOneOrMore(rule, true);
    }

    // Execute the rule produced by the provided the `rules` function. This
    // ofloads to one of the above matchers and consolidates the results. It is also
    // responsible for executing any code triggered by the rule (in `rule.run`.)

  }, {
    key: 'expect',
    value: function expect(rules) {
      L('Evaluating rules:', rules);
      var result = void 0;
      if (!rules) {
        throw new X('Invalid Rule: ' + rules, rules);
      }

      // Get rule from Grammar class.
      var rule = rules.bind(this.grammar)();

      if (rule.token) {
        // Base case: parse the regex and throw an error if the
        // line doesn't match.
        result = this.matchToken(rule.token, rule.noSpace === true);
        if (result.success) {
          // Token match! Update position and throw away parsed portion
          // of string.
          this.pos += result.incrementPos;
        }
      } else if (rule.expect) {
        if (rule.oneOrMore) {
          result = this.expectOneOrMore(rule);
        } else if (rule.zeroOrMore) {
          result = this.expectZeroOrMore(rule);
        } else {
          result = this.expectOne(rule);
        }
      } else {
        throw new X('Bad grammar! No `token` or `expect` property', rule);
      }

      // If there's a trigger attached to this rule, then pull it.
      result.matches = [];
      if (result.results) result.results.forEach(function (r) {
        return result.matches.push(flattenMatches(r));
      });
      if (rule.run && result.success) rule.run(result);
      return result;
    }
  }]);

  return Parser;
}();

/***/ }),
/* 64 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

var _element = __webpack_require__(3);

var _fraction = __webpack_require__(8);

var _renderer = __webpack_require__(14);

var _formatter = __webpack_require__(11);

var _music = __webpack_require__(25);

var _glyph = __webpack_require__(2);

var _stave = __webpack_require__(33);

var _stavenote = __webpack_require__(5);

var _stavemodifier = __webpack_require__(7);

var _voice = __webpack_require__(12);

var _accidental = __webpack_require__(24);

var _beam = __webpack_require__(15);

var _stavetie = __webpack_require__(16);

var _tabstave = __webpack_require__(48);

var _tabnote = __webpack_require__(38);

var _bend = __webpack_require__(31);

var _vibrato = __webpack_require__(32);

var _vibratobracket = __webpack_require__(49);

var _note = __webpack_require__(6);

var _modifiercontext = __webpack_require__(22);

var _tickcontext = __webpack_require__(13);

var _articulation = __webpack_require__(17);

var _annotation = __webpack_require__(30);

var _stavebarline = __webpack_require__(34);

var _notehead = __webpack_require__(41);

var _staveconnector = __webpack_require__(19);

var _clefnote = __webpack_require__(50);

var _keysignature = __webpack_require__(46);

var _timesignature = __webpack_require__(37);

var _timesignote = __webpack_require__(51);

var _stem = __webpack_require__(9);

var _tabtie = __webpack_require__(28);

var _clef = __webpack_require__(36);

var _modifier = __webpack_require__(4);

var _tabslide = __webpack_require__(72);

var _tuplet = __webpack_require__(18);

var _gracenote = __webpack_require__(52);

var _gracetabnote = __webpack_require__(73);

var _tuning = __webpack_require__(74);

var _keymanager = __webpack_require__(75);

var _stavehairpin = __webpack_require__(76);

var _boundingbox = __webpack_require__(10);

var _strokes = __webpack_require__(43);

var _textnote = __webpack_require__(35);

var _curve = __webpack_require__(53);

var _textdynamics = __webpack_require__(54);

var _staveline = __webpack_require__(55);

var _ornament = __webpack_require__(44);

var _pedalmarking = __webpack_require__(56);

var _textbracket = __webpack_require__(57);

var _frethandfinger = __webpack_require__(23);

var _staverepetition = __webpack_require__(45);

var _barnote = __webpack_require__(58);

var _ghostnote = __webpack_require__(59);

var _notesubgroup = __webpack_require__(26);

var _gracenotegroup = __webpack_require__(27);

var _tremolo = __webpack_require__(77);

var _stringnumber = __webpack_require__(29);

var _crescendo = __webpack_require__(78);

var _stavevolta = __webpack_require__(47);

var _vexflow_font = __webpack_require__(40);

var _system = __webpack_require__(60);

var _factory = __webpack_require__(61);

var _parser = __webpack_require__(63);

var _easyscore = __webpack_require__(62);

var _registry = __webpack_require__(39);

// [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.

_vex.Vex.Flow = _tables.Flow;
_vex.Vex.Flow.Element = _element.Element;
_vex.Vex.Flow.Fraction = _fraction.Fraction;
_vex.Vex.Flow.Renderer = _renderer.Renderer;
_vex.Vex.Flow.Formatter = _formatter.Formatter;
_vex.Vex.Flow.Music = _music.Music;
_vex.Vex.Flow.Glyph = _glyph.Glyph;
_vex.Vex.Flow.Stave = _stave.Stave;
_vex.Vex.Flow.StaveNote = _stavenote.StaveNote;
_vex.Vex.Flow.StaveModifier = _stavemodifier.StaveModifier;
_vex.Vex.Flow.Voice = _voice.Voice;
_vex.Vex.Flow.Accidental = _accidental.Accidental;
_vex.Vex.Flow.Beam = _beam.Beam;
_vex.Vex.Flow.StaveTie = _stavetie.StaveTie;
_vex.Vex.Flow.TabStave = _tabstave.TabStave;
_vex.Vex.Flow.TabNote = _tabnote.TabNote;
_vex.Vex.Flow.Bend = _bend.Bend;
_vex.Vex.Flow.Vibrato = _vibrato.Vibrato;
_vex.Vex.Flow.VibratoBracket = _vibratobracket.VibratoBracket;
_vex.Vex.Flow.Note = _note.Note;
_vex.Vex.Flow.ModifierContext = _modifiercontext.ModifierContext;
_vex.Vex.Flow.TickContext = _tickcontext.TickContext;
_vex.Vex.Flow.Articulation = _articulation.Articulation;
_vex.Vex.Flow.Annotation = _annotation.Annotation;
_vex.Vex.Flow.Barline = _stavebarline.Barline;
_vex.Vex.Flow.NoteHead = _notehead.NoteHead;
_vex.Vex.Flow.StaveConnector = _staveconnector.StaveConnector;
_vex.Vex.Flow.ClefNote = _clefnote.ClefNote;
_vex.Vex.Flow.KeySignature = _keysignature.KeySignature;
_vex.Vex.Flow.TimeSignature = _timesignature.TimeSignature;
_vex.Vex.Flow.TimeSigNote = _timesignote.TimeSigNote;
_vex.Vex.Flow.Stem = _stem.Stem;
_vex.Vex.Flow.TabTie = _tabtie.TabTie;
_vex.Vex.Flow.Clef = _clef.Clef;
_vex.Vex.Flow.Modifier = _modifier.Modifier;
_vex.Vex.Flow.TabSlide = _tabslide.TabSlide;
_vex.Vex.Flow.Tuplet = _tuplet.Tuplet;
_vex.Vex.Flow.GraceNote = _gracenote.GraceNote;
_vex.Vex.Flow.GraceTabNote = _gracetabnote.GraceTabNote;
_vex.Vex.Flow.Tuning = _tuning.Tuning;
_vex.Vex.Flow.KeyManager = _keymanager.KeyManager;
_vex.Vex.Flow.StaveHairpin = _stavehairpin.StaveHairpin;
_vex.Vex.Flow.BoundingBox = _boundingbox.BoundingBox;
_vex.Vex.Flow.Stroke = _strokes.Stroke;
_vex.Vex.Flow.TextNote = _textnote.TextNote;
_vex.Vex.Flow.Curve = _curve.Curve;
_vex.Vex.Flow.TextDynamics = _textdynamics.TextDynamics;
_vex.Vex.Flow.StaveLine = _staveline.StaveLine;
_vex.Vex.Flow.Ornament = _ornament.Ornament;
_vex.Vex.Flow.PedalMarking = _pedalmarking.PedalMarking;
_vex.Vex.Flow.TextBracket = _textbracket.TextBracket;
_vex.Vex.Flow.FretHandFinger = _frethandfinger.FretHandFinger;
_vex.Vex.Flow.Repetition = _staverepetition.Repetition;
_vex.Vex.Flow.BarNote = _barnote.BarNote;
_vex.Vex.Flow.GhostNote = _ghostnote.GhostNote;
_vex.Vex.Flow.NoteSubGroup = _notesubgroup.NoteSubGroup;
_vex.Vex.Flow.GraceNoteGroup = _gracenotegroup.GraceNoteGroup;
_vex.Vex.Flow.Tremolo = _tremolo.Tremolo;
_vex.Vex.Flow.StringNumber = _stringnumber.StringNumber;
_vex.Vex.Flow.Crescendo = _crescendo.Crescendo;
_vex.Vex.Flow.Volta = _stavevolta.Volta;
_vex.Vex.Flow.Font = _vexflow_font.Font;
_vex.Vex.Flow.System = _system.System;
_vex.Vex.Flow.Factory = _factory.Factory;
_vex.Vex.Flow.Parser = _parser.Parser;
_vex.Vex.Flow.EasyScore = _easyscore.EasyScore;
_vex.Vex.Flow.Registry = _registry.Registry;

exports.default = _vex.Vex;
module.exports = exports['default'];

/***/ }),
/* 65 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// ## Description
//
// Object which computes metrics for a bounding box by continuously
// taking canvas path commands

// Warning: This file is merely a crutch to get bounding box information without
// explicit metadata. This is likely to get deprecated following SMuFL support.
//
// taken from: https://github.com/gabelerner/canvg/blob/860e418aca67b9a41e858a223d74d375793ec364/ca
// nvg.js#L449

var BoundingBoxComputation = exports.BoundingBoxComputation = function () {
  function BoundingBoxComputation(x1, y1, x2, y2) {
    _classCallCheck(this, BoundingBoxComputation);

    // pass in initial points if you want
    this.x1 = Number.NaN;
    this.y1 = Number.NaN;
    this.x2 = Number.NaN;
    this.y2 = Number.NaN;

    this.addPoint(x1, y1);
    this.addPoint(x2, y2);
  }

  _createClass(BoundingBoxComputation, [{
    key: "width",
    value: function width() {
      return this.x2 - this.x1;
    }
  }, {
    key: "height",
    value: function height() {
      return this.y2 - this.y1;
    }
  }, {
    key: "addPoint",
    value: function addPoint(x, y) {
      if (x != null) {
        if (isNaN(this.x1) || isNaN(this.x2)) {
          this.x1 = x;
          this.x2 = x;
        }
        if (x < this.x1) this.x1 = x;
        if (x > this.x2) this.x2 = x;
      }

      if (y != null) {
        if (isNaN(this.y1) || isNaN(this.y2)) {
          this.y1 = y;
          this.y2 = y;
        }
        if (y < this.y1) this.y1 = y;
        if (y > this.y2) this.y2 = y;
      }
    }
  }, {
    key: "addX",
    value: function addX(x) {
      this.addPoint(x, null);
    }
  }, {
    key: "addY",
    value: function addY(y) {
      this.addPoint(null, y);
    }
  }, {
    key: "addQuadraticCurve",
    value: function addQuadraticCurve(p0x, p0y, p1x, p1y, p2x, p2y) {
      var cp1x = p0x + 2 / 3 * (p1x - p0x); // CP1 = QP0 + 2/3 *(QP1-QP0)
      var cp1y = p0y + 2 / 3 * (p1y - p0y); // CP1 = QP0 + 2/3 *(QP1-QP0)
      var cp2x = cp1x + 1 / 3 * (p2x - p0x); // CP2 = CP1 + 1/3 *(QP2-QP0)
      var cp2y = cp1y + 1 / 3 * (p2y - p0y); // CP2 = CP1 + 1/3 *(QP2-QP0)
      this.addBezierCurve(p0x, p0y, cp1x, cp1y, cp2x, cp2y, p2x, p2y);
    }
  }, {
    key: "addBezierCurve",
    value: function addBezierCurve(p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y) {
      // from http://blog.hackers-cafe.net/2009/06/how-to-calculate-bezier-curves-bounding.html
      var p0 = [p0x, p0y];
      var p1 = [p1x, p1y];
      var p2 = [p2x, p2y];
      var p3 = [p3x, p3y];
      var i = void 0;

      this.addPoint(p0[0], p0[1]);
      this.addPoint(p3[0], p3[1]);

      var f = function f(t, i) {
        return Math.pow(1 - t, 3) * p0[i] + 3 * Math.pow(1 - t, 2) * t * p1[i] + 3 * (1 - t) * Math.pow(t, 2) * p2[i] + Math.pow(t, 3) * p3[i];
      };

      for (i = 0; i <= 1; i++) {
        var b = 6 * p0[i] - 12 * p1[i] + 6 * p2[i];
        var a = -3 * p0[i] + 9 * p1[i] - 9 * p2[i] + 3 * p3[i];
        var c = 3 * p1[i] - 3 * p0[i];

        if (a === 0) {
          if (b === 0) continue;
          var t = -c / b;
          if (0 < t && t < 1) {
            if (i === 0) this.addX(f(t, i));
            if (i === 1) this.addY(f(t, i));
          }
          continue;
        }

        var b2ac = Math.pow(b, 2) - 4 * c * a;
        if (b2ac < 0) continue;
        var t1 = (-b + Math.sqrt(b2ac)) / (2 * a);
        if (0 < t1 && t1 < 1) {
          if (i === 0) this.addX(f(t1, i));
          if (i === 1) this.addY(f(t1, i));
        }
        var t2 = (-b - Math.sqrt(b2ac)) / (2 * a);
        if (0 < t2 && t2 < 1) {
          if (i === 0) this.addX(f(t2, i));
          if (i === 1) this.addY(f(t2, i));
        }
      }
    }
  }]);

  return BoundingBoxComputation;
}();

/***/ }),
/* 66 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Mohit Muthanna <mohit@muthanna.com>
//
// A rendering context for the Raphael backend.
//
// Copyright Mohit Cheppudira 2010

/** @constructor */
var CanvasContext = exports.CanvasContext = function () {
  _createClass(CanvasContext, null, [{
    key: 'WIDTH',
    get: function get() {
      return 600;
    }
  }, {
    key: 'HEIGHT',
    get: function get() {
      return 400;
    }
  }]);

  function CanvasContext(context) {
    _classCallCheck(this, CanvasContext);

    // Use a name that is unlikely to clash with a canvas context
    // property
    this.vexFlowCanvasContext = context;
    if (!context.canvas) {
      this.canvas = {
        width: CanvasContext.WIDTH,
        height: CanvasContext.HEIGHT
      };
    } else {
      this.canvas = context.canvas;
    }
  }

  _createClass(CanvasContext, [{
    key: 'clear',
    value: function clear() {
      this.vexFlowCanvasContext.clearRect(0, 0, this.canvas.width, this.canvas.height);
    }

    // Containers not implemented

  }, {
    key: 'openGroup',
    value: function openGroup() {}
  }, {
    key: 'closeGroup',
    value: function closeGroup() {}
  }, {
    key: 'add',
    value: function add() {}
  }, {
    key: 'setFont',
    value: function setFont(family, size, weight) {
      this.vexFlowCanvasContext.font = (weight || '') + ' ' + size + 'pt ' + family;
      return this;
    }
  }, {
    key: 'setRawFont',
    value: function setRawFont(font) {
      this.vexFlowCanvasContext.font = font;
      return this;
    }
  }, {
    key: 'setFillStyle',
    value: function setFillStyle(style) {
      this.vexFlowCanvasContext.fillStyle = style;
      return this;
    }
  }, {
    key: 'setBackgroundFillStyle',
    value: function setBackgroundFillStyle(style) {
      this.background_fillStyle = style;
      return this;
    }
  }, {
    key: 'setStrokeStyle',
    value: function setStrokeStyle(style) {
      this.vexFlowCanvasContext.strokeStyle = style;
      return this;
    }
  }, {
    key: 'setShadowColor',
    value: function setShadowColor(style) {
      this.vexFlowCanvasContext.shadowColor = style;
      return this;
    }
  }, {
    key: 'setShadowBlur',
    value: function setShadowBlur(blur) {
      this.vexFlowCanvasContext.shadowBlur = blur;
      return this;
    }
  }, {
    key: 'setLineWidth',
    value: function setLineWidth(width) {
      this.vexFlowCanvasContext.lineWidth = width;
      return this;
    }
  }, {
    key: 'setLineCap',
    value: function setLineCap(cap_type) {
      this.vexFlowCanvasContext.lineCap = cap_type;
      return this;
    }

    // setLineDash: is the one native method in a canvas context
    // that begins with set, therefore we don't bolster the method
    // if it already exists (see renderer.bolsterCanvasContext).
    // If it doesn't exist, we bolster it and assume it's looking for
    // a ctx.lineDash method, as previous versions of VexFlow
    // expected.

  }, {
    key: 'setLineDash',
    value: function setLineDash(dash) {
      this.vexFlowCanvasContext.lineDash = dash;
      return this;
    }
  }, {
    key: 'scale',
    value: function scale(x, y) {
      return this.vexFlowCanvasContext.scale(parseFloat(x), parseFloat(y));
    }
  }, {
    key: 'resize',
    value: function resize(width, height) {
      return this.vexFlowCanvasContext.resize(parseInt(width, 10), parseInt(height, 10));
    }
  }, {
    key: 'rect',
    value: function rect(x, y, width, height) {
      return this.vexFlowCanvasContext.rect(x, y, width, height);
    }
  }, {
    key: 'fillRect',
    value: function fillRect(x, y, width, height) {
      return this.vexFlowCanvasContext.fillRect(x, y, width, height);
    }
  }, {
    key: 'clearRect',
    value: function clearRect(x, y, width, height) {
      return this.vexFlowCanvasContext.clearRect(x, y, width, height);
    }
  }, {
    key: 'beginPath',
    value: function beginPath() {
      return this.vexFlowCanvasContext.beginPath();
    }
  }, {
    key: 'moveTo',
    value: function moveTo(x, y) {
      return this.vexFlowCanvasContext.moveTo(x, y);
    }
  }, {
    key: 'lineTo',
    value: function lineTo(x, y) {
      return this.vexFlowCanvasContext.lineTo(x, y);
    }
  }, {
    key: 'bezierCurveTo',
    value: function bezierCurveTo(x1, y1, x2, y2, x, y) {
      return this.vexFlowCanvasContext.bezierCurveTo(x1, y1, x2, y2, x, y);
    }
  }, {
    key: 'quadraticCurveTo',
    value: function quadraticCurveTo(x1, y1, x, y) {
      return this.vexFlowCanvasContext.quadraticCurveTo(x1, y1, x, y);
    }

    // This is an attempt (hack) to simulate the HTML5 canvas
    // arc method.

  }, {
    key: 'arc',
    value: function arc(x, y, radius, startAngle, endAngle, antiClockwise) {
      return this.vexFlowCanvasContext.arc(x, y, radius, startAngle, endAngle, antiClockwise);
    }

    // Adapted from the source for Raphael's Element.glow

  }, {
    key: 'glow',
    value: function glow() {
      return this.vexFlowCanvasContext.glow();
    }
  }, {
    key: 'fill',
    value: function fill() {
      return this.vexFlowCanvasContext.fill();
    }
  }, {
    key: 'stroke',
    value: function stroke() {
      return this.vexFlowCanvasContext.stroke();
    }
  }, {
    key: 'closePath',
    value: function closePath() {
      return this.vexFlowCanvasContext.closePath();
    }
  }, {
    key: 'measureText',
    value: function measureText(text) {
      return this.vexFlowCanvasContext.measureText(text);
    }
  }, {
    key: 'fillText',
    value: function fillText(text, x, y) {
      return this.vexFlowCanvasContext.fillText(text, x, y);
    }
  }, {
    key: 'save',
    value: function save() {
      return this.vexFlowCanvasContext.save();
    }
  }, {
    key: 'restore',
    value: function restore() {
      return this.vexFlowCanvasContext.restore();
    }
  }]);

  return CanvasContext;
}();

/***/ }),
/* 67 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// A rendering context for the Raphael backend.
//
// ## Warning: Deprecated for SVGContext
// Except in instances where SVG support for IE < 9.0 is
// needed, SVGContext is recommended.

var RaphaelContext = exports.RaphaelContext = function () {
  function RaphaelContext(element) {
    _classCallCheck(this, RaphaelContext);

    this.element = element;
    this.paper = Raphael(element); // eslint-disable-line
    this.path = '';
    this.pen = { x: 0, y: 0 };
    this.lineWidth = 1.0;
    this.state = {
      scale: { x: 1, y: 1 },
      font_family: 'Arial',
      font_size: 8,
      font_weight: 800
    };

    this.attributes = {
      'stroke-width': 0.3,
      'fill': 'black',
      'stroke': 'black',
      'font': '10pt Arial'
    };

    this.background_attributes = {
      'stroke-width': 0,
      'fill': 'white',
      'stroke': 'white',
      'font': '10pt Arial'
    };

    this.shadow_attributes = {
      width: 0,
      color: 'black'
    };

    this.state_stack = [];
  }

  // Containers not implemented


  _createClass(RaphaelContext, [{
    key: 'openGroup',
    value: function openGroup() {}
  }, {
    key: 'closeGroup',
    value: function closeGroup() {}
  }, {
    key: 'add',
    value: function add() {}
  }, {
    key: 'setFont',
    value: function setFont(family, size, weight) {
      this.state.font_family = family;
      this.state.font_size = size;
      this.state.font_weight = weight;
      this.attributes.font = (this.state.font_weight || '') + ' ' + this.state.font_size * this.state.scale.x + 'pt ' + this.state.font_family;
      return this;
    }
  }, {
    key: 'setRawFont',
    value: function setRawFont(font) {
      this.attributes.font = font;
      return this;
    }
  }, {
    key: 'setFillStyle',
    value: function setFillStyle(style) {
      this.attributes.fill = style;
      return this;
    }
  }, {
    key: 'setBackgroundFillStyle',
    value: function setBackgroundFillStyle(style) {
      this.background_attributes.fill = style;
      this.background_attributes.stroke = style;
      return this;
    }
  }, {
    key: 'setStrokeStyle',
    value: function setStrokeStyle(style) {
      this.attributes.stroke = style;
      return this;
    }
  }, {
    key: 'setShadowColor',
    value: function setShadowColor(style) {
      this.shadow_attributes.color = style;
      return this;
    }
  }, {
    key: 'setShadowBlur',
    value: function setShadowBlur(blur) {
      this.shadow_attributes.width = blur;
      return this;
    }
  }, {
    key: 'setLineWidth',
    value: function setLineWidth(width) {
      this.attributes['stroke-width'] = width;
      this.lineWidth = width;
    }

    // Empty because there is no equivalent in SVG

  }, {
    key: 'setLineDash',
    value: function setLineDash() {
      return this;
    }
  }, {
    key: 'setLineCap',
    value: function setLineCap() {
      return this;
    }
  }, {
    key: 'scale',
    value: function scale(x, y) {
      this.state.scale = { x: x, y: y };
      // The scale() method is deprecated as of Raphael.JS 2.0, and
      // can no longer be used as an option in an Element.attr() call.
      // It is preserved here for users running earlier versions of
      // Raphael.JS, though it has no effect on the SVG output in
      // Raphael 2 and higher.
      this.attributes.transform = 'S' + x + ',' + y + ',0,0';
      this.attributes.scale = x + ',' + y + ',0,0';
      this.attributes.font = this.state.font_size * this.state.scale.x + 'pt ' + this.state.font_family;
      this.background_attributes.transform = 'S' + x + ',' + y + ',0,0';
      this.background_attributes.font = this.state.font_size * this.state.scale.x + 'pt ' + this.state.font_family;
      return this;
    }
  }, {
    key: 'clear',
    value: function clear() {
      this.paper.clear();
    }
  }, {
    key: 'resize',
    value: function resize(width, height) {
      this.element.style.width = width;
      this.paper.setSize(width, height);
      return this;
    }

    // Sets the SVG `viewBox` property, which results in auto scaling images when its container
    // is resized.
    //
    // Usage: `ctx.setViewBox("0 0 600 400")`

  }, {
    key: 'setViewBox',
    value: function setViewBox(viewBox) {
      this.paper.canvas.setAttribute('viewBox', viewBox);
    }
  }, {
    key: 'rect',
    value: function rect(x, y, width, height) {
      if (height < 0) {
        y += height;
        height = -height;
      }

      this.paper.rect(x, y, width - 0.5, height - 0.5).attr(this.attributes).attr('fill', 'none').attr('stroke-width', this.lineWidth);
      return this;
    }
  }, {
    key: 'fillRect',
    value: function fillRect(x, y, width, height) {
      if (height < 0) {
        y += height;
        height = -height;
      }

      this.paper.rect(x, y, width - 0.5, height - 0.5).attr(this.attributes);
      return this;
    }
  }, {
    key: 'clearRect',
    value: function clearRect(x, y, width, height) {
      if (height < 0) {
        y += height;
        height = -height;
      }

      this.paper.rect(x, y, width - 0.5, height - 0.5).attr(this.background_attributes);
      return this;
    }
  }, {
    key: 'beginPath',
    value: function beginPath() {
      this.path = '';
      this.pen.x = 0;
      this.pen.y = 0;
      return this;
    }
  }, {
    key: 'moveTo',
    value: function moveTo(x, y) {
      this.path += 'M' + x + ',' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }
  }, {
    key: 'lineTo',
    value: function lineTo(x, y) {
      this.path += 'L' + x + ',' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }
  }, {
    key: 'bezierCurveTo',
    value: function bezierCurveTo(x1, y1, x2, y2, x, y) {
      this.path += 'C' + x1 + ',' + y1 + ',' + x2 + ',' + y2 + ',' + x + ',' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }
  }, {
    key: 'quadraticCurveTo',
    value: function quadraticCurveTo(x1, y1, x, y) {
      this.path += 'Q' + x1 + ',' + y1 + ',' + x + ',' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }

    // This is an attempt (hack) to simulate the HTML5 canvas
    // arc method.

  }, {
    key: 'arc',
    value: function arc(x, y, radius, startAngle, endAngle, antiClockwise) {
      function normalizeAngle(angle) {
        while (angle < 0) {
          angle += Math.PI * 2;
        }

        while (angle > Math.PI * 2) {
          angle -= Math.PI * 2;
        }
        return angle;
      }

      startAngle = normalizeAngle(startAngle);
      endAngle = normalizeAngle(endAngle);

      if (startAngle > endAngle) {
        var tmp = startAngle;
        startAngle = endAngle;
        endAngle = tmp;
        antiClockwise = !antiClockwise;
      }

      var delta = endAngle - startAngle;

      if (delta > Math.PI) {
        this.arcHelper(x, y, radius, startAngle, startAngle + delta / 2, antiClockwise);
        this.arcHelper(x, y, radius, startAngle + delta / 2, endAngle, antiClockwise);
      } else {
        this.arcHelper(x, y, radius, startAngle, endAngle, antiClockwise);
      }
      return this;
    }
  }, {
    key: 'arcHelper',
    value: function arcHelper(x, y, radius, startAngle, endAngle, antiClockwise) {
      var x1 = x + radius * Math.cos(startAngle);
      var y1 = y + radius * Math.sin(startAngle);

      var x2 = x + radius * Math.cos(endAngle);
      var y2 = y + radius * Math.sin(endAngle);

      var largeArcFlag = 0;
      var sweepFlag = 0;
      if (antiClockwise) {
        sweepFlag = 1;
        if (endAngle - startAngle < Math.PI) {
          largeArcFlag = 1;
        }
      } else if (endAngle - startAngle > Math.PI) {
        largeArcFlag = 1;
      }

      this.path += 'M' + x1 + ',' + y1 + ',A' + radius + ',' + radius + ',0,' + largeArcFlag + ',' + sweepFlag + ',' + x2 + ',' + y2 + 'M' + this.pen.x + ',' + this.pen.y;
    }

    // Adapted from the source for Raphael's Element.glow

  }, {
    key: 'glow',
    value: function glow() {
      var out = this.paper.set();
      if (this.shadow_attributes.width > 0) {
        var sa = this.shadow_attributes;
        var num_paths = sa.width / 2;
        for (var i = 1; i <= num_paths; i++) {
          out.push(this.paper.path(this.path).attr({
            stroke: sa.color,
            'stroke-linejoin': 'round',
            'stroke-linecap': 'round',
            'stroke-width': +(sa.width / num_paths * i).toFixed(3),
            opacity: +((sa.opacity || 0.3) / num_paths).toFixed(3),
            // See note in this.scale(): In Raphael the scale() method
            // is deprecated and removed as of Raphael 2.0 and replaced
            // by the transform() method.  It is preserved here for
            // users with earlier versions of Raphael, but has no effect
            // on the output SVG in Raphael 2.0+.
            transform: this.attributes.transform,
            scale: this.attributes.scale
          }));
        }
      }
      return out;
    }
  }, {
    key: 'fill',
    value: function fill() {
      var elem = this.paper.path(this.path).attr(this.attributes).attr('stroke-width', 0);
      this.glow(elem);
      return this;
    }
  }, {
    key: 'stroke',
    value: function stroke() {
      // The first line of code below is, unfortunately, a bit of a hack:
      // Raphael's transform() scaling does not scale the stroke-width, so
      // in order to scale a stroke, we have to manually scale the
      // stroke-width.
      //
      // This works well so long as the X & Y states for this.scale() are
      // relatively similar.  However, if they are very different, we
      // would expect horizontal and vertical lines to have different
      // stroke-widths.
      //
      // In the future, if we want to support very divergent values for
      // horizontal and vertical scaling, we may want to consider
      // implementing SVG scaling with properties of the SVG viewBox &
      // viewPort and removing it entirely from the Element.attr() calls.
      // This would more closely parallel the approach taken in
      // canvascontext.js as well.

      var strokeWidth = this.lineWidth * (this.state.scale.x + this.state.scale.y) / 2;
      var elem = this.paper.path(this.path).attr(this.attributes).attr('fill', 'none').attr('stroke-width', strokeWidth);
      this.glow(elem);
      return this;
    }
  }, {
    key: 'closePath',
    value: function closePath() {
      this.path += 'Z';
      return this;
    }
  }, {
    key: 'measureText',
    value: function measureText(text) {
      var txt = this.paper.text(0, 0, text).attr(this.attributes).attr('fill', 'none').attr('stroke', 'none');
      var bounds = txt.getBBox();
      txt.remove();

      return {
        width: bounds.width,
        height: bounds.height
      };
    }
  }, {
    key: 'fillText',
    value: function fillText(text, x, y) {
      this.paper.text(x + this.measureText(text).width / 2, y - this.state.font_size / (2.25 * this.state.scale.y), text).attr(this.attributes);

      return this;
    }
  }, {
    key: 'save',
    value: function save() {
      // TODO(mmuthanna): State needs to be deep-copied.
      this.state_stack.push({
        state: {
          font_family: this.state.font_family
        },
        attributes: {
          font: this.attributes.font,
          fill: this.attributes.fill,
          stroke: this.attributes.stroke,
          'stroke-width': this.attributes['stroke-width']
        },
        shadow_attributes: {
          width: this.shadow_attributes.width,
          color: this.shadow_attributes.color
        }
      });
      return this;
    }
  }, {
    key: 'restore',
    value: function restore() {
      // TODO(0xfe): State needs to be deep-restored.
      var state = this.state_stack.pop();
      this.state.font_family = state.state.font_family;
      this.attributes.font = state.attributes.font;
      this.attributes.fill = state.attributes.fill;
      this.attributes.stroke = state.attributes.stroke;
      this.attributes['stroke-width'] = state.attributes['stroke-width'];
      this.shadow_attributes.width = state.shadow_attributes.width;
      this.shadow_attributes.color = state.shadow_attributes.color;
      return this;
    }
  }]);

  return RaphaelContext;
}();

/***/ }),
/* 68 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.SVGContext = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// @author Gregory Ristow (2015)

var _vex = __webpack_require__(0);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var SVGContext = exports.SVGContext = function () {
  function SVGContext(element) {
    _classCallCheck(this, SVGContext);

    // element is the parent DOM object
    this.element = element;
    // Create the SVG in the SVG namespace:
    this.svgNS = 'http://www.w3.org/2000/svg';
    var svg = this.create('svg');
    // Add it to the canvas:
    this.element.appendChild(svg);

    // Point to it:
    this.svg = svg;
    this.groups = [this.svg]; // Create the group stack
    this.parent = this.svg;

    this.path = '';
    this.pen = { x: 0, y: 0 };
    this.lineWidth = 1.0;
    this.state = {
      scale: { x: 1, y: 1 },
      'font-family': 'Arial',
      'font-size': '8pt',
      'font-weight': 'normal'
    };

    this.attributes = {
      'stroke-width': 0.3,
      'fill': 'black',
      'stroke': 'black',
      'stroke-dasharray': 'none',
      'font-family': 'Arial',
      'font-size': '10pt',
      'font-weight': 'normal',
      'font-style': 'normal'
    };

    this.background_attributes = {
      'stroke-width': 0,
      'fill': 'white',
      'stroke': 'white',
      'stroke-dasharray': 'none',
      'font-family': 'Arial',
      'font-size': '10pt',
      'font-weight': 'normal',
      'font-style': 'normal'
    };

    this.shadow_attributes = {
      width: 0,
      color: 'black'
    };

    this.state_stack = [];

    // Test for Internet Explorer
    this.iePolyfill();
  }

  _createClass(SVGContext, [{
    key: 'create',
    value: function create(svgElementType) {
      return document.createElementNS(this.svgNS, svgElementType);
    }

    // Allow grouping elements in containers for interactivity.

  }, {
    key: 'openGroup',
    value: function openGroup(cls, id, attrs) {
      var group = this.create('g');
      this.groups.push(group);
      this.parent.appendChild(group);
      this.parent = group;
      if (cls) group.setAttribute('class', _vex.Vex.Prefix(cls));
      if (id) group.setAttribute('id', _vex.Vex.Prefix(id));

      if (attrs && attrs.pointerBBox) {
        group.setAttribute('pointer-events', 'bounding-box');
      }
      return group;
    }
  }, {
    key: 'closeGroup',
    value: function closeGroup() {
      this.groups.pop();
      this.parent = this.groups[this.groups.length - 1];
    }
  }, {
    key: 'add',
    value: function add(elem) {
      this.parent.appendChild(elem);
    }

    // Tests if the browser is Internet Explorer; if it is,
    // we do some tricks to improve text layout.  See the
    // note at ieMeasureTextFix() for details.

  }, {
    key: 'iePolyfill',
    value: function iePolyfill() {
      if (typeof navigator !== 'undefined') {
        this.ie = /MSIE 9/i.test(navigator.userAgent) || /MSIE 10/i.test(navigator.userAgent) || /rv:11\.0/i.test(navigator.userAgent) || /Trident/i.test(navigator.userAgent);
      }
    }

    // ### Styling & State Methods:

  }, {
    key: 'setFont',
    value: function setFont(family, size, weight) {
      // Unlike canvas, in SVG italic is handled by font-style,
      // not weight. So: we search the weight argument and
      // apply bold and italic to weight and style respectively.
      var bold = false;
      var italic = false;
      var style = 'normal';
      // Weight might also be a number (200, 400, etc...) so we
      // test its type to be sure we have access to String methods.
      if (typeof weight === 'string') {
        // look for "italic" in the weight:
        if (weight.indexOf('italic') !== -1) {
          weight = weight.replace(/italic/g, '');
          italic = true;
        }
        // look for "bold" in weight
        if (weight.indexOf('bold') !== -1) {
          weight = weight.replace(/bold/g, '');
          bold = true;
        }
        // remove any remaining spaces
        weight = weight.replace(/ /g, '');
      }
      weight = bold ? 'bold' : weight;
      weight = typeof weight === 'undefined' || weight === '' ? 'normal' : weight;

      style = italic ? 'italic' : style;

      var fontAttributes = {
        'font-family': family,
        'font-size': size + 'pt',
        'font-weight': weight,
        'font-style': style
      };

      // Store the font size so that if the browser is Internet
      // Explorer we can fix its calculations of text width.
      this.fontSize = Number(size);

      _vex.Vex.Merge(this.attributes, fontAttributes);
      _vex.Vex.Merge(this.state, fontAttributes);

      return this;
    }
  }, {
    key: 'setRawFont',
    value: function setRawFont(font) {
      font = font.trim();
      // Assumes size first, splits on space -- which is presently
      // how all existing modules are calling this.
      var fontArray = font.split(' ');

      this.attributes['font-family'] = fontArray[1];
      this.state['font-family'] = fontArray[1];

      this.attributes['font-size'] = fontArray[0];
      this.state['font-size'] = fontArray[0];

      // Saves fontSize for IE polyfill
      this.fontSize = Number(fontArray[0].match(/\d+/));
      return this;
    }
  }, {
    key: 'setFillStyle',
    value: function setFillStyle(style) {
      this.attributes.fill = style;
      return this;
    }
  }, {
    key: 'setBackgroundFillStyle',
    value: function setBackgroundFillStyle(style) {
      this.background_attributes.fill = style;
      this.background_attributes.stroke = style;
      return this;
    }
  }, {
    key: 'setStrokeStyle',
    value: function setStrokeStyle(style) {
      this.attributes.stroke = style;
      return this;
    }
  }, {
    key: 'setShadowColor',
    value: function setShadowColor(style) {
      this.shadow_attributes.color = style;
      return this;
    }
  }, {
    key: 'setShadowBlur',
    value: function setShadowBlur(blur) {
      this.shadow_attributes.width = blur;
      return this;
    }
  }, {
    key: 'setLineWidth',
    value: function setLineWidth(width) {
      this.attributes['stroke-width'] = width;
      this.lineWidth = width;
    }

    // @param array {lineDash} as [dashInt, spaceInt, dashInt, spaceInt, etc...]

  }, {
    key: 'setLineDash',
    value: function setLineDash(lineDash) {
      if (Object.prototype.toString.call(lineDash) === '[object Array]') {
        lineDash = lineDash.join(', ');
        this.attributes['stroke-dasharray'] = lineDash;
        return this;
      } else {
        throw new _vex.Vex.RERR('ArgumentError', 'lineDash must be an array of integers.');
      }
    }
  }, {
    key: 'setLineCap',
    value: function setLineCap(lineCap) {
      this.attributes['stroke-linecap'] = lineCap;
      return this;
    }

    // ### Sizing & Scaling Methods:

    // TODO (GCR): See note at scale() -- seperate our internal
    // conception of pixel-based width/height from the style.width
    // and style.height properties eventually to allow users to
    // apply responsive sizing attributes to the SVG.

  }, {
    key: 'resize',
    value: function resize(width, height) {
      this.width = width;
      this.height = height;
      this.element.style.width = width;
      var attributes = {
        width: width,
        height: height
      };
      this.applyAttributes(this.svg, attributes);
      return this;
    }
  }, {
    key: 'scale',
    value: function scale(x, y) {
      // uses viewBox to scale
      // TODO (GCR): we may at some point want to distinguish the
      // style.width / style.height properties that are applied to
      // the SVG object from our internal conception of the SVG
      // width/height.  This would allow us to create automatically
      // scaling SVG's that filled their containers, for instance.
      //
      // As this isn't implemented in Canvas or Raphael contexts,
      // I've left as is for now, but in using the viewBox to
      // handle internal scaling, am trying to make it possible
      // for us to eventually move in that direction.

      this.state.scale = { x: x, y: y };
      var visibleWidth = this.width / x;
      var visibleHeight = this.height / y;
      this.setViewBox(0, 0, visibleWidth, visibleHeight);

      return this;
    }
  }, {
    key: 'setViewBox',
    value: function setViewBox() {
      for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

      // Override for "x y w h" style:
      if (args.length === 1) {
        var viewBox = args[0];

        this.svg.setAttribute('viewBox', viewBox);
      } else {
        var xMin = args[0],
            yMin = args[1],
            width = args[2],
            height = args[3];

        var viewBoxString = xMin + ' ' + yMin + ' ' + width + ' ' + height;
        this.svg.setAttribute('viewBox', viewBoxString);
      }
    }

    // ### Drawing helper methods:

  }, {
    key: 'applyAttributes',
    value: function applyAttributes(element, attributes) {
      Object.keys(attributes).forEach(function (propertyName) {
        return element.setAttributeNS(null, propertyName, attributes[propertyName]);
      });

      return element;
    }

    // ### Shape & Path Methods:

  }, {
    key: 'clear',
    value: function clear() {
      // Clear the SVG by removing all inner children.

      // (This approach is usually slightly more efficient
      // than removing the old SVG & adding a new one to
      // the container element, since it does not cause the
      // container to resize twice.  Also, the resize
      // triggered by removing the entire SVG can trigger
      // a touchcancel event when the element resizes away
      // from a touch point.)

      while (this.svg.lastChild) {
        this.svg.removeChild(this.svg.lastChild);
      }

      // Replace the viewbox attribute we just removed:
      this.scale(this.state.scale.x, this.state.scale.y);
    }

    // ## Rectangles:

  }, {
    key: 'rect',
    value: function rect(x, y, width, height, attributes) {
      // Avoid invalid negative height attribs by
      // flipping the rectangle on its head:
      if (height < 0) {
        y += height;
        height *= -1;
      }

      // Create the rect & style it:
      var rectangle = this.create('rect');
      if (typeof attributes === 'undefined') {
        attributes = {
          fill: 'none',
          'stroke-width': this.lineWidth,
          stroke: 'black'
        };
      }

      _vex.Vex.Merge(attributes, {
        x: x,
        y: y,
        width: width,
        height: height
      });

      this.applyAttributes(rectangle, attributes);

      this.add(rectangle);
      return this;
    }
  }, {
    key: 'fillRect',
    value: function fillRect(x, y, width, height) {
      if (height < 0) {
        y += height;
        height *= -1;
      }

      this.rect(x, y, width, height, this.attributes);
      return this;
    }
  }, {
    key: 'clearRect',
    value: function clearRect(x, y, width, height) {
      // TODO(GCR): Improve implementation of this...
      // Currently it draws a box of the background color, rather
      // than creating alpha through lower z-levels.
      //
      // See the implementation of this in SVGKit:
      // http://sourceforge.net/projects/svgkit/
      // as a starting point.
      //
      // Adding a large number of transform paths (as we would
      // have to do) could be a real performance hit.  Since
      // tabNote seems to be the only module that makes use of this
      // it may be worth creating a seperate tabStave that would
      // draw lines around locations of tablature fingering.
      //

      this.rect(x, y, width, height, this.background_attributes);
      return this;
    }

    // ## Paths:

  }, {
    key: 'beginPath',
    value: function beginPath() {
      this.path = '';
      this.pen.x = 0;
      this.pen.y = 0;
      return this;
    }
  }, {
    key: 'moveTo',
    value: function moveTo(x, y) {
      this.path += 'M' + x + ' ' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }
  }, {
    key: 'lineTo',
    value: function lineTo(x, y) {
      this.path += 'L' + x + ' ' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }
  }, {
    key: 'bezierCurveTo',
    value: function bezierCurveTo(x1, y1, x2, y2, x, y) {
      this.path += 'C' + x1 + ' ' + y1 + ',' + x2 + ' ' + y2 + ',' + x + ' ' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }
  }, {
    key: 'quadraticCurveTo',
    value: function quadraticCurveTo(x1, y1, x, y) {
      this.path += 'Q' + x1 + ' ' + y1 + ',' + x + ' ' + y;
      this.pen.x = x;
      this.pen.y = y;
      return this;
    }

    // This is an attempt (hack) to simulate the HTML5 canvas
    // arc method.

  }, {
    key: 'arc',
    value: function arc(x, y, radius, startAngle, endAngle, antiClockwise) {
      function normalizeAngle(angle) {
        while (angle < 0) {
          angle += Math.PI * 2;
        }

        while (angle > Math.PI * 2) {
          angle -= Math.PI * 2;
        }
        return angle;
      }

      startAngle = normalizeAngle(startAngle);
      endAngle = normalizeAngle(endAngle);

      if (startAngle > endAngle) {
        var tmp = startAngle;
        startAngle = endAngle;
        endAngle = tmp;
        antiClockwise = !antiClockwise;
      }

      var delta = endAngle - startAngle;

      if (delta > Math.PI) {
        this.arcHelper(x, y, radius, startAngle, startAngle + delta / 2, antiClockwise);
        this.arcHelper(x, y, radius, startAngle + delta / 2, endAngle, antiClockwise);
      } else {
        this.arcHelper(x, y, radius, startAngle, endAngle, antiClockwise);
      }
      return this;
    }
  }, {
    key: 'arcHelper',
    value: function arcHelper(x, y, radius, startAngle, endAngle, antiClockwise) {
      var x1 = x + radius * Math.cos(startAngle);
      var y1 = y + radius * Math.sin(startAngle);

      var x2 = x + radius * Math.cos(endAngle);
      var y2 = y + radius * Math.sin(endAngle);

      var largeArcFlag = 0;
      var sweepFlag = 0;
      if (antiClockwise) {
        sweepFlag = 1;
        if (endAngle - startAngle < Math.PI) {
          largeArcFlag = 1;
        }
      } else if (endAngle - startAngle > Math.PI) {
        largeArcFlag = 1;
      }

      this.path += 'M' + x1 + ' ' + y1 + ' A' + radius + ' ' + radius + ' 0 ' + largeArcFlag + ' ' + sweepFlag + ' ' + x2 + ' ' + y2 + 'M' + this.pen.x + ' ' + this.pen.y;
    }
  }, {
    key: 'closePath',
    value: function closePath() {
      this.path += 'Z';

      return this;
    }

    // Adapted from the source for Raphael's Element.glow

  }, {
    key: 'glow',
    value: function glow() {
      // Calculate the width & paths of the glow:
      if (this.shadow_attributes.width > 0) {
        var sa = this.shadow_attributes;
        var num_paths = sa.width / 2;
        // Stroke at varying widths to create effect of gaussian blur:
        for (var i = 1; i <= num_paths; i++) {
          var attributes = {
            stroke: sa.color,
            'stroke-linejoin': 'round',
            'stroke-linecap': 'round',
            'stroke-width': +(sa.width * 0.4 / num_paths * i).toFixed(3),
            opacity: +((sa.opacity || 0.3) / num_paths).toFixed(3)
          };

          var path = this.create('path');
          attributes.d = this.path;
          this.applyAttributes(path, attributes);
          this.add(path);
        }
      }
      return this;
    }
  }, {
    key: 'fill',
    value: function fill(attributes) {
      // If our current path is set to glow, make it glow
      this.glow();

      var path = this.create('path');
      if (typeof attributes === 'undefined') {
        attributes = {};
        _vex.Vex.Merge(attributes, this.attributes);
        attributes.stroke = 'none';
      }

      attributes.d = this.path;

      this.applyAttributes(path, attributes);
      this.add(path);
      return this;
    }
  }, {
    key: 'stroke',
    value: function stroke() {
      // If our current path is set to glow, make it glow.
      this.glow();

      var path = this.create('path');
      var attributes = {};
      _vex.Vex.Merge(attributes, this.attributes);
      attributes.fill = 'none';
      attributes['stroke-width'] = this.lineWidth;
      attributes.d = this.path;

      this.applyAttributes(path, attributes);
      this.add(path);
      return this;
    }

    // ## Text Methods:

  }, {
    key: 'measureText',
    value: function measureText(text) {
      var txt = this.create('text');
      if (typeof txt.getBBox !== 'function') {
        return { x: 0, y: 0, width: 0, height: 0 };
      }

      txt.textContent = text;
      this.applyAttributes(txt, this.attributes);

      // Temporarily add it to the document for measurement.
      this.svg.appendChild(txt);

      var bbox = txt.getBBox();
      if (this.ie && text !== '' && this.attributes['font-style'] === 'italic') {
        bbox = this.ieMeasureTextFix(bbox, text);
      }

      this.svg.removeChild(txt);
      return bbox;
    }
  }, {
    key: 'ieMeasureTextFix',
    value: function ieMeasureTextFix(bbox) {
      // Internet Explorer over-pads text in italics,
      // resulting in giant width estimates for measureText.
      // To fix this, we use this formula, tested against
      // ie 11:
      // overestimate (in pixels) = FontSize(in pt) * 1.196 + 1.96
      // And then subtract the overestimate from calculated width.

      var fontSize = Number(this.fontSize);
      var m = 1.196;
      var b = 1.9598;
      var widthCorrection = m * fontSize + b;
      var width = bbox.width - widthCorrection;
      var height = bbox.height - 1.5;

      // Get non-protected copy:
      var box = {
        x: bbox.x,
        y: bbox.y,
        width: width,
        height: height
      };

      return box;
    }
  }, {
    key: 'fillText',
    value: function fillText(text, x, y) {
      var attributes = {};
      _vex.Vex.Merge(attributes, this.attributes);
      attributes.stroke = 'none';
      attributes.x = x;
      attributes.y = y;

      var txt = this.create('text');
      txt.textContent = text;
      this.applyAttributes(txt, attributes);
      this.add(txt);
    }
  }, {
    key: 'save',
    value: function save() {
      // TODO(mmuthanna): State needs to be deep-copied.
      this.state_stack.push({
        state: {
          'font-family': this.state['font-family'],
          'font-weight': this.state['font-weight'],
          'font-style': this.state['font-style'],
          'font-size': this.state['font-size']
        },
        attributes: {
          'font-family': this.attributes['font-family'],
          'font-weight': this.attributes['font-weight'],
          'font-style': this.attributes['font-style'],
          'font-size': this.attributes['font-size'],
          fill: this.attributes.fill,
          stroke: this.attributes.stroke,
          'stroke-width': this.attributes['stroke-width'],
          'stroke-dasharray': this.attributes['stroke-dasharray']
        },
        shadow_attributes: {
          width: this.shadow_attributes.width,
          color: this.shadow_attributes.color
        }
      });
      return this;
    }
  }, {
    key: 'restore',
    value: function restore() {
      // TODO(0xfe): State needs to be deep-restored.
      var state = this.state_stack.pop();
      this.state['font-family'] = state.state['font-family'];
      this.state['font-weight'] = state.state['font-weight'];
      this.state['font-style'] = state.state['font-style'];
      this.state['font-size'] = state.state['font-size'];

      this.attributes['font-family'] = state.attributes['font-family'];
      this.attributes['font-weight'] = state.attributes['font-weight'];
      this.attributes['font-style'] = state.attributes['font-style'];
      this.attributes['font-size'] = state.attributes['font-size'];

      this.attributes.fill = state.attributes.fill;
      this.attributes.stroke = state.attributes.stroke;
      this.attributes['stroke-width'] = state.attributes['stroke-width'];
      this.attributes['stroke-dasharray'] = state.attributes['stroke-dasharray'];

      this.shadow_attributes.width = state.shadow_attributes.width;
      this.shadow_attributes.color = state.shadow_attributes.color;
      return this;
    }
  }]);

  return SVGContext;
}();

/***/ }),
/* 69 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveSection = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _stavemodifier = __webpack_require__(7);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Larry Kuhns 2011

var StaveSection = exports.StaveSection = function (_StaveModifier) {
  _inherits(StaveSection, _StaveModifier);

  _createClass(StaveSection, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'stavesection';
    }
  }]);

  function StaveSection(section, x, shift_y) {
    _classCallCheck(this, StaveSection);

    var _this = _possibleConstructorReturn(this, (StaveSection.__proto__ || Object.getPrototypeOf(StaveSection)).call(this));

    _this.setAttribute('type', 'StaveSection');

    _this.setWidth(16);
    _this.section = section;
    _this.x = x;
    _this.shift_x = 0;
    _this.shift_y = shift_y;
    _this.font = {
      family: 'sans-serif',
      size: 12,
      weight: 'bold'
    };
    return _this;
  }

  _createClass(StaveSection, [{
    key: 'getCategory',
    value: function getCategory() {
      return StaveSection.CATEGORY;
    }
  }, {
    key: 'setStaveSection',
    value: function setStaveSection(section) {
      this.section = section;return this;
    }
  }, {
    key: 'setShiftX',
    value: function setShiftX(x) {
      this.shift_x = x;return this;
    }
  }, {
    key: 'setShiftY',
    value: function setShiftY(y) {
      this.shift_y = y;return this;
    }
  }, {
    key: 'draw',
    value: function draw(stave, shift_x) {
      var ctx = stave.checkContext();
      this.setRendered();

      ctx.save();
      ctx.lineWidth = 2;
      ctx.setFont(this.font.family, this.font.size, this.font.weight);
      var text_width = ctx.measureText('' + this.section).width;
      var width = text_width + 6; // add left & right padding
      if (width < 18) width = 18;
      var height = 20;
      //  Seems to be a good default y
      var y = stave.getYForTopText(3) + this.shift_y;
      var x = this.x + shift_x;
      ctx.beginPath();
      ctx.lineWidth = 2;
      ctx.rect(x, y, width, height);
      ctx.stroke();
      x += (width - text_width) / 2;
      ctx.fillText('' + this.section, x, y + 16);
      ctx.restore();
      return this;
    }
  }]);

  return StaveSection;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 70 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveTempo = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _tables = __webpack_require__(1);

var _modifier = __webpack_require__(4);

var _stavemodifier = __webpack_require__(7);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Radosaw Eichler 2012

var StaveTempo = exports.StaveTempo = function (_StaveModifier) {
  _inherits(StaveTempo, _StaveModifier);

  _createClass(StaveTempo, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'stavetempo';
    }
  }]);

  function StaveTempo(tempo, x, shift_y) {
    _classCallCheck(this, StaveTempo);

    var _this = _possibleConstructorReturn(this, (StaveTempo.__proto__ || Object.getPrototypeOf(StaveTempo)).call(this));

    _this.setAttribute('type', 'StaveTempo');

    _this.tempo = tempo;
    _this.position = _modifier.Modifier.Position.ABOVE;
    _this.x = x;
    _this.shift_x = 10;
    _this.shift_y = shift_y;
    _this.font = {
      family: 'times',
      size: 14,
      weight: 'bold'
    };
    _this.render_options = {
      glyph_font_scale: 30 // font size for note
    };
    return _this;
  }

  _createClass(StaveTempo, [{
    key: 'getCategory',
    value: function getCategory() {
      return StaveTempo.CATEGORY;
    }
  }, {
    key: 'setTempo',
    value: function setTempo(tempo) {
      this.tempo = tempo;return this;
    }
  }, {
    key: 'setShiftX',
    value: function setShiftX(x) {
      this.shift_x = x;return this;
    }
  }, {
    key: 'setShiftY',
    value: function setShiftY(y) {
      this.shift_y = y;return this;
    }
  }, {
    key: 'draw',
    value: function draw(stave, shift_x) {
      var ctx = stave.checkContext();
      this.setRendered();

      var options = this.render_options;
      // FIXME: What does the '38' mean? Why 38? Is that supposed to
      // be the default font size for standard notation?
      var scale = options.glyph_font_scale / 38;
      var name = this.tempo.name;
      var duration = this.tempo.duration;
      var dots = this.tempo.dots;
      var bpm = this.tempo.bpm;
      var font = this.font;
      var x = this.x + this.shift_x + shift_x;
      var y = stave.getYForTopText(1) + this.shift_y;

      ctx.save();

      if (name) {
        ctx.setFont(font.family, font.size, font.weight);
        ctx.fillText(name, x, y);
        x += ctx.measureText(name).width;
      }

      if (duration && bpm) {
        ctx.setFont(font.family, font.size, 'normal');

        if (name) {
          x += ctx.measureText(' ').width;
          ctx.fillText('(', x, y);
          x += ctx.measureText('(').width;
        }

        var code = _tables.Flow.durationToGlyph(duration);

        x += 3 * scale;
        _glyph.Glyph.renderGlyph(ctx, x, y, options.glyph_font_scale, code.code_head);
        x += code.getWidth() * scale;

        // Draw stem and flags
        if (code.stem) {
          var stem_height = 30;

          if (code.beam_count) stem_height += 3 * (code.beam_count - 1);

          stem_height *= scale;

          var y_top = y - stem_height;
          ctx.fillRect(x - scale, y_top, scale, stem_height);

          if (code.flag) {
            _glyph.Glyph.renderGlyph(ctx, x, y_top, options.glyph_font_scale, code.code_flag_upstem);

            if (!dots) x += 6 * scale;
          }
        }

        // Draw dot
        for (var i = 0; i < dots; i++) {
          x += 6 * scale;
          ctx.beginPath();
          ctx.arc(x, y + 2 * scale, 2 * scale, 0, Math.PI * 2, false);
          ctx.fill();
        }

        ctx.fillText(' = ' + bpm + (name ? ')' : ''), x + 3 * scale, y);
      }

      ctx.restore();
      return this;
    }
  }]);

  return StaveTempo;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 71 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveText = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _stavemodifier = __webpack_require__(7);

var _textnote = __webpack_require__(35);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author Taehoon Moon 2014

var StaveText = exports.StaveText = function (_StaveModifier) {
  _inherits(StaveText, _StaveModifier);

  _createClass(StaveText, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'stavetext';
    }
  }]);

  function StaveText(text, position, options) {
    _classCallCheck(this, StaveText);

    var _this = _possibleConstructorReturn(this, (StaveText.__proto__ || Object.getPrototypeOf(StaveText)).call(this));

    _this.setAttribute('type', 'StaveText');

    _this.setWidth(16);
    _this.text = text;
    _this.position = position;
    _this.options = {
      shift_x: 0,
      shift_y: 0,
      justification: _textnote.TextNote.Justification.CENTER
    };
    _vex.Vex.Merge(_this.options, options);

    _this.font = {
      family: 'times',
      size: 16,
      weight: 'normal'
    };
    return _this;
  }

  _createClass(StaveText, [{
    key: 'getCategory',
    value: function getCategory() {
      return StaveText.CATEGORY;
    }
  }, {
    key: 'setStaveText',
    value: function setStaveText(text) {
      this.text = text;return this;
    }
  }, {
    key: 'setShiftX',
    value: function setShiftX(x) {
      this.shift_x = x;return this;
    }
  }, {
    key: 'setShiftY',
    value: function setShiftY(y) {
      this.shift_y = y;return this;
    }
  }, {
    key: 'setFont',
    value: function setFont(font) {
      _vex.Vex.Merge(this.font, font);
    }
  }, {
    key: 'setText',
    value: function setText(text) {
      this.text = text;
    }
  }, {
    key: 'draw',
    value: function draw(stave) {
      var ctx = stave.checkContext();
      this.setRendered();

      ctx.save();
      ctx.lineWidth = 2;
      ctx.setFont(this.font.family, this.font.size, this.font.weight);
      var text_width = ctx.measureText('' + this.text).width;

      var x = void 0;
      var y = void 0;
      var Position = _stavemodifier.StaveModifier.Position;
      var Justification = _textnote.TextNote.Justification;
      switch (this.position) {
        case Position.LEFT:
        case Position.RIGHT:
          y = (stave.getYForLine(0) + stave.getBottomLineY()) / 2 + this.options.shift_y;
          if (this.position === Position.LEFT) {
            x = stave.getX() - text_width - 24 + this.options.shift_x;
          } else {
            x = stave.getX() + stave.getWidth() + 24 + this.options.shift_x;
          }
          break;
        case Position.ABOVE:
        case Position.BELOW:
          x = stave.getX() + this.options.shift_x;
          if (this.options.justification === Justification.CENTER) {
            x += stave.getWidth() / 2 - text_width / 2;
          } else if (this.options.justification === Justification.RIGHT) {
            x += stave.getWidth() - text_width;
          }

          if (this.position === Position.ABOVE) {
            y = stave.getYForTopText(2) + this.options.shift_y;
          } else {
            y = stave.getYForBottomText(2) + this.options.shift_y;
          }
          break;
        default:
          throw new _vex.Vex.RERR('InvalidPosition', 'Value Must be in Modifier.Position.');
      }

      ctx.fillText('' + this.text, x, y + 4);
      ctx.restore();
      return this;
    }
  }]);

  return StaveText;
}(_stavemodifier.StaveModifier);

/***/ }),
/* 72 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TabSlide = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tabtie = __webpack_require__(28);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class implements varies types of ties between contiguous notes. The
// ties include: regular ties, hammer ons, pull offs, and slides.

var TabSlide = exports.TabSlide = function (_TabTie) {
  _inherits(TabSlide, _TabTie);

  _createClass(TabSlide, null, [{
    key: 'createSlideUp',
    value: function createSlideUp(notes) {
      return new TabSlide(notes, TabSlide.SLIDE_UP);
    }
  }, {
    key: 'createSlideDown',
    value: function createSlideDown(notes) {
      return new TabSlide(notes, TabSlide.SLIDE_DOWN);
    }
  }, {
    key: 'SLIDE_UP',
    get: function get() {
      return 1;
    }
  }, {
    key: 'SLIDE_DOWN',
    get: function get() {
      return -1;
    }
  }]);

  function TabSlide(notes, direction) {
    _classCallCheck(this, TabSlide);

    var _this = _possibleConstructorReturn(this, (TabSlide.__proto__ || Object.getPrototypeOf(TabSlide)).call(this, notes, 'sl.'));
    /**
     * Notes is a struct that has:
     *
     *  {
     *    first_note: Note,
     *    last_note: Note,
     *    first_indices: [n1, n2, n3],
     *    last_indices: [n1, n2, n3]
     *  }
     *
     **/


    _this.setAttribute('type', 'TabSlide');

    if (!direction) {
      var first_fret = notes.first_note.getPositions()[0].fret;
      var last_fret = notes.last_note.getPositions()[0].fret;

      direction = parseInt(first_fret, 10) > parseInt(last_fret, 10) ? TabSlide.SLIDE_DOWN : TabSlide.SLIDE_UP;
    }

    _this.slide_direction = direction;
    _this.render_options.cp1 = 11;
    _this.render_options.cp2 = 14;
    _this.render_options.y_shift = 0.5;

    _this.setFont({ font: 'Times', size: 10, style: 'bold italic' });
    _this.setNotes(notes);
    return _this;
  }

  _createClass(TabSlide, [{
    key: 'renderTie',
    value: function renderTie(params) {
      if (params.first_ys.length === 0 || params.last_ys.length === 0) {
        throw new _vex.Vex.RERR('BadArguments', 'No Y-values to render');
      }

      var ctx = this.context;
      var first_x_px = params.first_x_px;
      var first_ys = params.first_ys;
      var last_x_px = params.last_x_px;

      var direction = this.slide_direction;
      if (direction !== TabSlide.SLIDE_UP && direction !== TabSlide.SLIDE_DOWN) {
        throw new _vex.Vex.RERR('BadSlide', 'Invalid slide direction');
      }

      for (var i = 0; i < this.first_indices.length; ++i) {
        var slide_y = first_ys[this.first_indices[i]] + this.render_options.y_shift;

        if (isNaN(slide_y)) {
          throw new _vex.Vex.RERR('BadArguments', 'Bad indices for slide rendering.');
        }

        ctx.beginPath();
        ctx.moveTo(first_x_px, slide_y + 3 * direction);
        ctx.lineTo(last_x_px, slide_y - 3 * direction);
        ctx.closePath();
        ctx.stroke();
      }

      this.setRendered();
    }
  }]);

  return TabSlide;
}(_tabtie.TabTie);

/***/ }),
/* 73 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.GraceTabNote = undefined;

var _get = function get(object, property, receiver) { if (object === null) object = Function.prototype; var desc = Object.getOwnPropertyDescriptor(object, property); if (desc === undefined) { var parent = Object.getPrototypeOf(object); if (parent === null) { return undefined; } else { return get(parent, property, receiver); } } else if ("value" in desc) { return desc.value; } else { var getter = desc.get; if (getter === undefined) { return undefined; } return getter.call(receiver); } };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _tabnote = __webpack_require__(38);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// @author Balazs Forian-Szabo
//
// ## Description
//
// A basic implementation of grace notes
// to be rendered on a tab stave.
//
// See `tests/gracetabnote_tests.js` for usage examples.

var GraceTabNote = exports.GraceTabNote = function (_TabNote) {
  _inherits(GraceTabNote, _TabNote);

  _createClass(GraceTabNote, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'gracetabnotes';
    }
  }]);

  function GraceTabNote(note_struct) {
    _classCallCheck(this, GraceTabNote);

    var _this = _possibleConstructorReturn(this, (GraceTabNote.__proto__ || Object.getPrototypeOf(GraceTabNote)).call(this, note_struct, false));

    _this.setAttribute('type', 'GraceTabNote');

    _vex.Vex.Merge(_this.render_options, {
      // vertical shift from stave line
      y_shift: 0.3,
      // grace glyph scale
      scale: 0.6,
      // grace tablature font
      font: '7.5pt Arial'
    });

    _this.updateWidth();
    return _this;
  }

  _createClass(GraceTabNote, [{
    key: 'getCategory',
    value: function getCategory() {
      return GraceTabNote.CATEGORY;
    }
  }, {
    key: 'draw',
    value: function draw() {
      _get(GraceTabNote.prototype.__proto__ || Object.getPrototypeOf(GraceTabNote.prototype), 'draw', this).call(this);
      this.setRendered();
    }
  }]);

  return GraceTabNote;
}(_tabnote.TabNote);

/***/ }),
/* 74 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Tuning = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class implements varies types of tunings for tablature.

var _vex = __webpack_require__(0);

var _tables = __webpack_require__(1);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Tuning = exports.Tuning = function () {
  _createClass(Tuning, null, [{
    key: 'names',
    get: function get() {
      return {
        'standard': 'E/5,B/4,G/4,D/4,A/3,E/3',
        'dagdad': 'D/5,A/4,G/4,D/4,A/3,D/3',
        'dropd': 'E/5,B/4,G/4,D/4,A/3,D/3',
        'eb': 'Eb/5,Bb/4,Gb/4,Db/4,Ab/3,Db/3',
        'standardBanjo': 'D/5,B/4,G/4,D/4,G/5'
      };
    }
  }]);

  function Tuning() {
    var tuningString = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 'E/5,B/4,G/4,D/4,A/3,E/3,B/2,E/2';

    _classCallCheck(this, Tuning);

    // Default to standard tuning.
    this.setTuning(tuningString);
  }

  _createClass(Tuning, [{
    key: 'noteToInteger',
    value: function noteToInteger(noteString) {
      return _tables.Flow.keyProperties(noteString).int_value;
    }
  }, {
    key: 'setTuning',
    value: function setTuning(noteString) {
      if (Tuning.names[noteString]) {
        noteString = Tuning.names[noteString];
      }

      this.tuningString = noteString;
      this.tuningValues = [];
      this.numStrings = 0;

      var keys = noteString.split(/\s*,\s*/);
      if (keys.length === 0) {
        throw new _vex.Vex.RERR('BadArguments', 'Invalid tuning string: ' + noteString);
      }

      this.numStrings = keys.length;
      for (var i = 0; i < this.numStrings; ++i) {
        this.tuningValues[i] = this.noteToInteger(keys[i]);
      }
    }
  }, {
    key: 'getValueForString',
    value: function getValueForString(stringNum) {
      var s = parseInt(stringNum, 10);
      if (s < 1 || s > this.numStrings) {
        throw new _vex.Vex.RERR('BadArguments', 'String number must be between 1 and ' + this.numStrings + ':' + stringNum);
      }

      return this.tuningValues[s - 1];
    }
  }, {
    key: 'getValueForFret',
    value: function getValueForFret(fretNum, stringNum) {
      var stringValue = this.getValueForString(stringNum);
      var f = parseInt(fretNum, 10);

      if (f < 0) {
        throw new _vex.Vex.RERR('BadArguments', 'Fret number must be 0 or higher: ' + fretNum);
      }

      return stringValue + f;
    }
  }, {
    key: 'getNoteForFret',
    value: function getNoteForFret(fretNum, stringNum) {
      var noteValue = this.getValueForFret(fretNum, stringNum);

      var octave = Math.floor(noteValue / 12);
      var value = noteValue % 12;

      return _tables.Flow.integerToNote(value) + '/' + octave;
    }
  }]);

  return Tuning;
}();

/***/ }),
/* 75 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.KeyManager = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }(); // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This class implements diatonic key management.

var _vex = __webpack_require__(0);

var _music = __webpack_require__(25);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var KeyManager = exports.KeyManager = function () {
  function KeyManager(key) {
    _classCallCheck(this, KeyManager);

    this.music = new _music.Music();
    this.setKey(key);
  }

  _createClass(KeyManager, [{
    key: 'setKey',
    value: function setKey(key) {
      this.key = key;
      this.reset();
      return this;
    }
  }, {
    key: 'getKey',
    value: function getKey() {
      return this.key;
    }
  }, {
    key: 'reset',
    value: function reset() {
      this.keyParts = this.music.getKeyParts(this.key);

      this.keyString = this.keyParts.root;
      if (this.keyParts.accidental) this.keyString += this.keyParts.accidental;

      var is_supported_type = _music.Music.scaleTypes[this.keyParts.type];
      if (!is_supported_type) {
        throw new _vex.Vex.RERR('BadArguments', 'Unsupported key type: ' + this.key);
      }

      this.scale = this.music.getScaleTones(this.music.getNoteValue(this.keyString), _music.Music.scaleTypes[this.keyParts.type]);

      this.scaleMap = {};
      this.scaleMapByValue = {};
      this.originalScaleMapByValue = {};

      var noteLocation = _music.Music.root_indices[this.keyParts.root];

      for (var i = 0; i < _music.Music.roots.length; ++i) {
        var index = (noteLocation + i) % _music.Music.roots.length;
        var rootName = _music.Music.roots[index];

        var noteName = this.music.getRelativeNoteName(rootName, this.scale[i]);
        this.scaleMap[rootName] = noteName;
        this.scaleMapByValue[this.scale[i]] = noteName;
        this.originalScaleMapByValue[this.scale[i]] = noteName;
      }

      return this;
    }
  }, {
    key: 'getAccidental',
    value: function getAccidental(key) {
      var root = this.music.getKeyParts(key).root;
      var parts = this.music.getNoteParts(this.scaleMap[root]);

      return {
        note: this.scaleMap[root],
        accidental: parts.accidental
      };
    }
  }, {
    key: 'selectNote',
    value: function selectNote(note) {
      note = note.toLowerCase();
      var parts = this.music.getNoteParts(note);

      // First look for matching note in our altered scale
      var scaleNote = this.scaleMap[parts.root];
      var modparts = this.music.getNoteParts(scaleNote);

      if (scaleNote === note) {
        return {
          'note': scaleNote,
          'accidental': parts.accidental,
          'change': false
        };
      }

      // Then search for a note of equivalent value in our altered scale
      var valueNote = this.scaleMapByValue[this.music.getNoteValue(note)];
      if (valueNote != null) {
        return {
          'note': valueNote,
          'accidental': this.music.getNoteParts(valueNote).accidental,
          'change': false
        };
      }

      // Then search for a note of equivalent value in the original scale
      var originalValueNote = this.originalScaleMapByValue[this.music.getNoteValue(note)];
      if (originalValueNote != null) {
        this.scaleMap[modparts.root] = originalValueNote;
        delete this.scaleMapByValue[this.music.getNoteValue(scaleNote)];
        this.scaleMapByValue[this.music.getNoteValue(note)] = originalValueNote;
        return {
          'note': originalValueNote,
          'accidental': this.music.getNoteParts(originalValueNote).accidental,
          'change': true
        };
      }

      // Then try to unmodify a currently modified note.
      if (modparts.root === note) {
        delete this.scaleMapByValue[this.music.getNoteValue(this.scaleMap[parts.root])];
        this.scaleMapByValue[this.music.getNoteValue(modparts.root)] = modparts.root;
        this.scaleMap[modparts.root] = modparts.root;
        return {
          'note': modparts.root,
          'accidental': null,
          'change': true
        };
      }

      // Last resort -- shitshoot
      delete this.scaleMapByValue[this.music.getNoteValue(this.scaleMap[parts.root])];
      this.scaleMapByValue[this.music.getNoteValue(note)] = note;

      delete this.scaleMap[modparts.root];
      this.scaleMap[modparts.root] = note;

      return {
        note: note,
        'accidental': parts.accidental,
        'change': true
      };
    }
  }]);

  return KeyManager;
}();

/***/ }),
/* 76 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.StaveHairpin = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _element = __webpack_require__(3);

var _modifier = __webpack_require__(4);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
// This class by Raffaele Viglianti, 2012 http://itisnotsound.wordpress.com/
//
// This class implements hairpins between notes.
// Hairpins can be either Crescendo or Descrescendo.

var StaveHairpin = exports.StaveHairpin = function (_Element) {
  _inherits(StaveHairpin, _Element);

  _createClass(StaveHairpin, null, [{
    key: 'FormatByTicksAndDraw',


    /* Helper function to convert ticks into pixels.
     * Requires a Formatter with voices joined and formatted (to
     * get pixels per tick)
     *
     * options is struct that has:
     *
     *  {
     *   height: px,
     *   y_shift: px, //vertical offset
     *   left_shift_ticks: 0, //left horizontal offset expressed in ticks
     *   right_shift_ticks: 0 // right horizontal offset expressed in ticks
     *  }
     *
     **/
    value: function FormatByTicksAndDraw(ctx, formatter, notes, type, position, options) {
      var ppt = formatter.pixelsPerTick;

      if (ppt == null) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'A valid Formatter must be provide to draw offsets by ticks.');
      }

      var l_shift_px = ppt * options.left_shift_ticks;
      var r_shift_px = ppt * options.right_shift_ticks;

      var hairpin_options = {
        height: options.height,
        y_shift: options.y_shift,
        left_shift_px: l_shift_px,
        right_shift_px: r_shift_px };

      new StaveHairpin({
        first_note: notes.first_note,
        last_note: notes.last_note
      }, type).setContext(ctx).setRenderOptions(hairpin_options).setPosition(position).draw();
    }

    /**
     * Create a new hairpin from the specified notes.
     *
     * @constructor
     * @param {!Object} notes The notes to tie up.
     * @param {!Object} type The type of hairpin
     */

  }, {
    key: 'type',
    get: function get() {
      return {
        CRESC: 1,
        DECRESC: 2
      };
    }
  }]);

  function StaveHairpin(notes, type) {
    _classCallCheck(this, StaveHairpin);

    var _this = _possibleConstructorReturn(this, (StaveHairpin.__proto__ || Object.getPrototypeOf(StaveHairpin)).call(this));
    /**
     * Notes is a struct that has:
     *
     *  {
     *    first_note: Note,
     *    last_note: Note,
     *  }
     *
     **/


    _this.setAttribute('type', 'StaveHairpin');
    _this.notes = notes;
    _this.hairpin = type;
    _this.position = _modifier.Modifier.Position.BELOW;

    _this.render_options = {
      height: 10,
      y_shift: 0, // vertical offset
      left_shift_px: 0, // left horizontal offset
      right_shift_px: 0 // right horizontal offset
    };

    _this.setNotes(notes);
    return _this;
  }

  _createClass(StaveHairpin, [{
    key: 'setPosition',
    value: function setPosition(position) {
      if (position === _modifier.Modifier.Position.ABOVE || position === _modifier.Modifier.Position.BELOW) {
        this.position = position;
      }
      return this;
    }
  }, {
    key: 'setRenderOptions',
    value: function setRenderOptions(options) {
      if (options.height != null && options.y_shift != null && options.left_shift_px != null && options.right_shift_px != null) {
        this.render_options = options;
      }
      return this;
    }

    /**
     * Set the notes to attach this hairpin to.
     *
     * @param {!Object} notes The start and end notes.
     */

  }, {
    key: 'setNotes',
    value: function setNotes(notes) {
      if (!notes.first_note && !notes.last_note) {
        throw new _vex.Vex.RuntimeError('BadArguments', 'Hairpin needs to have either first_note or last_note set.');
      }

      // Success. Lets grab 'em notes.
      this.first_note = notes.first_note;
      this.last_note = notes.last_note;
      return this;
    }
  }, {
    key: 'renderHairpin',
    value: function renderHairpin(params) {
      var ctx = this.checkContext();
      var dis = this.render_options.y_shift + 20;
      var y_shift = params.first_y;

      if (this.position === _modifier.Modifier.Position.ABOVE) {
        dis = -dis + 30;
        y_shift = params.first_y - params.staff_height;
      }

      var l_shift = this.render_options.left_shift_px;
      var r_shift = this.render_options.right_shift_px;

      ctx.beginPath();

      switch (this.hairpin) {
        case StaveHairpin.type.CRESC:
          ctx.moveTo(params.last_x + r_shift, y_shift + dis);
          ctx.lineTo(params.first_x + l_shift, y_shift + this.render_options.height / 2 + dis);
          ctx.lineTo(params.last_x + r_shift, y_shift + this.render_options.height + dis);
          break;
        case StaveHairpin.type.DECRESC:
          ctx.moveTo(params.first_x + l_shift, y_shift + dis);
          ctx.lineTo(params.last_x + r_shift, y_shift + this.render_options.height / 2 + dis);
          ctx.lineTo(params.first_x + l_shift, y_shift + this.render_options.height + dis);
          break;
        default:
          // Default is NONE, so nothing to draw
          break;
      }

      ctx.stroke();
      ctx.closePath();
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      var firstNote = this.first_note;
      var lastNote = this.last_note;

      var start = firstNote.getModifierStartXY(this.position, 0);
      var end = lastNote.getModifierStartXY(this.position, 0);

      this.renderHairpin({
        first_x: start.x,
        last_x: end.x,
        first_y: firstNote.getStave().y + firstNote.getStave().height,
        last_y: lastNote.getStave().y + lastNote.getStave().height,
        staff_height: firstNote.getStave().height
      });
      return true;
    }
  }]);

  return StaveHairpin;
}(_element.Element);

/***/ }),
/* 77 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Tremolo = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _modifier = __webpack_require__(4);

var _glyph = __webpack_require__(2);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
// Author: Mike Corrigan <corrigan@gmail.com>
//
// This class implements tremolo notation.

var Tremolo = exports.Tremolo = function (_Modifier) {
  _inherits(Tremolo, _Modifier);

  _createClass(Tremolo, null, [{
    key: 'CATEGORY',
    get: function get() {
      return 'tremolo';
    }
  }]);

  function Tremolo(num) {
    _classCallCheck(this, Tremolo);

    var _this = _possibleConstructorReturn(this, (Tremolo.__proto__ || Object.getPrototypeOf(Tremolo)).call(this));

    _this.setAttribute('type', 'Tremolo');

    _this.num = num;
    _this.note = null;
    _this.index = null;
    _this.position = _modifier.Modifier.Position.CENTER;
    _this.code = 'v74';
    _this.shift_right = -2;
    _this.y_spacing = 4;

    _this.render_options = {
      font_scale: 35,
      stroke_px: 3,
      stroke_spacing: 10
    };

    _this.font = {
      family: 'Arial',
      size: 16,
      weight: ''
    };
    return _this;
  }

  _createClass(Tremolo, [{
    key: 'getCategory',
    value: function getCategory() {
      return Tremolo.CATEGORY;
    }
  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();

      if (!(this.note && this.index != null)) {
        throw new _vex.Vex.RERR('NoAttachedNote', "Can't draw Tremolo without a note and index.");
      }

      this.setRendered();
      var start = this.note.getModifierStartXY(this.position, this.index);
      var x = start.x;
      var y = start.y;

      x += this.shift_right;
      for (var i = 0; i < this.num; ++i) {
        _glyph.Glyph.renderGlyph(this.context, x, y, this.render_options.font_scale, this.code);
        y += this.y_spacing;
      }
    }
  }]);

  return Tremolo;
}(_modifier.Modifier);

/***/ }),
/* 78 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Crescendo = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _vex = __webpack_require__(0);

var _note = __webpack_require__(6);

var _tickcontext = __webpack_require__(13);

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; } // [VexFlow](http://vexflow.com) - Copyright (c) Mohit Muthanna 2010.
//
// ## Description
//
// This file implements the `Crescendo` object which draws crescendos and
// decrescendo dynamics markings. A `Crescendo` is initialized with a
// duration and formatted as part of a `Voice` like any other `Note`
// type in VexFlow. This object would most likely be formatted in a Voice
// with `TextNotes` - which are used to represent other dynamics markings.

// To enable logging for this class. Set `Vex.Flow.Crescendo.DEBUG` to `true`.
function L() {
  for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
    args[_key] = arguments[_key];
  }

  if (Crescendo.DEBUG) _vex.Vex.L('Vex.Flow.Crescendo', args);
}

// Private helper to draw the hairpin
function renderHairpin(ctx, params) {
  var begin_x = params.begin_x;
  var end_x = params.end_x;
  var y = params.y;
  var half_height = params.height / 2;

  ctx.beginPath();

  if (params.reverse) {
    ctx.moveTo(begin_x, y - half_height);
    ctx.lineTo(end_x, y);
    ctx.lineTo(begin_x, y + half_height);
  } else {
    ctx.moveTo(end_x, y - half_height);
    ctx.lineTo(begin_x, y);
    ctx.lineTo(end_x, y + half_height);
  }

  ctx.stroke();
  ctx.closePath();
}

var Crescendo = exports.Crescendo = function (_Note) {
  _inherits(Crescendo, _Note);

  // Initialize the crescendo's properties
  function Crescendo(note_struct) {
    _classCallCheck(this, Crescendo);

    var _this = _possibleConstructorReturn(this, (Crescendo.__proto__ || Object.getPrototypeOf(Crescendo)).call(this, note_struct));

    _this.setAttribute('type', 'Crescendo');

    // Whether the object is a decrescendo
    _this.decrescendo = false;

    // The staff line to be placed on
    _this.line = note_struct.line || 0;

    // The height at the open end of the cresc/decresc
    _this.height = 15;

    _vex.Vex.Merge(_this.render_options, {
      // Extensions to the length of the crescendo on either side
      extend_left: 0,
      extend_right: 0,
      // Vertical shift
      y_shift: 0
    });
    return _this;
  }

  // Set the line to center the element on


  _createClass(Crescendo, [{
    key: 'setLine',
    value: function setLine(line) {
      this.line = line;return this;
    }

    // Set the full height at the open end

  }, {
    key: 'setHeight',
    value: function setHeight(height) {
      this.height = height;return this;
    }

    // Set whether the sign should be a descresendo by passing a bool
    // to `decresc`

  }, {
    key: 'setDecrescendo',
    value: function setDecrescendo(decresc) {
      this.decrescendo = decresc;
      return this;
    }

    // Preformat the note

  }, {
    key: 'preFormat',
    value: function preFormat() {
      this.preFormatted = true;return this;
    }

    // Render the Crescendo object onto the canvas

  }, {
    key: 'draw',
    value: function draw() {
      this.checkContext();
      this.setRendered();

      var tick_context = this.getTickContext();
      var next_context = _tickcontext.TickContext.getNextContext(tick_context);

      var begin_x = this.getAbsoluteX();
      var end_x = next_context ? next_context.getX() : this.stave.x + this.stave.width;
      var y = this.stave.getYForLine(this.line + -3) + 1;

      L('Drawing ', this.decrescendo ? 'decrescendo ' : 'crescendo ', this.height, 'x', begin_x - end_x);

      renderHairpin(this.context, {
        begin_x: begin_x - this.render_options.extend_left,
        end_x: end_x + this.render_options.extend_right,
        y: y + this.render_options.y_shift,
        height: this.height,
        reverse: this.decrescendo
      });
    }
  }]);

  return Crescendo;
}(_note.Note);

/***/ })
/******/ ]);
});
//# sourceMappingURL=vexflow-debug.js.map