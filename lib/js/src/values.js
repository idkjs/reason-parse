// Generated by BUCKLESCRIPT VERSION 2.2.2, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");

function join(joiner, list) {
  return $$Array.of_list(list).join(joiner);
}

function stringOfString(param) {
  return param[1];
}

function stringOfInteger(param) {
  return Pervasives.string_of_int(param[1]);
}

function stringOfFloat(param) {
  return Pervasives.string_of_float(param[1]);
}

function stringOfValue(value) {
  var variant = value[0];
  if (variant >= 365180284) {
    if (variant >= 848054398) {
      return stringOfList(value);
    } else {
      return Pervasives.string_of_float(value[1]);
    }
  } else if (variant >= -462625378) {
    return Pervasives.string_of_int(value[1]);
  } else {
    return value[1];
  }
}

function stringOfList(param) {
  var list = List.map(stringOfValue, param[1]);
  return $$Array.of_list(list).join(", ");
}

function stringOfResult(result) {
  if (result[0] >= 781116926) {
    return $$Array.of_list(result[1]).join("\n");
  } else {
    return stringOfValue(result[1][0]);
  }
}

exports.join = join;
exports.stringOfString = stringOfString;
exports.stringOfInteger = stringOfInteger;
exports.stringOfFloat = stringOfFloat;
exports.stringOfValue = stringOfValue;
exports.stringOfList = stringOfList;
exports.stringOfResult = stringOfResult;
/* No side effect */
