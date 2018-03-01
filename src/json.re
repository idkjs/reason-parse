open Regex;

open Pstream;

open Node;

let trim = (p) => between(maybeWhitespace, p, maybeWhitespace);

let lBrace = trim(regex([%re "/{/"]));

let rBrace = trim(regex([%re "/}/"]));

let lBrak = trim(regex([%re "/\\[/"]));

let rBrak = trim(regex([%re "/]/"]));

let colon = trim(regex([%re "/:/"]));

let comma = trim(regex([%re "/,/"]));

let integer = map(~f=intMapper, digits);

let quotedString = regex(~group=1, [%re "/\"([^\"]*)\"/"]);

let firstChar = (s) =>
  try (Some(s.[0])) {
  | Invalid_argument(_) => None
  };

let rec matchArray = (s) => between(lBrak, matchEntries, rBrak, s)
and matchEntries = (s) => sepBy(~separator=comma, matchValue, s)
and matchValue = (s) => alt([integer, quotedString, matchArray], s);

print_endline(
  {|

  [
    123, "a" ,
 [
   [
   3,"bcd"]]   , [5,6],

 "pqrst"]|}
  |> matchArray
  |> stringOfResult
);