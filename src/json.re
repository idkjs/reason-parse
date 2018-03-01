open Regex;

open Pstream;

open Node;

let lBrace = regex([%re "/{/"]);

let rBrace = regex([%re "/}/"]);

let lBrak = regex([%re "/\\[/"]);

let rBrak = regex([%re "/]/"]);

let colon = regex([%re "/:/"]);

let comma = regex([%re "/,/"]);

let quotedString = regex(~group=1, [%re "/\"([^\"]*)\"/"]);

let firstChar = (s) =>
  try (Some(s.[0])) {
  | Invalid_argument(_) => None
  };

let rec matchArray = (s) => between(lBrak, matchEntries, rBrak, s)
and matchEntries = (s) => sepBy(~separator=comma, matchEntry, s)
and matchEntry = (s) => alt([integer, quotedString, matchArray], s);

print_endline({|[123,"a",[[3,"bcd"]],[5,6],"pqrst"]|} |> matchArray |> stringOfResult);