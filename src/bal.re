type parens =
  | Pair(option(parens));

let rec bal = (s) =>
  if (s.[0] === '[') {
    let rest = Js_string.sliceToEnd(~from=1, s);
    let inner = bal(rest);
    Pair(Some(inner))
  } else if (s.[0] === ']') {
    Pair(None)
  } else {
    Js.Exn.raiseError("Unrecognized character.")
  };

let rec stringOfPair = (pair) =>
  switch pair {
  | Pair(None) => ""
  | Pair(Some(inner)) => Format.sprintf("(%s)", stringOfPair(inner))
  };

print_endline("[[[]]]" |> bal |> stringOfPair);

type arr = list(entry)
and entry =
  | Int(int)
  | Arr(arr);