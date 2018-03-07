open RegExp;

open CombEager;

let k = regExp([%re "/^\"[A-Za-z]+\"/"]);

let quotedString = regExp([%re "/^\"[A-Za-z]*\"/"]);

let number = (s) =>
  switch (regExp([%re "/^\\d+/"], s)) {
  | `Fail(_) as fail => fail
  | `Success(`String(s), remainder) => `Success((`Number(int_of_string(s)), remainder))
  };

let trim = (p) => between(maybeWhitespace, p, maybeWhitespace);

let comma = trim(regExp([%re "/^,/"]));

let colon = trim(regExp([%re "/^:/"]));

let lBrak = trim(regExp([%re "/^\\[/"]));

let rBrak = trim(regExp([%re "/^\\]/"]));

let lBrace = trim(regExp([%re "/^\\{/"]));

let rBrace = trim(regExp([%re "/^\\}/"]));

let rec primValu = (s) => alt([number, quotedString], s)
and valu = (s) => alt([primValu, array, obj], s)
and array = (s) => between(lBrak, sepBy(~separator=comma, valu), rBrak, s)
and obj = (s) => between(lBrace, sepBy(~separator=comma, pair), rBrace, s)
and pair = (s) => keyVal(~separator=colon, k, valu, s);

let rec stringOfValue = (value) =>
  switch value {
  | `Number(n) => string_of_int(n)
  | `String(s) => s
  | `Pair(k, v) => "(" ++ stringOfValue(k) ++ ", " ++ stringOfValue(v) ++ ")"
  | `List(list) =>
    "[" ++ (list |> List.map(stringOfValue) |> Array.of_list |> Js_array.joinWith(", ")) ++ "]"
  };

let stringOfResult = (result) =>
  switch result {
  | `Success(value, _) => stringOfValue(value)
  | `Fail(fail) => "Fail: " ++ fail
  | _ => assert false
  };

{|
   [[   {"abc":456,"cde":987},"abcd", [[123, 321, "xyz", "", 1], ""]]
      ,  321 ]   |}
|> array
|> stringOfResult
|> Js.log;

let r = {|{"abc":456,"cde":987}|} |> valu |> stringOfResult |> Js.log;