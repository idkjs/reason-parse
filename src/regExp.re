let regExp = (re, string) =>
  switch (Revamp.find(re |> Js_re.source, string)) {
  | Some(match) =>
    let remainder: string = string |> Js_string.sliceToEnd(~from=match |> String.length);
    `Success((`String(match), remainder))
  | None => `Fail("Match not found.")
  };

let char = (c: char) => regExp(c |> Char.escaped |> ((s) => "^" ++ s) |> Js_re.fromString);

let letter = (s: string) => regExp([%re "/^[A-Za-z]/"], s);

let letters = (s: string) => regExp([%re "/^[A-Za-z]+/"], s);

let digit = (s: string) => regExp([%re "/^[0-9]/"], s);

let digits = (s: string) => regExp([%re "/^[0-9]+/"], s);

let maybeWhitespace = (s: string) => regExp([%re "/^\\s*/"], s);