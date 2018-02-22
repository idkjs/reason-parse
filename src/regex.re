open Node;

let regex = (~group=0, re, s) => {
  let resultOption = Js_re.exec(s, Js_re.fromString("^" ++ Js_re.source(re)));
  switch resultOption {
  | None => Fail_("Match not found -- exec failed.")
  | Some(result) =>
    /* Js.log(Js_re.captures(result)); */
    switch (Belt_Array.get(result |> Js_re.captures, group)) {
    | None => Fail_("Match not found -- captures array is empty.")
    | Some(nullableString) =>
      switch (nullableString |> Js.toOption) {
      | None => Fail_(Format.sprintf("Match not found -- captures[%d] is null.", group))
      | Some(match) =>
        Success(
          Str(match),
          makeParseData(~match, ~rest=Js_string.sliceToEnd(~from=Js_string.length(match), s), ())
        )
      }
    }
  }
};

let maybeWhitespace = regex([%re "/\\s*/"]);

let string = (s) => regex(Js_re.fromString(s));

let digit = regex([%re "/\\d/"]);

let digits = regex([%re "/\\d+/"]);

let letter = regex([%re "/[a-zA-Z]/"]);

let letters = regex([%re "/[a-zA-Z]+/"]);

let quotedString = regex(~group=1, [%re "/\"([^\"]*)\"/"]);

let intMapper = (value) =>
  switch value {
  | Str(n) => Int(int_of_string(n))
  | _ as v => v
  };