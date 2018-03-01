open Node;

let regex = (~group=0, re, s) => {
  let resultOption = Js_re.exec(s, Js_re.fromString("^" ++ Js_re.source(re)));
  switch resultOption {
  | None => Fail("Match not found -- exec failed.")
  | Some(result) =>
    let captures = result |> Js_re.captures;
    switch (Belt_Array.get(captures, 0), Belt_Array.get(captures, group)) {
    | (None, _) => Fail("Match not found -- captures array is empty.")
    | (_, None) => Fail(Format.sprintf("Capture group %d is undefined.", group))
    | (Some(nullableMatch), Some(nullableMatchGroup)) =>
      switch (nullableMatch |> Js.toOption, nullableMatchGroup |> Js.toOption) {
      | (None, _) => Fail("Match not found -- capture group 0 is null.")
      | (_, None) => Fail(Format.sprintf("Match not found -- capture group %d is null.", group))
      | (Some(match), Some(matchGroup)) =>
        Success(
          Str(matchGroup),
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

let intMapper = (value) =>
  switch value {
  | Str(n) => Int(int_of_string(n))
  | _ as v => v
  };

let floatMapper = (value) =>
  switch value {
  | Str(n) => Flt(float_of_string(n))
  | _ as v => v
  };

let map = (~f=(x) => x, ~g=(x) => x, p, s) =>
  switch (p(s)) {
  | Fail(_) as x => x
  | Success(value, parseData) => Success(f(value), g(parseData))
  };

let integer = map(~f=intMapper, digits);

let float = regex([%re "/\\d+\\.\\d+/"]) |> map(~f=floatMapper);