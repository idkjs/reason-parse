open Values;

let regex = (~name=?, ~group=0, re, s) => {
  let resultOption = Js_re.exec(s, Js_re.fromString("^" ++ Js_re.source(re)));
  switch resultOption {
  | None => `Fail(["Match not found -- exec `Failed."])
  | Some(result) =>
    let captures = result |> Js_re.captures;
    switch (Belt_Array.get(captures, 0), Belt_Array.get(captures, group)) {
    | (None, _) => `Fail(["Match not found -- captures array is empty."])
    | (_, None) => `Fail([Format.sprintf("Capture group %d is undefined.", group)])
    | (Some(nullableMatch), Some(nullableValue)) =>
      switch (nullableMatch |> Js.toOption, nullableValue |> Js.toOption) {
      | (None, _) => `Fail(["Match not found -- capture group 0 is null."])
      | (_, None) => `Fail([Format.sprintf("Match not found -- capture group %d is null.", group)])
      | (Some(match), Some(value)) =>
        `Success((
          `String(value),
          {
            match,
            remainder: Js_string.sliceToEnd(~from=Js_string.length(match), s),
            name:
              switch name {
              | Some(name) => name
              | None => Js_re.source(re)
              }
          }
        ))
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

let intMapper = (`String(n)) => `Integer(n);

let floatMapper = (`String(x)) => `Float(x);

let mapResult = (f, result) =>
  switch result {
  | `Success(value, pRec) => `Success((f(value), pRec))
  | `Fail(_) as fail => fail
  };

let map = (f, p, s) => mapResult(f, p(s));

let float = map(floatMapper, regex([%re "/\\d+\\.\\d+/"]));