open Node;

let regex = (re, s) => {
  let resultOption = Js_re.exec(s, Js_re.fromString("^" ++ Js_re.source(re)));
  switch resultOption {
  | None => Fail("Match not found -- exec failed.")
  | Some(result) =>
    switch (Belt_Array.get(result |> Js_re.captures, 0)) {
    | None => Fail("Match not found -- captures array is empty.")
    | Some(nullableString) =>
      switch (nullableString |> Js.toOption) {
      | None => Fail("Match not found -- captures[0] is null.")
      | Some(match) =>
        Node({
          value: Str(match),
          parseData:
            makeParseData(~match, ~rest=Js_string.sliceToEnd(~from=Js_string.length(match), s), ())
        })
      }
    }
  }
};

let string = (s) => regex(Js_re.fromString(s));

let digit = regex([%re "/\\d/"]);

let digits = regex([%re "/\\d+/"]);

let letter = regex([%re "/[a-zA-Z]/"]);

let letters = regex([%re "/[a-zA-Z]+/"]);

let intMapper = (node) => {
  let value =
    switch node.value {
    | Str(n) => Int(int_of_string(n))
    | _ as v => v
    };
  {...node, value}
};