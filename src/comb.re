let nextOpt = (stream) =>
  try (Some(Stream.next(stream))) {
  | Stream.Failure => None
  };

let rec altStream = (parserStream, charStream) =>
  switch (nextOpt(parserStream)) {
  | None => `Fail("None of the parsers matched.")
  | Some(p) =>
    switch (p(charStream)) {
    | `Success(_) as success => success
    | `Fail(_) => altStream(parserStream, charStream)
    }
  };

let rec alt = (parsers, charStream) =>
  switch parsers {
  | [] => `Fail("None of the parsers matched.")
  | [p, ...ps] =>
    switch (p(charStream)) {
    | `Success(_) as success => success
    | `Fail(_) => alt(ps, charStream)
    }
  };

let rec tillFailure = (parserStream, charStream) =>
  switch (nextOpt(parserStream)) {
  | None => ([], Some(`StreamFailure), charStream)
  | Some(p) =>
    switch (p(charStream)) {
    | `Fail(_) as fail => ([], Some(`ParseFailure(fail)), charStream)
    | `Success(value, restOfCharStream) =>
      let (values, failure, restOfRestOfCharStream) = tillFailure(parserStream, restOfCharStream);
      ([value, ...values], failure, restOfRestOfCharStream)
    }
  };

let tillFailureWrapped = (parserStream, charStream) =>
  switch (tillFailure(parserStream, charStream)) {
  | (successes, Some(failure), restOfCharStream) => (successes, failure, restOfCharStream)
  | (_, None, _) => assert false
  };

let stream = (parserStream, charStream) =>
  switch (tillFailureWrapped(parserStream, charStream)) {
  | (successes, `StreamFailure, restOfCharStream) => `Success((`List(successes), restOfCharStream))
  | (_, `ParseFailure(`Fail(fail)), _) =>
    let message = Format.sprintf("Parser %d failed: %s", Stream.count(parserStream), fail);
    `Fail(message)
  };

let seq = (parsers) => stream(parsers |> Stream.of_list);

let atLeastStream = (n, parserStream, charStream) => {
  let (successes, failure, restOfCharStream) = tillFailureWrapped(parserStream, charStream);
  switch failure {
  | `StreamFailure
  | `ParseFailure(_) =>
    if (List.length(successes) >= n) {
      `Success((`List(successes), restOfCharStream))
    } else {
      `Fail(Format.sprintf("Only %d of %d matches found.", List.length(successes), n))
    }
  }
};

let many = (~atLeast=0, ~atMost=?, parser) =>
  atLeastStream(
    atLeast,
    Stream.from(
      (n) =>
        switch atMost {
        | Some(x) =>
          if (n < x) {
            Some(parser)
          } else {
            None
          }
        | None => Some(parser)
        }
    )
  );

let appendRange = (list) => list |> List.mapi((i, a) => (i, a));

let filteri = (predicate, list) =>
  List.filter(((i, a)) => predicate(i, a), appendRange(list)) |> List.map(snd);

let keep = (indices, ps, s) =>
  switch (seq(ps, s)) {
  | `Success(`List(list), remainder) =>
    let filteredList = filteri((i, _) => List.mem(i, indices), list);
    `Success((`List(filteredList), remainder))
  | _ as fail => fail
  };

let keepNth = (n, ps, s) =>
  switch (keep([n], ps, s)) {
  | `Success(`List(filteredList), remainder) =>
    switch filteredList {
    | [value] => `Success((value, remainder))
    | _ => assert false
    }
  | `Fail(_) as f => f
  };

let keepFirst = (ps) => keepNth(0, ps);

let keepLast = (ps) => keepNth(List.length(ps) - 1, ps);

let between = (p, q, r) => keepNth(1, [p, q, r]);

let sepBy = (~separator, parser, string) => {
  let stream = Stream.from((n) => n === 0 ? Some(parser) : Some(keepLast([separator, parser])));
  atLeastStream(0, stream, string)
};

let stringOfStringList = (lst) => "[" ++ (lst |> Array.of_list |> Js_array.joinWith(", ")) ++ "]";

let stringOfCharList = (lst) => lst |> List.map(Char.escaped) |> stringOfStringList;

let rec stringOfValue = (v) =>
  switch v {
  | `Letter(c) => Char.escaped(c)
  | `Digit(d) => string_of_int(d)
  | `String(s) => s
  | `List(lst) => lst |> List.map(stringOfValue) |> stringOfStringList
  };

let stringOfResult = (result) : string =>
  switch result {
  | `Fail(fail) => Format.sprintf("fail!\n%s", fail)
  | `Success(value, remainder) =>
    Format.sprintf("\nsuccess!\nvalue: %s, rest: %s\n", value |> stringOfValue, remainder)
  };