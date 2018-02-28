open Node;

let hd = (stream) =>
  try (Some(Stream.next(stream))) {
  | _ => None
  };

let rec reduce = (reducer, acc, stream) =>
  switch (stream |> hd) {
  | Some(x) => reduce(reducer, reducer(acc, x), stream)
  | None => acc
  };

exception Break(result);

let alt = (ps, s) => {
  let stream = Stream.of_list(ps);
  let f = (acc, p) =>
    try (
      switch (p(s)) {
      | Success(_) as success => Break(success) |> raise
      | Fail(_) => acc
      }
    ) {
    | Break(success) => success
    };
  reduce(f, Fail("alt failed: none of the parsers matched."), stream)
};

exception ParseFailureExn(list((value, parseData)), string, string);

type termination =
  | EndOfStream(int)
  | ParseFailure(int, string);

let tillFailure = (stream, string) => {
  let f = ((list, rest), parser) =>
    switch (parser(rest)) {
    | Success(value, parseData) => ([(value, parseData), ...list], parseData.rest)
    | Fail(message) => raise(ParseFailureExn(list, rest, message))
    };
  try {
    let (list, rest) = reduce(f, ([], string), stream);
    (list, rest, EndOfStream(stream |> Stream.count))
  } {
  | ParseFailureExn(list, rest, message) => (
      list,
      rest,
      ParseFailure(stream |> Stream.count, message)
    )
  }
};

let rec mergeParseData = (~parserName=None, initialString, list) =>
  switch list {
  | [] => {match: "", rest: initialString, parserName}
  | [x, ...y] => {...x, match: mergeParseData(~parserName, initialString, y).match ++ x.match}
  };

let stream = (stream, string) =>
  switch (tillFailure(stream, string)) {
  | (list, _, EndOfStream(_)) =>
    let (values, parseData) = List.split(list);
    Success(Lst(values |> List.rev), mergeParseData(string, parseData))
  | (_, _, ParseFailure(count, message)) =>
    Fail(Format.sprintf("Parser failure at stream index %d: %s", count, message))
  };

let seq = (ps, s) =>
  switch (tillFailure(ps |> Stream.of_list, s)) {
  | (list, _, EndOfStream(_)) =>
    let (values, parseData) = List.split(list);
    Success(Lst(values |> List.rev), mergeParseData(s, parseData))
  | (_, _, ParseFailure(count, message)) =>
    Fail(Format.sprintf("Parser %d of %d failed: %s", count, List.length(ps), message))
  };

let keepNth = (n, ps, s) =>
  switch (seq(ps, s)) {
  | Success(Lst(list), parseData) =>
    switch (Belt_List.get(list, n)) {
    | Some(value) => Success(value, parseData)
    | None =>
      Fail(
        Format.sprintf(
          "Index out of range: 0-based index = %d, length of parser sequence = %d",
          n,
          ps |> List.length
        )
      )
    }
  | Success(_, _) => assert false
  | Fail(f) => Fail(f)
  };

let keepFirst = keepNth(0);

let keepLast = (ps) => keepNth(List.length(ps) - 1, ps);

let between = (p, q, r) => keepNth(1, [p, q, r]);

let successes = (~atLeast=0, stream, string) =>
  switch (tillFailure(stream, string)) {
  | (list, _, EndOfStream(count))
  | (list, _, ParseFailure(count, _)) =>
    if (List.length(list) >= atLeast) {
      print_endline(Format.sprintf("count: %d, atLeast: %d", count, atLeast));
      let (values, parseData) = List.split(list);
      Success(Lst(values |> List.rev), mergeParseData(string, parseData))
    } else {
      Fail(
        Format.sprintf(
          "Only %d instance(s) matched. Needed to match at least %d instance(s).",
          list |> List.length,
          atLeast
        )
      )
    }
  };

let _times = (~atLeast=0, ~atMost=?, parser) =>
  successes(
    ~atLeast,
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

let zeroOrMore = (parser) => _times(~atLeast=0, parser);

let oneOrMore = (parser) => _times(~atLeast=1, parser);

let atLeast = (atLeast, parser) => _times(~atLeast, parser);

let atMost = (atMost) => _times(~atLeast=0, ~atMost);

let atLeastAtMost = (atLeast, atMost) => _times(~atLeast, ~atMost);

let nTimes = (n) => atLeastAtMost(n, n);

let sepBy = (~separator, parser, string) => {
  let stream = Stream.from((n) => n === 0 ? Some(parser) : Some(keepLast([separator, parser])));
  let result = successes(~atLeast=0, stream, string);
  switch result {
  | Success(_) as success => success
  | Fail(_) as fail => fail
  }
};