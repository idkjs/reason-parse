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

let seq = (ps, s) =>
  switch (tillFailure(ps |> Stream.of_list, s)) {
  | (list, _, EndOfStream(_)) =>
    let (values, parseData) = List.split(list);
    Success(Lst(values |> List.rev), mergeParseData(s, parseData))
  | (_, _, ParseFailure(count, message)) =>
    Fail(Format.sprintf("Parser %d of %d failed: %s", count, List.length(ps), message))
  };

let atMost = (n, parser, string) => {
  let stream = Stream.from((k) => k < n ? Some(parser) : None);
  let (list, _, _) = tillFailure(stream, string);
  let (values, parseData) = List.split(list);
  Success(Lst(values |> List.rev), mergeParseData(string, parseData))
};

let atLeast = (n, parser, string) => {
  let stream = Stream.from((_) => Some(parser));
  switch (tillFailure(stream, string)) {
  | (list, _, ParseFailure(count, _)) =>
    if (count >= n) {
      let (values, parseData) = List.split(list);
      Success(Lst(values |> List.rev), mergeParseData(string, parseData))
    } else {
      Fail(Format.sprintf("Only %d instances matched.", count))
    }
  | (_, _, EndOfStream(_)) => assert false /* infinite stream */
  }
};

let _times = (~atLeast=0, ~atMost=?, parser, string) => {
  let stream =
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
    );
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
  }
};

let zeroOrMore = (parser) => _times(~atLeast=0, parser);

let atLeast = (atLeast, parser) => _times(~atLeast, parser);

let atMost = (atMost) => _times(~atLeast=0, ~atMost);

let times = (atLeast, atMost) => _times(~atLeast, ~atMost);