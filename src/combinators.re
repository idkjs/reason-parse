open ParseRecord;

let nextOpt = (stream) =>
  try (Some(Stream.next(stream))) {
  | Stream.Failure => None
  };

let rec alt = (stream, string) =>
  switch (nextOpt(stream)) {
  | None => `Fail("None of the parsers matched")
  | Some(p) =>
    switch (p(string)) {
    | `Success(_) as success => success
    | `Fail(_) => alt(stream, string)
    }
  };

let rec tillFailure = (stream, string: string) =>
  switch (nextOpt(stream)) {
  | None => ([], None)
  | Some(p) =>
    switch (p(string)) {
    | `Success(_, {remainder}) as success =>
      let (successes, failOpt) = tillFailure(stream, remainder);
      ([success, ...successes], failOpt)
    | `Fail(_) as fail => ([], Some(fail))
    }
  };

let rec mergeParseData = (initialString, list) =>
  switch list {
  | [] => {match: "", remainder: initialString}
  | [x, ...y] => {...x, match: mergeParseData(initialString, y).match ++ x.match}
  };

let tillFailureMerged = (stream, string) => {
  let (list, failOpt) = tillFailure(stream, string);
  let (values, pRecs) = list |> List.map((`Success(value, pRec)) => (value, pRec)) |> List.split;
  (values |> List.rev, mergeParseData(string, pRecs), failOpt)
};

let stream = (stream, string) =>
  switch (tillFailureMerged(stream, string)) {
  | (values, pRec, None) => `Success((`List(values), pRec))
  | (_, _, Some(`Fail(errorList))) =>
    `Fail([
      Format.sprintf("Parser `Stream failure at parser index %d", stream |> Stream.count),
      ...errorList
    ])
  };

let seq = (ps) => stream(ps |> Stream.of_list);

let appendRange = (list) => list |> List.mapi((i, a) => (i, a));

let filteri = (predicate, list) =>
  List.filter(((i, a)) => predicate(i, a), appendRange(list)) |> List.map(snd);

let keep = (indices, ps, s) =>
  switch (seq(ps, s)) {
  | `Success(`List(list), pRec) =>
    let filteredList = filteri((i, _) => List.mem(i, indices), list);
    `Success((`List(filteredList), pRec))
  | _ as fail => fail
  };

let keepNth = (n, ps) => keep([n], ps);

let keepFirst = (ps) => keepNth(0, ps);

let keepLast = (ps) => keepNth(List.length(ps) - 1, ps);

let between = (p, q, r) => keepNth(1, [p, q, r]);

let successes = (~atLeast=0, stream, string) => {
  let (values, pRec, failOpt) = tillFailureMerged(stream, string);
  let n = List.length(values);
  switch failOpt {
  | None => n >= atLeast ? `Success((`List(values), pRec)) : `Fail(["Stream ended too soon."])
  | Some(`Fail(errorList)) =>
    n >= atLeast ? `Success((`List(values), pRec)) : `Fail(["Parse failed too soon", ...errorList])
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
  successes(~atLeast=0, stream, string)
};