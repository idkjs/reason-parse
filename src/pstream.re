open Node;

let hd = (stream) =>
  try (Some(Stream.next(stream))) {
  | _ => None
  };

let rec red = (reducer, acc, stream) =>
  switch (stream |> hd) {
  | Some(x) => red(reducer, reducer(acc, x), stream)
  | None => acc
  };

exception Break(result);

let altt = (ps, s) => {
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
  red(f, Fail("alt failed: none of the parsers matched."), stream)
};

let seqUnwrapped = (stream, s) => {
  let f = ((_, n, vList, pData), p) =>
    switch (p(s)) {
    | Success(value, parseData) as success => (
        Some(success),
        n + 1,
        [value, ...vList],
        {...parseData, match: pData.match ++ parseData.match}
      )
    | Fail(_) as fail => (Some(fail), n, vList, pData)
    };
  let init = (None, 0, [], {parserName: None, match: "", rest: s});
  red(f, init, stream)
};

let seq = (ps, s) => {
  let (lastResult, _, vList, pData) = seqUnwrapped(ps |> Stream.of_list, s);
  switch lastResult {
  | None
  | Some(Success(_)) => Success(Lst(vList), pData)
  | Some(Fail(_) as fail) => fail
  }
};

let zOrMore = (p, s) => {
  let stream = Stream.from((_) => p);
  let (_, _, vList, pData) = seqUnwrapped(stream, s);
  Success(Lst(vList), pData)
};

let oneOrMore = (p, s) => {
  let stream = Stream.from((_) => p);
  let (lastResult, n, vList, pData) = seqUnwrapped(stream, s);
  n >= 1 ?
    Success(Lst(vList), pData) :
    (
      switch lastResult {
      | Some(result) => result
      | None => assert false
      }
    )
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
    let (list, rest) = red(f, ([], string), stream);
    (list, rest, EndOfStream(stream |> Stream.count))
  } {
  | ParseFailureExn(list, rest, message) => (
      list,
      rest,
      ParseFailure(stream |> Stream.count, message)
    )
  }
};

let rec mergeParseData = (list) =>
  switch list {
  | [] => {match: "", rest: "", parserName: None}
  | [x, ...y] => {...x, match: mergeParseData(y).match ++ x.match}
  };

let seqqq = (ps, s) =>
  switch (tillFailure(ps |> Stream.of_list, s)) {
  | (list, _, EndOfStream(_)) =>
    let (values, parseData) = List.split(list);
    Success(Lst(values |> List.rev), mergeParseData(parseData))
  | (_, _, ParseFailure(count, message)) =>
    Fail(Format.sprintf("Parser %d of %d failed: %s", count, List.length(ps), message))
  };

let atMost = (n, parser, string) => {
  let stream = Stream.from((k) => k < n ? Some(parser) : None);
  switch (tillFailure(stream, string)) {
  | (list, _, _) =>
    let (values, parseData) = List.split(list);
    Success(Lst(values |> List.rev), mergeParseData(parseData))
  }
};

let atLeast = (n, parser, string) => {
  let stream = Stream.from((_) => Some(parser));
  switch (tillFailure(stream, string)) {
  | (list, _, ParseFailure(count, _)) =>
    if (count >= n) {
      let (values, parseData) = List.split(list);
      Success(Lst(values |> List.rev), mergeParseData(parseData))
    } else {
      Fail(Format.sprintf("Only %d instances matched.", count))
    }
  | (_, _, EndOfStream(_)) => assert false
  }
};

/* let stream = Stream.from(_ => parser); */
/* switch (tillFailure(stream, string)) {
   | (list, _, ParseFailure(count)) => 0 */
/* if (count >= n) {
     let (values, parseData) = List.split(list);
     Success(Lst(values |> List.rev), mergeParseData(parseData))
   } else {Fail(Format.sprintf("Only %d instances matched.", count))} */
let reduce = (~break=(_, _, _) => false, reducer, init, stream) => {
  let acc = ref(init);
  try {
    let x = ref(stream |> Stream.next);
    while (! break(acc^, x^, stream |> Stream.count)) {
      acc := reducer(acc^, x^, stream |> Stream.count);
      x := stream |> Stream.next
    };
    acc^
  } {
  | Stream.Failure => acc^
  }
};

let alt = (ps, s) =>
  reduce(
    ~break=
      (acc, _, _) =>
        switch acc {
        | Success(_) => true
        | Fail(_) => false
        },
    (_, parser, _) => parser(s),
    Fail("All the parsers failed."),
    ps |> Stream.of_list
  );

let seqReducer = ((valueList, valueListLength, prevParseData), parser, _) =>
  switch (parser(prevParseData.rest)) {
  | Fail(_) => (valueList, valueListLength, prevParseData)
  | Success(value, parseData) => (
      [value, ...valueList],
      valueListLength + 1,
      {...parseData, match: prevParseData.match ++ parseData.match}
    )
  };

let isSuccess = (result) =>
  switch result {
  | Success(_) => true
  | Fail(_) => false
  };

/* let seq = (ps, rest) => {
     let stream = ps |> Stream.of_list;
     let (valueList, valueListLength, parseData) =
       reduce(
         ~break=((_, valueListLength, _), _, count) => valueListLength !== count - 1,
         seqReducer,
         ([], 0, {parserName: None, match: "", rest}),
         stream
       );
     let count = Stream.count(stream);
     if (valueListLength === count) {
       Success(Lst(valueList |> List.rev), parseData)
     } else {
       let message = Format.sprintf("Parser %d of %d failed.", valueListLength + 1, count);
       Fail(message)
     }
   }; */
let zeroOrMore = (p, rest) => {
  let stream = Stream.from((_) => Some(p));
  let reducer = ((valueList, prevParseData), _, _) =>
    switch (p(prevParseData.rest)) {
    | Success(value, parseData) => (
        [value, ...valueList],
        {...parseData, match: prevParseData.match ++ parseData.match}
      )
    | Fail(_) => raise(Stream.Failure)
    };
  let (valueList, parseData) =
    reduce(~break=(_, _, _) => false, reducer, ([], {parserName: None, match: "", rest}), stream);
  Success(Lst(valueList |> List.rev), parseData)
};