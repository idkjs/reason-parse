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

let tillParseFailureOrStreamFailure = (p) => 0;

let tillParseSuccessorStreamFailure = (p) => 0;

let atMost = (n, p) => {
  let stream = Stream.from((k) => k < n ? Some(p) : None);
  ()
};

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

let p = Regex.letters;

let q = Combs.map(~f=Regex.intMapper, Regex.digits);

let r = Regex.maybeWhitespace;

let pqr = seq([p, q, r]);

let ltrs = zeroOrMore(Regex.letter);

let result = pqr("efg12!3   436***");

print_endline(ltrs("abcdef123456") |> stringOfResult);