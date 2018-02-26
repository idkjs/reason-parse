open Node;

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

let seq = (ps, rest) => {
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
};

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