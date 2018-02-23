open Node;

exception Break(string, result);

let reduceStream = (reducer, init, stream) => {
  let accRef = ref(init);
  try {
    stream
    |> Stream.iter(
         (x) => {
           accRef := reducer(accRef^, x, stream |> Stream.count);
           ()
         }
       );
    accRef^
  } {
  | Break(string, result) => (string, result)
  }
};

let altReducer = ((string, result), parser, _) =>
  switch (parser(string)) {
  | Success(_) as success => Break(string, success) |> raise
  | Fail(_) => (string, result)
  };

let alt = (ps, s) =>
  reduceStream(altReducer, (s, Fail("All the parsers failed.")), ps |> Stream.of_list) |> snd;

let extractList = (value) =>
  switch value {
  | Lst(values) => values
  | _ => Js.Exn.raiseError("Don't pass a non-list to extractList!")
  };

let seqReducer = (nParsers, (string, result), parser, count) =>
  switch (result, parser(string)) {
  | (Fail(_), _) => Break(string, Fail("You shouldn't be here!")) |> raise
  | (_, Fail(message)) =>
    Break(string, Fail(Format.sprintf("Parser %d of %d failed: %s", count, nParsers, message)))
    |> raise
  | (Success(prevValue, prevParseData), Success(value, parseData)) => (
      parseData.rest,
      Success(
        Lst([value, ...extractList(prevValue)]),
        {...parseData, match: prevParseData.match ++ parseData.match}
      )
    )
  };

let seq = (ps, s) =>
  reduceStream(seqReducer(List.length(ps)), (s, Combs.success(Lst([]), s)), ps |> Stream.of_list)
  |> snd;

let p = Regex.letters;

let q = Combs.map(~f=Regex.intMapper, Regex.digits);

let r = Regex.maybeWhitespace;

let pqr = seq([p, q, r, q]);

print_endline("efg123   436***" |> pqr |> Node.stringOfResult);