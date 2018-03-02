open ParseRecord;

module type Base = {type value;};

let hd = (stream) =>
  try (Some(Stream.next(stream))) {
  | _ => None
  };

let rec reduce = (reducer, acc, stream) =>
  switch (stream |> hd) {
  | Some(x) => reduce(reducer, reducer(acc, x), stream)
  | None => acc
  };

module Comb = (B: Base) => {
  type value = B.value;
  type result = [ | `Fail(list(string)) | `Success(value, parseRecord)];
  exception Break(value, parseRecord);
  let alt = (ps, s: string) : result => {
    let stream = Stream.of_list(ps);
    let f = (acc, p) =>
      try (
        switch (p(s)) {
        | `Success(value, pRec) => Break(value, pRec) |> raise
        | `Fail(_) => acc
        }
      ) {
      | Break(value, pRec) => `Success((value, pRec))
      };
    let ans = reduce(f, `Fail(["alt `Failed: none of the parsers matched."]), stream);
    ans
  };
  /* let p = alt([letters, digits]);
     print_endline(Format.sprintf("Value: %s", "987abc123" |> p |> stringOfResult)); */
  exception ParseFailure(
              list([ | `Success(value, parseRecord)]),
              option([ | `Fail(list(string))])
            );
  /* type termination =
     | EndOfStream(int)
     | ParseFailure(int, string); */
  let tillFailure = (stream, string) => {
    let f = ((list, _), parser) => {
      let s =
        switch list {
        | [] => string
        | [`Success(_, {remainder}), ..._] => remainder
        };
      switch (parser(s)) {
      | `Success(_) as success => ([success, ...list], None)
      | `Fail(_) as fail => raise(ParseFailure(list, Some(fail)))
      }
    };
    try (reduce(f, ([], None), stream)) {
    | ParseFailure(list, failOpt) => (list, failOpt)
    }
  };
  let rec mergeParseData = (~name, initialString, list) =>
    switch list {
    | [] => {match: "", remainder: initialString, name}
    | [x, ...y] => {...x, match: mergeParseData(~name, initialString, y).match ++ x.match}
    };
  let stream = (~name, stream, string) => {
    let (list, parsefailure) = tillFailure(stream, string);
    let (values, pRecs) = list |> List.map((`Success(value, pRec)) => (value, pRec)) |> List.split;
    switch parsefailure {
    | None => `Success((`List(values |> List.rev), mergeParseData(~name, string, pRecs)))
    | Some(`Fail(errorList)) =>
      `Fail([
        Format.sprintf("Parser `Stream failure at parser index %d", stream |> Stream.count),
        ...errorList
      ])
    }
  };
  let seq = (~name, ps) => stream(~name, ps |> Stream.of_list);
  /* let seq = (ps, s) =>
     switch (tillFailure(ps |> Stream.of_list, s)) {
     | (list, _, EndOfStream(_)) =>
       let (values, pRec) = List.split(list);
       `Success((`List(values |> List.rev), mergeParseData(s, pRec)))
     | (_, _, ParseFailure(count, message)) =>
       `Fail(Format.sprintf("Parser %d of %d `Failed: %s", count, List.length(ps), message))
     }; */
  /* let rec reduceWithIndexInternal = (f, init, i, list) =>
     switch list {
     | [] => init
     | [x, ...remainder] => reduceWithIndexInternal(f, f(init, x, i), i + 1, remainder)
     }; */
  let addRange = (list) => list |> List.mapi((i, a) => (i, a));
  /* let fold_lefti = (f, init, list) =>
     List.fold_left((acc, i, a) => f(acc, (i, a)), init, addRange(list)); */
  let filteri = (predicate, list) =>
    List.filter(((i, a)) => predicate(i, a), addRange(list)) |> List.map(snd);
  let keep = (~name="keep", indices, ps, s) =>
    switch (seq(~name, ps, s)) {
    | `Success(`List(list), pRec) =>
      let filteredList = filteri((i, _) => List.mem(i, indices), list);
      `Success((`List(filteredList), pRec))
    | _ as fail => fail
    };
  let keepNth = (~name="keepNth", n) => keep(~name, [n]);
  let keepFirst = (~name="keepFirts") => keepNth(~name, 0);
  let keepLast = (~name="keepLast", ps) => keepNth(~name, List.length(ps) - 1, ps);
  let between = (~name="between", p, q, r) => keepNth(~name, 1, [p, q, r]);
};
/*


 let keepNth = (n, ps, s) =>
   switch (seq(ps, s)) {
   | `Success(`List(list), pRec) =>
     switch (Belt_List.get(list, n)) {
     | Some(value) => `Success((value, pRec))
     | None =>
       `Fail(
         Format.sprintf(
           "Index out of range: 0-based index = %d, length of parser sequence = %d",
           n,
           ps |> List.length
         )
       )
     }
   | `Success(_, _) => assert false
   | `Fail(f) => `Fail(f)
   };

 let keepFirst = keepNth(0);

 let keepLast = (ps) => keepNth(List.length(ps) - 1, ps);

 let between = (p, q, r) => keepNth(1, [p, q, r]);

 let successes = (~atLeast=0, stream, string) =>
   switch (tillFailure(stream, string)) {
   | (list, _, EndOfStream(_))
   | (list, _, ParseFailure(_, _)) =>
     if (List.length(list) >= atLeast) {
       let (values, pRec) = List.split(list);
       `Success((`List(values |> List.rev), mergeParseData(string, pRec)))
     } else {
       `Fail(
         Format.sprintf(
           "Only %d instance(s) matched. Needed to match at least %d instance(s).",
           list |> List.length,
           atLeast
         )
       )
     }
   };

 let _times = (~atLeast=0, ~atMost=?, parser) =>
   `Successes(
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
   let result = `Successes(~atLeast=0, stream, string);
   switch result {
   | `Success(_) as `Success => `Success
   | `Fail(_) as `Fail => `Fail
   }
 }; */