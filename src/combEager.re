let toLazy = LazyList.toLazy;

let alt = (parsers) => CombLazy.alt(parsers |> toLazy);

let seq = (parsers) => CombLazy.seq(parsers |> toLazy);

let atLeast = (n, parsers) => CombLazy.atLeast(n, parsers |> toLazy);

let many = CombLazy.many;

let keep = (indices, parsers) => CombLazy.keep(indices, parsers |> toLazy);

let keepNth = (n, parsers) => CombLazy.keepNth(n, parsers |> toLazy);

let keepFirst = (parsers) => CombLazy.keepFirst(parsers |> toLazy);

let keepLast = CombLazy.keepLast;

let between = CombLazy.between;

let sepBy = CombLazy.sepBy;

let keyVal = (~separator, p, q, s) =>
  switch (seq([p, separator, q], s)) {
  | `Success(`List([k, _, v]), remainder) => `Success((`Pair((k, v)), remainder))
  /* switch values {
     | [k, v] => `Success((`Pair((k, v)), remainder))
     | _ => assert false
     } */
  | `Fail(_) as fail => fail
  | _ => assert false
  };