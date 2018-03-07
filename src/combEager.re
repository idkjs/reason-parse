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