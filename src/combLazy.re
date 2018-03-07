open LazyList;

let rec alt = (parsers, charStream) =>
  switch parsers {
  | Nil => `Fail("None of the parsers matched.")
  | Cons(p, ps) =>
    switch (p(charStream)) {
    | `Success(_) as success => success
    | `Fail(_) => alt(ps(), charStream)
    }
  };

let rec tillFailure = (parsers, charStream) =>
  switch parsers {
  | Nil => ([], Some(`StreamFailure), charStream)
  | Cons(p, ps) =>
    switch (p(charStream)) {
    | `Fail(_) as fail => ([], Some(`ParseFailure(fail)), charStream)
    | `Success(value, restOfCharStream) =>
      let (values, failure, restOfRestOfCharStream) = tillFailure(ps(), restOfCharStream);
      ([value, ...values], failure, restOfRestOfCharStream)
    }
  };

let tillFailureWrapped = (parsers, charStream) =>
  switch (tillFailure(parsers, charStream)) {
  | (successes, Some(failure), restOfCharStream) => (successes, failure, restOfCharStream)
  | (_, None, _) => assert false
  };

let seq = (parsers, charStream) =>
  switch (tillFailureWrapped(parsers, charStream)) {
  | (successes, `StreamFailure, restOfCharStream) => `Success((`List(successes), restOfCharStream))
  | (successes, `ParseFailure(`Fail(fail)), _) =>
    let message = Format.sprintf("Parser %d failed: %s", List.length(successes), fail);
    `Fail(message)
  };

let atLeast = (n, parsers, charStream) => {
  let (successes, failure, restOfCharStream) = tillFailureWrapped(parsers, charStream);
  switch failure {
  | `StreamFailure
  | `ParseFailure(_) =>
    if (List.length(successes) >= n) {
      `Success((`List(successes), restOfCharStream))
    } else {
      `Fail(Format.sprintf("Only %d of %d matches found.", List.length(successes), n))
    }
  }
};

let many = (~min=0, ~max=?, parser) =>
  switch max {
  | Some(length) => atLeast(min, const(~length, parser))
  | None => atLeast(min, const(parser))
  };

let appendRange = (list) => list |> List.mapi((i, a) => (i, a));

let filteri = (predicate, list) =>
  List.filter(((i, a)) => predicate(i, a), appendRange(list)) |> List.map(snd);

let keep = (indices, ps, s) =>
  switch (seq(ps, s)) {
  | `Success(`List(list), remainder) =>
    let filteredList = filteri((i, _) => List.mem(i, indices), list);
    `Success((`List(filteredList), remainder))
  | _ as fail => fail
  };

let keepNth = (n, ps, s) =>
  switch (keep([n], ps, s)) {
  | `Success(`List(filteredList), remainder) =>
    switch filteredList {
    | [value] => `Success((value, remainder))
    | _ => assert false
    }
  | `Fail(_) as f => f
  };

let keepFirst = (ps) => keepNth(0, ps);

let keepLast = (ps) => keepNth(List.length(ps) - 1, ps |> toLazy);

let between = (p, q, r) => keepNth(1, [p, q, r] |> toLazy);

let sepBy = (~separator, parser, string) =>
  atLeast(0, Cons(parser, () => const(keepLast([separator, parser]))), string);

/* The following aren't combinators and should be moved. */
let stringOfStringList = (lst) => "[" ++ (lst |> Array.of_list |> Js_array.joinWith(", ")) ++ "]";

let stringOfCharList = (lst) => lst |> List.map(Char.escaped) |> stringOfStringList;

let rec stringOfValue = (v) =>
  switch v {
  | `Letter(c) => Char.escaped(c)
  | `Digit(d) => string_of_int(d)
  | `String(s) => s
  | `List(lst) => lst |> List.map(stringOfValue) |> stringOfStringList
  };

let stringOfResult = (result) : string =>
  switch result {
  | `Fail(fail) => Format.sprintf("fail!\n%s", fail)
  | `Success(value, remainder) =>
    Format.sprintf("\nsuccess!\nvalue: %s, rest: %s\n", value |> stringOfValue, remainder)
  };