type result_t =
  | Chr(char)
  | Str(string)
  | Seq(list(result_t))
  | Nothing;

let rec stringOfResult = (result) =>
  switch result {
  | Chr(c) => "'" ++ Char.escaped(c) ++ "'"
  | Str(s) => s
  | Seq(resultList) =>
    "["
    ++ (List.map(stringOfResult, resultList) |> Array.of_list |> Js_array.joinWith(", "))
    ++ "]"
  | Nothing => "Nothing"
  };

type output_t =
  | Success(result_t, string)
  | Fail(string);

let map = (f: result_t => result_t, p: string => output_t, s) =>
  switch (p(s)) {
  | Success(result, leftover) => Success(f(result), leftover)
  | Fail(_) as x => x
  };

let stringOfOutput = (output) =>
  switch output {
  | Success(result, leftover) => "Result: " ++ stringOfResult(result) ++ ", Leftover: " ++ leftover
  | Fail(message) => "Fail: " ++ message
  };

let succeedWith = (result, s) => Success(result, s);

let fail = (message, _) => Fail(message);

let satisfy = (f, s) =>
  String.(
    switch (length(s)) {
    | 0 => Fail("Parsing empty string!")
    | _ =>
      f(s.[0]) ?
        Success(Chr(s.[0]), sub(s, 1, length(s) - 1)) : Fail("s.[0] does not satisfy predicate.")
    }
  );

let character = (a) => satisfy((===)(a));

/* a regexp, whitespace, optwhitespace, any, takeWhile */
let rec seq = (ps, s) =>
  switch ps {
  | [] => succeedWith(Seq([]), s)
  | [p, ...qs] =>
    switch (p(s)) {
    | Fail(_) => Fail("One of the parsers in the sequence failed.")
    | Success(pResult, pLeftover) =>
      switch (seq(qs, pLeftover)) {
      | Success(Seq(resultList), leftover) => Success(Seq([pResult, ...resultList]), leftover)
      | _ => Fail("One of the parsers in the sequence failed.")
      }
    }
  };

let (>>>) = (p, q) => seq([p, q]);

let rec alt = (ps, s) =>
  switch ps {
  | [] => Fail("None of the parsers matched.")
  | [p, ...qs] =>
    switch (p(s)) {
    | Success(_) as x => x
    | Fail(_) => alt(qs, s)
    }
  };

let (<|>) = (p, q) => alt([p, q]);

/* between, followed by, not followed by, sepby, lazy, chain, fallback, skip, trim, wrap, times, atmost, atleast */
let toCharList = (s) => s |> Js_string.split("") |> Js_array.map((t) => t.[0]) |> Array.to_list;

let charInString = (s) => s |> toCharList |> List.map(character) |> alt;

let letter = charInString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");

let digit = charInString("0123456789");

let rec inclusiveRange = (m, n) =>
  if (m < n) {
    [m, ...inclusiveRange(m + 1, n)]
  } else if (m > n) {
    [m, ...inclusiveRange(m - 1, n)]
  } else {
    [m]
  };

let concatCharList = (charList) =>
  charList |> List.map(Char.escaped) |> Array.of_list |> Js_array.joinWith("");

let unwrapSeq = (result) =>
  switch result {
  | Seq(results) => results
  | _ => assert false
  };

let unwrapChr = (result) =>
  switch result {
  | Chr(c) => c
  | _ => assert false
  };

let wrapStr = (s) => Str(s);

let thisString = (s) =>
  s
  |> toCharList
  |> List.map(character)
  |> seq
  |> map((result) => result |> unwrapSeq |> List.map(unwrapChr) |> concatCharList |> wrapStr);

let charInRange = (m, n) =>
  inclusiveRange(m, n) |> List.map(Char.chr) |> List.map(character) |> alt;

let rec times = (~results=[], ~atLeast=0, ~atMost, p, s) : output_t => {
  /* atLeast and/or atMost should be optional!!! */
  if (atLeast > atMost) {
    Js.Exn.raiseError("atLeast > atMost??")
  };
  let count = List.length(results);
  switch (p(s)) {
  | Fail(_) =>
    if (atLeast <= count && count <= atMost) {
      Success(Seq(results), s)
    } else if (count < atLeast) {
      Fail("Didn't match enough times.")
    } else {
      Js.Exn.raiseError("count > atMost??")
    }
  | Success(result, leftover) =>
    if (count < atMost) {
      times(~results=[result, ...results], ~atLeast, ~atMost, p, leftover)
    } else if (count === atMost) {
      Fail("Matched too many times.")
    } else {
      Js.Exn.raiseError("count > atMost??")
    }
  }
};