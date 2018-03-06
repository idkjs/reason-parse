module Combinator = {
  let seq = (parser1 /*: parser('a, 'b)*/, parser2 /*: parser('a, 'b)*/, charStream) =>
    switch (parser1(charStream)) {
    | `Success(value1, restOfStream1) =>
      switch (parser2(restOfStream1)) {
      | `Success(value2, restOfStream2) => `Success((`List([value1, value2]), restOfStream2))
      | `Fail(message2) =>
        `Fail("Parser 1 succeeded, but parser 2 failed with message: " ++ message2)
      }
    | `Fail(message1) => `Fail("Parser 1 failed with message: " ++ message1)
    };
  let nextOpt = (stream) =>
    try (Some(Stream.next(stream))) {
    | Stream.Failure => None
    };
  let stringOfStringList = (lst) => lst |> Array.of_list |> Js_array.joinWith(", ");
  let stringOfCharList = (lst) => lst |> List.map(Char.escaped) |> stringOfStringList;
  let rec alt = (parserStream, charStream: list(char)) =>
    switch (nextOpt(parserStream)) {
    | None => `Fail("None of the parsers matched")
    | Some(p) =>
      switch (p(charStream)) {
      | `Success(_) as success => success
      | `Fail(_) => alt(parserStream, charStream)
      }
    };
  let rec tillFailure = (parserStream, charStream) =>
    switch (nextOpt(parserStream)) {
    | None => ([], Some(`StreamFailure), charStream)
    | Some(p) =>
      switch (p(charStream)) {
      | `Fail(_) as fail => ([], Some(`ParseFailure(fail)), charStream)
      | `Success(value, restOfCharStream) =>
        let (values, failure, restOfRestOfCharStream) =
          tillFailure(parserStream, restOfCharStream);
        ([value, ...values], failure, restOfRestOfCharStream)
      }
    };
  let tillFailureWrapped = (parserStream, charStream) =>
    switch (tillFailure(parserStream, charStream)) {
    | (successes, Some(failure), restOfCharStream) => (successes, failure, restOfCharStream)
    | (_, None, _) => assert false
    };
  let stream = (parserStream, charStream) =>
    switch (tillFailureWrapped(parserStream, charStream)) {
    | (successes, `StreamFailure, restOfCharStream) =>
      `Success((`List(successes), restOfCharStream))
    | (_, `ParseFailure(`Fail(fail)), _) =>
      let message = Format.sprintf("Parser %d failed: %s", Stream.count(parserStream), fail);
      `Fail(message)
    };
};

let stringOfStringList = (lst) => "[" ++ (lst |> Array.of_list |> Js_array.joinWith(", ")) ++ "]";

let stringOfCharList = (lst) => lst |> List.map(Char.escaped) |> stringOfStringList;

/* type value = [ | `Letter(char) | `Digit(int) | `List(list(value))]; */
let rec stringOfValue = (v) =>
  switch v {
  | `Letter(c) => Char.escaped(c)
  | `Digit(d) => string_of_int(d)
  | `List(lst) => lst |> List.map(stringOfValue) |> stringOfStringList
  /* | _ => "I don't know how to stringify this!" */
  };

let letter = (stream) => {
  print_endline("letter: " ++ stringOfCharList(stream));
  switch stream {
  | [] => `Fail("Stream is empty")
  | [c, ...cs] =>
    65 <= Char.code(c) && Char.code(c) <= 90 || 97 <= Char.code(c) && Char.code(c) <= 122 ?
      `Success((`Letter(c), cs)) : `Fail("Not a letter.")
  }
};

let digit = (stream) =>
  switch stream {
  | [] => `Fail("Stream is empty")
  | [c, ...cs] =>
    48 <= Char.code(c) && Char.code(c) <= 57 ?
      `Success((`Digit(int_of_string(c |> Char.escaped)), cs)) : `Fail("Not a digit.")
  };

let p = Combinator.stream([letter, digit, digit] |> Stream.of_list);

(
  switch (p(['a', '0', 'b', '1'])) {
  | `Success(value, rest) =>
    Format.sprintf(
      "success!\nvalue: %s, rest: %s",
      value |> stringOfValue,
      rest |> stringOfCharList
    )
  | `Fail(message) => Format.sprintf("fail!\n%s", message)
  }
)
|> print_endline;
/* let q = Combinator.alt(letter, digit); */
/* let q = Combinator.alt([letter, digit] |> Stream.of_list); */
/* (
     switch (q(['a', '0', 'b', '1'])) {
     | `Success(value, rest) =>
       Format.sprintf(
         "success!\nvalue: %s, rest: %s",
         value |> stringOfValue,
         rest |> stringOfCharList
       )
     | `Fail(message) => Format.sprintf("fail!\n%s", message)
     }
   )
   |> print_endline; */
/* let r = Combinator.seq(p, q);

   (
     switch (r(['a', '0', 'b', '1'])) {
     | `Success(value, rest) =>
       Format.sprintf(
         "Success! value: %s, rest: %s",
         value |> stringOfValue,
         rest |> stringOfCharList
       )
     | `Fail(message) => Format.sprintf("Fail! %s", message)
     }
   )
   |> print_endline; */
/* let (`Success(value, rest), _) =
     Combinator.tillFailure(
       [letter, digit, letter, digit] |> Stream.of_list,
       ['a', '0', 'b', '1', '*']
     );

   print_endline("Parsed value: " ++ (value |> stringOfValue));

   let s = stringOfValue(value);

   print_endline("Remainder: " ++ stringOfCharList(rest)); */