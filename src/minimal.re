module Combinator = {
  /* type value('a) = [> | `List(value('a))] as 'a;
     type result('a, 'b) = [< | `Fail(string) | `Success(value('a), list(char))] as 'b;
     type parser('a, 'b) = list(char) => result('a, 'b); */
  let seq = (parser1 /*: parser('a, 'b)*/, parser2 /*: parser('a, 'b)*/, charStream) => {
    print_endline("seq called.");
    switch (parser1(charStream)) {
    | `Success(value1, restOfStream1) =>
      switch (parser2(restOfStream1)) {
      | `Success(value2, restOfStream2) => `Success((`List([value1, value2]), restOfStream2))
      | `Fail(message2) =>
        `Fail("Parser 1 succeeded, but parser 2 failed with message: " ++ message2)
      }
    | `Fail(message1) => `Fail("Parser 1 failed with message: " ++ message1)
    }
  };
  /* let alt = (parser1, parser2, charStream) =>
     switch (parser1(charStream)) {
     | `Success(_) as success => success
     | `Fail(_) =>
       switch (parser2(charStream)) {
       | `Success(_) as success => success
       | `Fail(_) => `Fail("Both parsers failed.")
       }
     }; */
  let nextOpt = (stream) =>
    try (Some(Stream.next(stream))) {
    | Stream.Failure => None
    };
  let stringOfStringList = (lst) => lst |> Array.of_list |> Js_array.joinWith(", ");
  let stringOfCharList = (lst) => lst |> List.map(Char.escaped) |> stringOfStringList;
  let rec alt = (parserStream, charStream: list(char)) => {
    print_endline("alt called.");
    print_endline(stringOfCharList(charStream));
    switch (nextOpt(parserStream)) {
    | None => `Fail("None of the parsers matched")
    | Some(p) =>
      switch (p(charStream)) {
      | `Success(_) as success => success
      | `Fail(_) => alt(parserStream, charStream)
      }
    }
  };
};

let stringOfStringList = (lst) => lst |> Array.of_list |> Js_array.joinWith(", ");

let stringOfCharList = (lst) => lst |> List.map(Char.escaped) |> stringOfStringList;

/* type value = [ | `Letter(char) | `Digit(int) | `List(list(value))]; */
let rec stringOfValue = (v) =>
  switch v {
  | `Letter(c) => Char.escaped(c)
  | `Digit(d) => string_of_int(d)
  | `List(lst) => "[" ++ (lst |> List.map(stringOfValue) |> stringOfStringList) ++ "]"
  | _ => "I don't know how to stringify this!"
  };

let letter = (stream) => {
  print_endline("letter called.");
  switch stream {
  | [] => `Fail("Stream is empty")
  | [c, ...cs] =>
    65 <= Char.code(c) && Char.code(c) <= 90 || 97 <= Char.code(c) && Char.code(c) <= 122 ?
      `Success((`Letter(c), cs)) : `Fail("Not a letter.")
  }
};

let digit = (stream) => {
  print_endline("digit called.");
  switch stream {
  | [] => `Fail("Stream is empty")
  | [c, ...cs] =>
    48 <= Char.code(c) && Char.code(c) <= 57 ?
      `Success((`Digit(int_of_string(c |> Char.escaped)), cs)) : `Fail("Not a digit.")
  }
};

let p = Combinator.seq(letter, digit);

/* (
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
   |> print_endline; */
/* let q = Combinator.alt(letter, digit); */
let q = Combinator.alt([letter, digit] |> Stream.of_list);

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
let r = Combinator.seq(p, q);

(
  switch (r(['a', '0', 'b', '1'])) {
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