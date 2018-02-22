open Combinators;

let f = (result) =>
  switch result {
  | Chr(c) => Chr(Char.uppercase(c))
  | _ => assert false
  };

let h = charInRange(97, 122) |> map(f);

let g = character('a') <|> character('b') <|> character('c');

Js.log("aaaabb" |> times(~atLeast=5, ~atMost=10, character('a')) |> stringOfOutput);