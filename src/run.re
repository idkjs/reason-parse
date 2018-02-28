let tillFailure = Pstream.tillFailure;

let p = Regex.letters;

let q = Regex.digits;

let r = Regex.quotedString;

let s = {|abc123\"abc123\"|};

let t = tillFailure([p, q, p, q] |> Stream.of_list);

let (results, _, _) = t("abc123pqr789");

let print_results = (results) =>
  results
  |> List.iter(
       ((a, b)) => {
         print_endline(a |> Node.stringOfValue);
         print_endline(b |> Node.stringOfParseData)
       }
     );

print_results(results);

print_endline(
  results |> List.split |> snd |> Pstream.mergeParseData("abc123pqr789") |> Node.stringOfParseData
);

print_endline(Pstream.seq([p, q, p, q], "abc123pqr789xyz") |> Node.stringOfResult);

let ds = Regex.digits;

let ls = Regex.letters;

let atoj =
  Pstream.sepBy(~separator=Regex.digit, Regex.letter, "a1b2c3d4efghijklmnopqrstuvwxyz4567pqrs");

print_endline(atoj |> Node.stringOfResult);