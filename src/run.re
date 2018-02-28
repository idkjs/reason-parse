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

print_endline(results |> List.split |> snd |> Pstream.mergeParseData |> Node.stringOfParseData);

print_endline(Pstream.seqqq([p, q, p, q], "abc123pqr789xyz") |> Node.stringOfResult);

let atoj = Pstream.atLeast(10, Regex.letter, "abcdefghijklmnopqrstuvwxyz12345");

print_endline(atoj |> Node.stringOfResult);