let tillFailure = Pstream.tillFailure;

let p = Regex.letters;

let q = Regex.digits;

let r = Json.quotedString;

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

print_endline(
  Json.quotedString({|"My bonny lies over the ocean."sdfa  324|}) |> Node.stringOfResult
);

print_endline(Regex.integer("123abc") |> Node.stringOfResult);

print_endline(Regex.float("123.000abc") |> Node.stringOfResult);

open Json;

print_endline(comma(",:][}{") |> Node.stringOfResult);

let primVal = Pstream.alt([quotedString, Regex.integer]);

let primValArray = Pstream.between(lBrak, Pstream.sepBy(~separator=comma, primVal), rBrak);

let between = Pstream.between;

let sepBy = Pstream.sepBy;

let alt = Pstream.alt;

let rec array = (primVal) => {
  Js.log("array called.");
  between(lBrak, sepBy(~separator=comma, arrayVal(primVal)), rBrak)
}
and arrayVal = (primVal) => alt([primVal, array(primVal)]);

let primVal = alt([Regex.integer, quotedString]);

print_endline("");

print_endline(
  array(primVal, {|[155,"My bonny lies over the ocean.",324]***|}) |> Node.stringOfResult
);

print_endline("");