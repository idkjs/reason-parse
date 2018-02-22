let p = Regex.letters;

let q = Combs.map(Regex.intMapper, Regex.digits);

let pq = Combs.seq([q, p, q, p]);

let n = pq("456ABCdef123abc***");

/* let n = Regex.quotedString({|"The quick brown fox, etc...",|}); */
let s =
  switch n {
  | Fail_(message) => message
  | Success(value, parseData) =>
    Format.sprintf("Value: %s, %s", Node.stringOfValue(value), Node.stringOfParseData(parseData))
  };

print_endline(s);