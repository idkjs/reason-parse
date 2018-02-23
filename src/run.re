let p = Regex.letters;

let q = Combs.map(~f=Regex.intMapper, Regex.digits);

let qpqp = Combs.seq([q, p, q, p]);

let res = qpqp("456ABCdef123abc***");

/* let res = Combs.oneOrMore(Regex.digit, "123456abc"); */
let s =
  switch res {
  | Fail_(message) => message
  | Success(value, parseData) =>
    Format.sprintf("Value: %s, %s", Node.stringOfValue(value), Node.stringOfParseData(parseData))
  };

print_endline(s);