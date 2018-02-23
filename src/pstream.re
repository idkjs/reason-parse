open Node;

exception Done(result);

let streamAlt = (stream, str) =>
  try {
    stream
    |> Stream.iter(
         (p) =>
           switch (p(str)) {
           | Success(_) as success => raise(Done(success))
           | Fail_(_) => ()
           }
       );
    Fail_("All the parsers failed.")
  } {
  | Done(success) => success
  /* | Failure("") => Fail_("All the parsers failed.") */
  };

let p = Regex.letters;

let q = Combs.map(~f=Regex.intMapper, Regex.digits);

let porq = streamAlt([p, q] |> Stream.of_list);

print_endline("*123abc" |> porq |> Node.stringOfResult);