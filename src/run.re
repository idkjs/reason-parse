/* let p = Regex.letters;

   let q = Combs.map(Regex.intMapper, Regex.digits);

   let pq = Combs.seq([q, p, q, p]);

   let n = pq("456ABCdef123abc***"); */
let n = Regex.quotedString({|"The quick brown fox, etc...",|});

let s =
  switch n {
  | Fail(message) => message
  | Node(node) => Node.stringOfNode(node)
  };

print_endline(s);