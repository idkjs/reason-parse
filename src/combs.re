open Node;

let map = (f, p, s) =>
  switch (p(s)) {
  | Fail(_) as f => f
  | Node(node) => Node(f(node))
  };

let rec alt = (parsers, s) =>
  switch parsers {
  | [] => Fail("Alt failed. (Did you pass it an empty list?)")
  | [p, ...ps] =>
    switch (p(s)) {
    | Fail(_) => alt(ps, s)
    | Node(_) as n => n
    }
  };

let success = (value, s) => Node({value, parseData: makeParseData(~match="", ~rest=s, ())});

let concatMatches = (nodeList) =>
  nodeList |> List.map((node) => node.parseData.match) |> Array.of_list |> Js_array.joinWith("");

let rec seq = (parsers, s) =>
  switch parsers {
  | [] => success(Lst([]), s)
  | [p, ...ps] =>
    switch (p(s)) {
    | Fail(_) as f => f
    | Node(node) =>
      switch (seq(ps, node.parseData.rest)) {
      | Fail(_) as f => f
      | Node(listNode) =>
        switch listNode.value {
        | Lst(childNodes) =>
          Node({
            value: Lst([node, ...childNodes]),
            parseData: {...listNode.parseData, match: concatMatches([node, ...childNodes])}
          })
        | _ => Js.Exn.raiseTypeError("This should always be a list.")
        }
      }
    }
  };