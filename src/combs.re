open Node;

let map = (~f=(x) => x, ~g=(x) => x, p, s) =>
  switch (p(s)) {
  | Fail(_) as x => x
  | Success(value, parseData) => Success(f(value), g(parseData))
  };

let rec alt = (parsers, s) =>
  switch parsers {
  | [] => Fail("Alt failed. (Did you pass it an empty list?)")
  | [p, ...ps] =>
    switch (p(s)) {
    | Fail(_) => alt(ps, s)
    | Success(_) as s => s
    }
  };

let success = (value, s) => Success(value, makeParseData(~match="", ~rest=s, ()));

let fail = (message, _: string) => Fail(message);

/* let extractList = (value) =>
   switch value {
   | Lst(values) => values
   | _ => Js.Exn.raiseError("Don't pass a non-list to extractList!")
   }; */
let flatten = (value) =>
  switch value {
  | Lst(values) =>
    Lst(
      List.fold_right(
        (v, acc) =>
          switch v {
          | Lst(vs) => List.append(acc, vs)
          | _ => [v, ...acc]
          },
        values,
        []
      )
    )
  | _ => value
  };

/* let rec seq = (parsers, s) =>
   switch parsers {
   | [] => success(Lst([]), s)
   | [p, ...ps] =>
     switch (p(s)) {
     | Fail(_) as f => f
     | Success(value, parseData) =>
       switch (seq(ps, parseData.rest)) {
       | Fail(_) as f => f
       | Success(moreValues, moreParseData) =>
         Success(
           flatten(Lst([value, moreValues])),
           {...moreParseData, match: parseData.match ++ moreParseData.match}
         )
       }
     }
   }; */
let rec preSeq = (parsers, s) =>
  switch parsers {
  | [] => ([], [])
  | [p, ...ps] =>
    switch (p(s)) {
    | Fail(_) => ([], [])
    | Success(value, parseData) =>
      let (moreValues, moreParseData) = preSeq(ps, parseData.rest);
      ([value, ...moreValues], [parseData, ...moreParseData])
    }
  };

let seq = (parsers, s) => {
  let (valueList, parseDataList) = preSeq(parsers, s);
  let (m, n) = (List.length(valueList), List.length(parsers));
  if (m < n) {
    Fail(Format.sprintf("Parser %d of %d failed.", m + 1, n))
  } else {
    let match =
      parseDataList |> List.map(({match}) => match) |> Array.of_list |> Js_array.joinWith("");
    let parseData = List.rev(parseDataList) |> List.hd;
    Success(Lst(valueList), {...parseData, match})
  }
};

let maybe = (p, s) =>
  switch (p(s)) {
  | Fail(_) => success(Nil, s)
  | Success(_) as s => s
  };

let rec zeroOrMore = (p, s) =>
  switch (p(s)) {
  | Fail(_) => success(Lst([]), s)
  | Success(value, parseData) =>
    switch (zeroOrMore(p, parseData.rest)) {
    | Fail(_) => Js.Exn.raiseError("zeroOrMore should never fail!")
    | Success(moreValues, moreParseData) =>
      Success(
        flatten(Lst([value, moreValues])),
        {...moreParseData, match: parseData.match ++ moreParseData.match}
      )
    }
  };

let oneOrMore = (p) => map(~f=flatten, seq([p, zeroOrMore(p)]));
/* let nth = (ps) =>  */