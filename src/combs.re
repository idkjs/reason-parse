open Node;

let map = (f, p, s) =>
  switch (p(s)) {
  | Fail_(_) as x => x
  | Success(value, parseData) => Success(f(value), parseData)
  };

let rec alt = (parsers, s) =>
  switch parsers {
  | [] => Fail_("Alt failed. (Did you pass it an empty list?)")
  | [p, ...ps] =>
    switch (p(s)) {
    | Fail_(_) => alt(ps, s)
    | Success(_) as s => s
    }
  };

let success = (value, s) => Success(value, makeParseData(~match="", ~rest=s, ()));

let rec seq = (parsers, s) =>
  switch parsers {
  | [] => success(Lst([]), s)
  | [p, ...ps] =>
    switch (p(s)) {
    | Fail_(_) as f => f
    | Success(value, parseData) =>
      switch (seq(ps, parseData.rest)) {
      | Fail_(_) as f => f
      | Success(valueList, moreParseData) =>
        switch valueList {
        | Lst(values) =>
          Success(
            Lst([value, ...values]),
            {...moreParseData, match: parseData.match ++ moreParseData.match}
          )
        | _ => Js.Exn.raiseTypeError("This should always be a list.")
        }
      }
    }
  };