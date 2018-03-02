type value = [ | `String(string) | `Integer(int) | `Float(float) | `List(list(value))];

type parseRecord = {
  match: string,
  remainder: string,
  name: string
};

type result = [ | `Success(value, parseRecord) | `Fail(list(string))];

let join = (joiner, list: list(string)) => list |> Array.of_list |> Js_array.joinWith(joiner);

let stringOfString = (`String(s)) => s;

let stringOfInteger = (`Integer(n)) => string_of_int(n);

let stringOfFloat = (`Float(x)) => string_of_float(x);

let rec stringOfValue = (value) =>
  switch value {
  | `String(_) as s => stringOfString(s)
  | `Integer(_) as n => stringOfInteger(n)
  | `Float(_) as x => stringOfFloat(x)
  | `List(_) as moreValues => stringOfList(moreValues)
  }
and stringOfList = (`List(values)) => values |> List.map(stringOfValue) |> join(", ");

let stringOfResult = (result) =>
  switch result {
  | `Success(value, _) => stringOfValue(value)
  | `Fail(messages) => messages |> join("\n")
  };