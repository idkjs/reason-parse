type value = [ | `String(string) | `Integer(int) | `Float(float) | `List(list(value))];

let join = (joiner, list) => list |> Array.of_list |> Js_array.joinWith(joiner);

let stringOfString = (`String(s)) => s;

let stringOfInteger = (`Integer(n)) => string_of_int(n);

let stringOfFloat = (`Float(x)) => string_of_float(x);

let rec stringOfList = (`List(values)) =>
  values
  |> List.map(
       (value) =>
         switch value {
         | `String(_) as s => stringOfString(s)
         | `Integer(_) as n => stringOfInteger(n)
         | `Float(_) as x => stringOfFloat(x)
         | `List(_) as moreValues => stringOfList(moreValues)
         }
     )
  |> join(", ");

type matchRecord = {
  value,
  match: string,
  remainder: string,
  parserName: option(string)
};