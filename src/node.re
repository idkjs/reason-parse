type parseData = {
  match: string,
  rest: string,
  parserName: option(string)
};

let makeParseData = (~parserName=?, ~match, ~rest, ()) => {match, rest, parserName};

let stringOfParseData = ({match, rest}) => Format.sprintf("Match: %s, Rest: %s", match, rest);

let stringOfParseDataList = (parseDataList) =>
  parseDataList |> List.map(stringOfParseData) |> Array.of_list |> Js_array.joinWith("\n");

type value =
  | Str(string)
  | Int(int)
  | Flt(float)
  | Nil
  | Lst(list(value))
  | Map(list((string, value)));

let rec stringOfValue = (value) =>
  switch value {
  | Str(s) => {|"|} ++ s ++ {|"|}
  | Int(n) => string_of_int(n)
  | Flt(x) => string_of_float(x)
  | Nil => "null"
  | Lst(values) => stringOfValueList(values)
  | Map(_) => Js.Exn.raiseError("I haven't gotten to that yet.")
  }
and stringOfValueList = (values) =>
  "[" ++ (values |> List.map(stringOfValue) |> Array.of_list |> Js_array.joinWith(", ")) ++ "]";

type result =
  | Success(value, parseData)
  | Fail(string);

let stringOfResult = (result) =>
  switch result {
  | Success(value, parseData) =>
    Format.sprintf("%s\n%s", stringOfValue(value), stringOfParseData(parseData))
  | Fail(message) => message
  };