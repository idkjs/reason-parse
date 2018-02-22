type parseData = {
  match: string,
  rest: string,
  parserName: option(string)
};

let makeParseData = (~parserName=?, ~match, ~rest, ()) => {match, rest, parserName};

let stringOfParseData = ({match, rest}) => Format.sprintf("Match: %s, Rest: %s", match, rest);

let logParseData = (parseData) => Js.log(parseData |> stringOfParseData);

type value =
  | Str(string)
  | Int(int)
  | Flt(float)
  | Nil
  | Lst(list(node))
  | Map(list((string, node)))
and node = {
  value,
  parseData
};

let rec stringOfValue = (value) =>
  switch value {
  | Str(s) => {|"|} ++ s ++ {|"|}
  | Int(n) => string_of_int(n)
  | Flt(x) => string_of_float(x)
  | Nil => "null"
  | Lst(nodes) =>
    "[" ++ (nodes |> List.map(stringOfNode) |> Array.of_list |> Js_array.joinWith("\n")) ++ "]"
  | Map(_) => Js.Exn.raiseError("I haven't gotten to that yet.")
  }
and stringOfNode = ({value, parseData}) =>
  "(" ++ stringOfValue(value) ++ ", " ++ stringOfParseData(parseData) ++ ")";

type nodeOrFail =
  | Node(node)
  | Fail(string);

type parser = string => nodeOrFail;