open Comb;

open RegExp;

open Jest;

open Expect;

let rec compareValues = (v, w) =>
  switch (v, w) {
  | (`String((s: string)), `String((t: string))) => s === t
  | (`List(vs), `List(ws)) => List.for_all2(compareValues, vs, ws)
  | (`List(_), `String(_)) => false
  | (`String(_), `List(_)) => false
  };

let compareResults = (x, y) =>
  switch (x, y) {
  | (`Success(v1, (r1: string)), `Success(v2, (r2: string))) => compareValues(v1, v2) && r1 === r2
  | (`Fail(_), `Fail(_)) => true
  | (`Success(_), `Fail(_)) => false
  | (`Fail(_), `Success(_)) => false
  };

let lBrak = regExp([%re "/^\\[/"]);

let rBrak = regExp([%re "/^\\]/"]);

let () =
  describe(
    "Comb",
    () => {
      test(
        "atLeast",
        () => {
          let result = many(~atLeast=4, letter, "abcd123");
          let correctResult =
            `Success((`List([`String("a"), `String("b"), `String("c"), `String("d")]), "123"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "seq",
        () => {
          let result = seq([letter, digit, digit, digit], "a123**");
          let correctResult =
            `Success((`List([`String("a"), `String("1"), `String("2"), `String("3")]), "**"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "keep",
        () => {
          let result = keep([1, 2], [letter, digit, digit, digit], "a123**");
          let correctResult = `Success((`List([`String("1"), `String("2")]), "**"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "keepFirst",
        () => {
          let result = keepFirst([letter, digit, digit, digit], "a123**");
          let correctResult = `Success((`String("a"), "**"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "keepLast",
        () => {
          let result = keepLast([letter, digit, digit, digit], "a123**");
          let correctResult = `Success((`String("3"), "**"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "between",
        () => {
          let result = between(lBrak, digit, rBrak, "[0]a123**");
          let correctResult = `Success((`String("0"), "a123**"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "nesting: between, keepLast",
        () => {
          let result = between(lBrak, keepLast([letter, digit, digit, digit]), rBrak, "[a123]**");
          let correctResult = `Success((`String("3"), "**"));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      );
      test(
        "sepBy",
        () => {
          let result = sepBy(~separator=regExp([%re "/^,/"]), digit, "0,1,1,2,35811");
          stringOfResult(result) |> Js.log;
          let correctResult =
            `Success((
              `List([`String("0"), `String("1"), `String("1"), `String("2"), `String("3")]),
              "5811"
            ));
          expect(compareResults(result, correctResult)) |> toBe(true)
        }
      )
    }
  );