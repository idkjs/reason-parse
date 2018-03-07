type lazyList('a) =
  | Nil
  | Cons('a, unit => lazyList('a));

let rec const = (~length=?, a) =>
  switch length {
  | None => Cons(a, (() => const(a)))
  | Some(n) =>
    if (n === 0) {
      Nil
    } else if (n < 0) {
      Js.Exn.raiseRangeError("List length must be nonnegative.")
    } else {
      Cons(a, (() => const(~length=n - 1, a)))
    }
  };

let (@@) = (x, xs) => Cons(x, () => xs);

let rec posInts = (n) =>
  if (n < 0) {
    Nil
  } else {
    Cons(n, () => posInts(n + 1))
  };

let rec fromList = (lst) =>
  switch lst {
  | [] => Nil
  | [x, ...xs] => Cons(x, (() => fromList(xs)))
  };

let toLazy = fromList;

let rec toList = (n, llst) =>
  switch llst {
  | Nil => []
  | Cons(a, f) => n > 0 ? [a, ...toList(n - 1, f())] : []
  };

posInts(5) |> toList(20) |> Array.of_list |> Js.log;

let ll = 3 @@ 2 @@ 1 @@ 0 @@ posInts(1);

ll |> toList(5) |> Array.of_list |> Js.log;

fromList([2, 4, 6, 8]) |> toList(5) |> Array.of_list |> Js.log;