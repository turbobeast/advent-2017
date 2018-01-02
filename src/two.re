module DayTwo: Defs.AdventCalculator = {
  let make2dArray = input =>
    input
    |> Js.String.split("\n")
    |> Array.map(row =>
         row
         |> Js.String.split("\t")
         |> Array.map(b => b |> Int32.of_string |> Int32.to_int)
       );
  let findSmallest = ray =>
    ray
    |> Array.fold_left(
         (a, b) =>
           switch (b < a) {
           | true => b
           | _ => a
           },
         Int32.max_int |> Int32.to_int
       );
  let findLargest = ray =>
    ray
    |> Array.fold_left(
         (a, b) =>
           switch (b > a) {
           | true => b
           | _ => a
           },
         Int32.min_int |> Int32.to_int
       );
  let sumDiff = ray =>
    ray
    |> Array.fold_left(
         (acc, row) => acc + (findLargest(row) - findSmallest(row)),
         0
       );
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch advent {
    | _ => input |> make2dArray |> sumDiff
    };
};