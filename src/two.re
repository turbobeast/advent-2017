module DayTwo: Defs.AdventCalculator = {
  let make2dArray = input =>
    input
    |> Js.String.split("\n")
    |> Array.map(row =>
         row
         |> Js.String.split("\t")
         |> Array.map(b => b |> Int32.of_string |> Int32.to_int)
       );
  let largestDiff = row =>
    row
    |> Array.fold_left(
         ((largest, smallest), b) =>
           switch (b > largest, b < smallest) {
           | (true, true) => (b, b)
           | (true, _) => (b, smallest)
           | (_, true) => (largest, b)
           | _ => (largest, smallest)
           },
         (Int32.min_int |> Int32.to_int, Int32.max_int |> Int32.to_int)
       )
    |> (((largest, smallest)) => largest - smallest);
  let sumDiff = ray =>
    ray |> Array.fold_left((acc, row) => acc + (row |> largestDiff), 0);
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch advent {
    | _ => input |> make2dArray |> sumDiff
    };
};