module DayTwo: Defs.AdventCalculator = {
  let make2dArray = input =>
    input
    |> Js.String.split("\n")
    |> Array.map(row =>
         row
         |> Js.String.split("\t")
         |> Array.map(b => b |> Int32.of_string |> Int32.to_int)
       );
  let largestSmallest = row =>
    switch (row |> Array.to_list) {
    | [first, ...rest] =>
      rest |> Array.of_list
      |> Array.fold_left(
           ((largest, smallest), next) =>
             switch (next > largest, next < smallest) {
             | (true, _) => (next, smallest)
             | (_, true) => (largest, next)
             | _ => (largest, smallest)
             },
           (first, first)
         )
    | _ => (0, 0)
    };
  let largestDiff = row =>
    row |> largestSmallest |> ((largest, smallest)) => largest - smallest;
  let sumDiff = ray =>
    ray |> Array.fold_left((acc, row) => acc + (row |> largestDiff), 0);
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch advent {
    | _ => input |> make2dArray |> sumDiff
    };
};