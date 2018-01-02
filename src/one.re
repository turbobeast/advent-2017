include Defs;

module DayOne: AdventCalculator = {
  type advent =
    | PartOne
    | PartTwo;
  let convertToIntArray = rawInput =>
    rawInput
    |> Js.String.split("")
    |> Array.map(b => Int32.of_string(b) |> Int32.to_int);
  let sumMatches = (~inc, ~ray) => {
    let i = ref(-1);
    ray
    |> Array.fold_left(
         (sum, next) => {
           i := i^ + 1;
           let potentialMatch = ray[(i^ + inc) mod Array.length(ray)];
           sum
           + (
             switch (next === potentialMatch) {
             | true => potentialMatch
             | _ => 0
             }
           );
         },
         0
       );
  };
  let calculate = (~advent: advent, ~input: string) =>
    switch advent {
    | PartTwo =>
      input
      |> convertToIntArray
      |> (ray => sumMatches(~inc=Array.length(ray) / 2, ~ray))
    | _ => input |> convertToIntArray |> (ray => sumMatches(~inc=1, ~ray))
    };
};