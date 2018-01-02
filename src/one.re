module DayOne: Defs.AdventCalculator = {
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
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch advent {
    | PartTwo =>
      input
      |> Defs.convertToIntArray
      |> (ray => sumMatches(~inc=Array.length(ray) / 2, ~ray))
    | _ => input |> Defs.convertToIntArray |> (ray => sumMatches(~inc=1, ~ray))
    };
};