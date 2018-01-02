let convertToIntArray = rawInput =>
  rawInput
  |> Js.String.split("")
  |> Array.map(b => Int32.of_string(b) |> Int32.to_int);

type advent =
  | PartOne
  | PartTwo;

module type AdventCalculator = {
  let calculate: (~advent: advent, ~input: string) => int;
};