type advent =
  | PartOne
  | PartTwo;

module type AdventCalculator = {
  let calculate: (~advent: advent, ~input: string) => int;
};