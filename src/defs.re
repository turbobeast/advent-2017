module type AdventCalculator = {
  type advent =
    | PartOne
    | PartTwo;
  let calculate: (~advent: advent, ~input: string) => int;
};