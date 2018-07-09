module DayThree: Defs.AdventCalculator = {
  let abs = i =>
    switch (i < 0) {
    | true => i * (-1)
    | _ => i
    };
  let makeCenters = (start: int, layerSize: int) =>
    Array.mapi(
      (i, _b) => start + (i + 1) * (layerSize / 4),
      Array.make(4, 0),
    );
  let distanceFromCenterOfWall = (centers, wallSize, square) =>
    centers
    |> Array.fold_left(
         (dist, center) =>
           switch (square - center |> abs < dist) {
           | true => square - center |> abs
           | _ => dist
           },
         wallSize,
       );
  let getSpiralDimensions = (square: int) : (int, int, int) => {
    let rec buildNextLayer = (gridSize, wallInc, layerNum, layerSize) =>
      switch (gridSize < square) {
      | true =>
        wallInc
        * 4
        + 4
        |> (
          ls => buildNextLayer(gridSize + ls, wallInc + 2, layerNum + 1, ls)
        )
      | _ => (gridSize, layerSize, layerNum)
      };
    buildNextLayer(1, 1, 0, 1);
  };
  let distanceFromCenterOfSpiral = (square: int) => {
    let (gridSize, layerSize, layerNum) = getSpiralDimensions(square);
    let start = gridSize - layerSize - layerSize / 8;
    makeCenters(start, layerSize)
    |> (
      centers =>
        distanceFromCenterOfWall(centers, layerSize / 4, square)
        |> (dist => dist + layerNum)
    );
  };
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch (advent) {
    | _ =>
      input |> Int32.of_string |> Int32.to_int |> distanceFromCenterOfSpiral
    };
};