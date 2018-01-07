module DayThree: Defs.AdventCalculator = {
  let abs = i =>
    switch (i < 0) {
    | true => i * (-1)
    | _ => i
    };
  let makeCenters = (start: int, layerSize: int) =>
    Array.mapi((i, _b) => start + (i + 1) * (layerSize / 4), Array.make(4, 0));
  let distanceFromCenterOfWall = (centers, wallSize, square) =>
    centers
    |> Array.fold_left(
         (dist, center) =>
           switch (square - center |> abs < dist) {
           | true => square - center |> abs
           | _ => dist
           },
         wallSize
       );
  let getSpiralDimensions = (square: int) : (int, int, int) => {
    let gridSize = ref(1);
    let layerNum = ref(0);
    let wallInc = ref(1);
    let layerSize = ref(1);
    while (gridSize^ < square) {
      layerSize := wallInc^ * 4 + 4;
      gridSize := gridSize^ + layerSize^;
      wallInc := wallInc^ + 2;
      layerNum := layerNum^ + 1;
    };
    (gridSize^, layerSize^, layerNum^);
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
    switch advent {
    | _ =>
      input |> Int32.of_string |> Int32.to_int |> distanceFromCenterOfSpiral
    };
};