module DayThree: Defs.AdventCalculator = {
  let abs = int =>
    switch (int < 0) {
    | true => int * (-1)
    | _ => int
    };
  let makeSpiral = (square: int) => {
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
    Js.log(gridSize^);
    Js.log(layerSize^ / 4);
    let start = gridSize^ - layerSize^ - layerSize^ / 8;
    let centers =
      Array.mapi(
        (i, _b) => start + (i + 1) * (layerSize^ / 4),
        Array.make(4, 0)
      );
    centers
    |> Array.fold_left(
         (dist, center) =>
           switch (abs(square - center) < dist) {
           | true => square - center |> abs
           | _ => dist
           },
         layerSize^ / 4
       )
    |> (dist => dist + layerNum^);
  };
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch advent {
    | _ => input |> Int32.of_string |> Int32.to_int |> makeSpiral
    };
};