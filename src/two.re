module DayTwo: Defs.AdventCalculator = {
  let make2dArray = input =>
    input
    |> Js.String.split("\n")
    |> Array.map(row =>
         row
         |> Js.String.split("\t")
         |> Array.map(b => b |> Int32.of_string |> Int32.to_int)
       );
  let sortAndReturn = (row: array(int)) => {
    Array.sort(
      (a, b) =>
        switch (a == b, a < b) {
        | (true, _) => 0
        | (_, true) => 1
        | _ => (-1)
        },
      row
    );
    row;
  };
  let sortNestedRows = rows => rows |> Array.map(sortAndReturn);
  /* let evenDivisors = row =>  */
  let largestSmallest = row =>
    switch (row |> Array.to_list) {
    | [first, ...rest] =>
      rest
      |> Array.of_list
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
  let evenDivisorProduct = row => {
    let i = ref(-1);
    let product =
      row
      |> Array.fold_left(
           (product, firstVal) => {
             i := i^ + 1;
             let stop = (row |> Array.length) - (i^ + 1);
             let matchers = Array.sub(row, i^ + 1, stop);
             let match =
               matchers
               |> Array.fold_left(
                    (match, nextVal) =>
                      switch (firstVal mod nextVal == 0) {
                      | true => Some(nextVal)
                      | _ => match
                      },
                    None
                  );
             switch match {
             | Some(matchVal) => Some(firstVal / matchVal)
             | _ => product
             };
           },
           None
         );
    switch product {
    | Some(i) => i
    | None => 0
    };
  };
  let largestDiff = row =>
    row |> largestSmallest |> (((largest, smallest)) => largest - smallest);
  let sumDiff = ray =>
    ray |> Array.fold_left((acc, row) => acc + (row |> largestDiff), 0);
  let sumProducts = ray =>
    ray |> Array.fold_left((acc, row) => acc + (row |> evenDivisorProduct), 0);
  let calculate = (~advent: Defs.advent, ~input: string) =>
    switch advent {
    | PartTwo => input |> make2dArray |> sortNestedRows |> sumProducts
    | _ => input |> make2dArray |> sumDiff
    };
};