let range n =
  let rec range' = function
    | (l, 0) -> l
    | (l, n') -> range' (0 :: l, n' - 1)
  in
  range' ([], n)

let gridHeight = 3
let gridWidth = 50

let initialGrid = List.map (fun _ -> range gridWidth) (range gridHeight)

let doRect a b grid =
  List.mapi (fun r row ->
    List.mapi (fun c cell ->
      if c < a && r < b then 1
      else cell
    ) row
  ) grid

let rotateCol c by grid =
  List.mapi (fun ir row ->
    List.mapi (fun ic cell ->
      if ic == c then List.nth (List.nth grid ((ir - by) mod gridHeight)) c
      else cell
    ) row
  ) grid

let rotateRow r by grid =
  List.mapi (fun ir row ->
    if ir == r then
      List.mapi (fun ic _ ->
        List.nth row ((ic - by) mod gridWidth)
      ) row
    else row
  ) grid

let () =
  let ic = open_in "./day08/input.txt" in
  let lines =
    let linesRef = ref [] in
    try
      while true; do
        linesRef := input_line ic :: !linesRef
      done;
      !linesRef
    with End_of_file ->
      close_in ic;
      !linesRef
  in
  (* Str.regexp *)
  List.iter print_endline lines;
  flush stdout;
