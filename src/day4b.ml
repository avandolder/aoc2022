let readlines () =
  let rec go lines =
    try
      go (read_line () :: lines)
    with
      End_of_file -> lines
  in go []

let parse line =
  let pairs = String.split_on_char ',' line in
  let ((a :: b :: _) :: (c :: d :: _) :: _) = List.map (fun pair ->
    String.split_on_char '-' pair
    |> List.map int_of_string
  ) pairs in (a, b), (c, d)

let overlap (a, b) (c, d) =
  (a <= c && b >= c) ||
  (a <= d && b >= d) ||
  (a >= c && b <= d)

let () =
  List.map parse (readlines ())
  |> List.filter (fun (x, y) -> overlap x y)
  |> List.length
  |> print_int