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

let contains (a, b) (c, d) = a <= c && d <= b

let () =
  List.map parse (readlines ())
  |> List.filter (fun (x, y) -> contains x y || contains y x)
  |> List.length
  |> print_int