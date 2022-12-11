let read_calories () =
  let rec read_elf cals =
    try
      match read_int_opt () with
        None -> cals
      | Some cal -> read_elf (cal :: cals)
    with
      End_of_file -> cals in
  let rec go elves =
    match read_elf [] with
      [] -> elves
    | elf -> go (elf :: elves) in
  go []

let () =
  read_calories ()
  |> List.map (List.fold_left ( + ) 0)
  |> List.sort (compare)
  |> List.rev
  |> function (x :: y :: z :: _) -> x + y + z
  |> print_int