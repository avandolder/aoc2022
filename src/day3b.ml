module CharSet = Set.Make(Char)

let get_dup () =
  [read_line (); read_line (); read_line ()]
  |> List.map (fun x -> CharSet.of_seq (String.to_seq x))
  |> function [x; y; z] ->
    CharSet.(min_elt (inter (inter x y) z))

let priority x =
  let open Char in
  let x = code x in
  if (code 'a') <= x && x <= (code 'z') then x - (code 'a') + 1
  else x - (code 'A') + 27

let () =
  let rec loop total =
    try
      loop (total + priority (get_dup ()))
    with
      End_of_file -> total in
  print_int (loop 0)
