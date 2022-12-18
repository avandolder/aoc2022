module CharSet = Set.Make(Char)

let get_dup () =
  read_line ()
  |> String.to_seq
  |> Array.of_seq
  |> fun a ->
    let open Array in
    let half = (length a) / 2 in
    let left = sub a 0 half in
    let right = sub a half half in
    let to_set x = to_seq x |> CharSet.of_seq in
    CharSet.(min_elt (inter (to_set left) (to_set right)))

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
