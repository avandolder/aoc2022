let input = read_line ()
let count_uniq l = List.length (List.sort_uniq compare l)
let explode s = List.init (String.length s) (String.get s)

let start_of_packet p =
  let rec find i (_ :: prev) = function
    | x :: xs when count_uniq (x :: prev) == 14 -> i + 1
    | x :: xs -> find (i + 1) (prev @ [ x ]) xs
    | [] -> i
  in
  let l = List.init 14 (List.nth p) in
  let p = List.filteri (fun i _ -> i >= 14) p in
  find 14 l p

let () = start_of_packet (explode input) |> print_int
