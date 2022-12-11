type shape = Rock | Paper | Scissors

let score_shape = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let beats = function
  | Rock, Scissors -> true
  | Scissors, Paper -> true
  | Paper, Rock -> true
  | _ -> false

let score_outcome (opp, you) =
  if beats (opp, you) then 0 else if beats (you, opp) then 6 else 3

let score_round (opp, you) = score_shape you + score_outcome (opp, you)

let read_round () =
  let (opp :: you :: _) = String.split_on_char ' ' (read_line ()) in
  ( (match opp with "A" -> Rock | "B" -> Paper | "C" -> Scissors),
    match you with "X" -> Rock | "Y" -> Paper | "Z" -> Scissors )

let read_rounds () =
  let rec go rds =
    try
      go (read_round () :: rds)
    with
      End_of_file -> rds
  in go []

let () =
  read_rounds ()
  |> List.map score_round
  |> List.fold_left ( + ) 0
  |> print_int
