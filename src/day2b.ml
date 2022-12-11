type shape = Rock | Paper | Scissors
type outcome = Lose | Draw | Win

let score_shape = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let needed_shape = function
    (Rock, Lose) -> Scissors
  | (Rock, Win) -> Paper
  | (Scissors, Lose) -> Paper
  | (Scissors, Win) -> Rock
  | (Paper, Lose) -> Rock
  | (Paper, Win) -> Scissors
  | (shape, Draw) -> shape 

let score_outcome = function
  | Lose -> 0
  | Draw -> 3
  | Win -> 6

let score_round (opp, you) = score_shape (needed_shape (opp, you)) + score_outcome you

let read_round () =
  let (opp :: you :: _) = String.split_on_char ' ' (read_line ()) in
  ( (match opp with "A" -> Rock | "B" -> Paper | "C" -> Scissors),
    match you with "X" -> Lose | "Y" -> Draw | "Z" -> Win )

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
