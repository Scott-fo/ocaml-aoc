open Base

type move = Rock | Paper | Scissors
type result = Win | Lose | Draw

let get_moves (o, y) =
  match (o, y) with
  | "A", "X" -> (Rock, Rock)
  | "A", "Y" -> (Rock, Paper)
  | "A", "Z" -> (Rock, Scissors)
  | "B", "X" -> (Paper, Rock)
  | "B", "Y" -> (Paper, Paper)
  | "B", "Z" -> (Paper, Scissors)
  | "C", "X" -> (Scissors, Rock)
  | "C", "Y" -> (Scissors, Paper)
  | "C", "Z" -> (Scissors, Scissors)
  | _ -> failwith "Unexpected move"

let get_result yours opponents =
  match (yours, opponents) with
  | Rock, Paper -> Lose
  | Rock, Scissors -> Win
  | Rock, Rock -> Draw
  | Paper, Scissors -> Lose
  | Paper, Rock -> Win
  | Paper, Paper -> Draw
  | Scissors, Rock -> Lose
  | Scissors, Paper -> Win
  | Scissors, Scissors -> Draw

let get_move_score m = match m with Rock -> 1 | Paper -> 2 | Scissors -> 3
let get_result_score r = match r with Win -> 6 | Lose -> 0 | Draw -> 3

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> String.split_lines

let play_round line =
  match String.split line ~on:' ' with
  | [ o; y ] ->
      let opponent_move, your_move = get_moves (o, y) in
      let result = get_result your_move opponent_move in
      let result_score = get_result_score result in
      let move_score = get_move_score your_move in
      result_score + move_score
  | _ -> failwith "Invalid input line"

let () =
  let lines = read_lines "./input/day2.txt" in
  let scores = List.map lines ~f:play_round in
  let total_score = List.fold ~init:0 ~f:( + ) scores in
  Stdio.printf "Total score: %d\n" total_score
