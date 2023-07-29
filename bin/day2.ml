open Base

module Result = struct
  type t = Win | Lose | Draw

  let of_string = function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | s -> failwith (Printf.sprintf "Unexpected result string: %s" s)

  let score = function Lose -> 0 | Draw -> 3 | Win -> 6
end

module Move = struct
  type t = Rock | Paper | Scissors

  let of_string = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | s -> failwith (Printf.sprintf "Unexpected move string: %s" s)

  let get_win = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock
  let get_lose = function Rock -> Scissors | Paper -> Rock | Scissors -> Paper
  let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3
end

module Game = struct
  let get_result (ym : Move.t) (om : Move.t) : Result.t =
    match (ym, om) with
    | Rock, Paper -> Lose
    | Rock, Scissors -> Win
    | Rock, Rock -> Draw
    | Paper, Scissors -> Lose
    | Paper, Rock -> Win
    | Paper, Paper -> Draw
    | Scissors, Rock -> Lose
    | Scissors, Paper -> Win
    | Scissors, Scissors -> Draw
end

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> String.split_lines

let part_2 line =
  let om = String.get line 0 |> Char.to_string |> Move.of_string in
  let res = String.get line 2 |> Char.to_string |> Result.of_string in
  let ym =
    match res with
    | Lose -> Move.get_lose om
    | Draw -> om
    | Win -> Move.get_win om
  in
  Result.score res + Move.score ym

let part_1 line =
  let om = String.get line 0 |> Char.to_string |> Move.of_string in
  let ym = String.get line 2 |> Char.to_string |> Move.of_string in
  let result = Game.get_result ym om in
  Result.score result + Move.score ym

let play_games file f =
  read_lines file |> List.map ~f |> List.fold ~init:0 ~f:( + )

let () =
  (*Part 1*)
  Printf.sprintf "Part 1 Total score: %d" (play_games "./input/day2.txt" part_1)
  |> Stdio.print_endline;

  (*Part 2*)
  Printf.sprintf "Part 2 Total score: %d" (play_games "./input/day2.txt" part_2)
  |> Stdio.print_endline
