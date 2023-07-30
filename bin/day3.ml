open Base

module Score = struct
  let get_score c =
    let code = Stdlib.Char.code c in
    match c with
    | 'A' .. 'Z' -> code - 38
    | 'a' .. 'z' -> code - 96
    | _ -> assert false
end

module Items = struct
  include Stdlib.Set.Make (Char)

  let of_string s = Stdlib.String.to_seq s |> of_seq
end

module Rucksack = struct
  type t = { left : Items.t; right : Items.t }

  let create r =
    let len = String.length r in
    let left = String.sub r ~pos:0 ~len:(len / 2) |> Items.of_string in
    let right = String.sub r ~pos:(len / 2) ~len:(len / 2) |> Items.of_string in
    { left; right }

  let find_duplicate { left; right } = Items.inter left right |> Items.choose
end

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> String.split_lines

let score_rucksack line =
  Rucksack.create line |> Rucksack.find_duplicate |> Score.get_score

let () =
  read_lines "./input/day3.txt"
  |> List.map ~f:score_rucksack |> List.fold ~init:0 ~f:( + )
  |> Stdio.printf "Total score: %d\n"
