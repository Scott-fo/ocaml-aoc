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

let three_rucksacks ~f list =
  let rec aux acc f = function
    | a :: b :: c :: tl -> aux (acc + f a b c) f tl
    | _ -> acc
  in
  aux 0 f list

let check_three_rucksacks a b c =
  let set_a = Items.of_string a in
  let set_b = Items.of_string c in
  let set_c = Items.of_string b in
  Items.inter set_a set_b |> Items.inter set_c |> Items.choose
  |> Score.get_score

let () =
  (* Part 1 *)
  read_lines "./input/day3.txt"
  |> List.map ~f:score_rucksack |> List.fold ~init:0 ~f:( + )
  |> Stdio.printf "Part 1 Total score: %d\n";

  (* Part 2 *)
  read_lines "./input/day3.txt"
  |> three_rucksacks ~f:check_three_rucksacks
  |> Stdio.printf "Part 2 Total score: %d\n"
