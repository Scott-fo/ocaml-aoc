open Base

module Segment = struct
  type t = { lb : int; ub : int }

  let of_string s =
    match String.split s ~on:'-' with
    | [ lb; ub ] -> { lb = Int.of_string lb; ub = Int.of_string ub }
    | _ -> failwith "Invalid format"

  let contains s1 s2 =
    (s1.lb <= s2.lb && s1.ub >= s2.ub) || (s2.lb <= s1.lb && s2.ub >= s1.ub)

  let overlap s1 s2 =
    (s1.lb <= s2.lb && s1.ub >= s2.lb) || (s2.lb <= s1.lb && s2.ub >= s1.lb)
end

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> String.split_lines

let check_rule ~rule line =
  match String.split line ~on:',' with
  | [ str1; str2 ] ->
      let s1 = Segment.of_string str1 in
      let s2 = Segment.of_string str2 in
      if rule s1 s2 then 1 else 0
  | _ -> 0

let () =
  let total_contains, total_overlaps =
    read_lines "./input/day4.txt"
    |> List.fold ~init:(0, 0) ~f:(fun (total_contains, total_overlaps) line ->
           ( total_contains + check_rule ~rule:Segment.contains line,
             total_overlaps + check_rule ~rule:Segment.overlap line ))
  in
  Stdio.printf "Part 1 Number of Contains: %d\n" total_contains;
  Stdio.printf "Part 2 Number of Overlaps: %d\n" total_overlaps
