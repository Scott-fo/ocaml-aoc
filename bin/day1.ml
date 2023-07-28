open Base

let read_lines file =
  In_channel.with_open_text file In_channel.input_all |> String.split ~on:'\n'

let max_of_list input =
  match input with
  | [] -> None
  | h :: t -> Some (List.fold t ~init:h ~f:Base.Int.max)

let get_top_three x top_three =
  List.take (List.sort ~compare:Int.descending (x :: top_three)) 3

let max_three_sums sums =
  List.fold ~init:[] ~f:(fun top_three x -> get_top_three x top_three) sums

let sum_of_group group =
  group
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Int.of_string |> List.fold ~init:0 ~f:( + )

let sum_groups file =
  let lines = read_lines file in
  lines
  |> List.group ~break:(fun _ s -> String.is_empty s)
  |> List.map ~f:sum_of_group

let () =
  let sums = sum_groups "./input/day1.txt" in

  (* Part 1 *)
  Stdio.print_endline "Largest calories carried by elf: ";
  (match max_of_list sums with
  | None -> Stdio.print_endline "No input"
  | Some m -> Stdio.printf "%d\n\n" m);

  (* Part 2 *)
  Stdio.print_endline "Sum of top 3 calories carried by elves: ";
  let r = max_three_sums sums in
  Stdio.printf "%d\n\n" (List.fold ~init:0 ~f:( + ) r)
