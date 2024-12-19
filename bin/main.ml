open A6.Set

(** [time f x] is the number of wall-clock seconds it takes to run [f x]. *)
let time f x =
  let start = Unix.gettimeofday () in
  let _ = f x in
  let stop = Unix.gettimeofday () in
  stop -. start

(** [median lst] is the median of [lst], assuming [lst] is sorted. *)
let median lst =
  let n = List.length lst in
  if n mod 2 = 1 then List.nth lst (n / 2)
  else (List.nth lst ((n / 2) - 1) +. List.nth lst (n / 2)) /. 2.0
(* gotta handle case when odd *)
(* Was inspired by the reponses on how to implement median in Python.
   https://stackoverflow.com/questions/24101524/finding-median-of-list-in-python *)

(** [trial_times n trials] is a list of measured times to insert [n] distinct
    elements [trials] times and returns the resulting times of each trials. *)
let trial_times n trials =
  List.init trials (fun _ ->
      let elements = List.init n (fun x -> x) in
      time (fun () -> List.fold_left (fun s x -> insert x s) empty elements) ())

(** [median_time_for_n n trials] is the median time of thee list of trial times. *)
let median_time_for_n n trials =
  trial_times n trials |> List.sort compare |> median

(** [populate_list start stop step] is a list of integers from [start] to [stop]
    with a step size of [step]. *)
let rec populate_list start stop step =
  if start > stop then [] else start :: populate_list (start + step) stop step

(** [log10 x] is the base-10 logarithm of [x]. *)
let log10 x = log x /. log 10.

(** [n_log_n n] is the product of [n] and log base 10 of [n]. *)
let n_log_n n = float_of_int n *. log10 (float_of_int n)

(** [()] is the main driver function. *)
let () =
  let trials = 5 in
  let n_values = populate_list 500_000 20_500_000 5_00_000 in
  Printf.printf "N, N log N, Time\n";
  List.iter
    (fun n ->
      let median_time = median_time_for_n n trials in
      Printf.printf "%d,%g,%g\n%!" n (n_log_n n) median_time)
    n_values
