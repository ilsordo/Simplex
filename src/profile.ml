let steps = ref false

let debug = ref false

let profile = ref false

let dprintf x =
  if !steps then
    Printf.fprintf stdout x
  else
    Printf.ifprintf stdout x

let stats = Hashtbl.create 5

let register s =
  if !debug then
    try
      let old = Hashtbl.find stats s in
      Hashtbl.replace stats s (old+1)
    with Not_found -> Hashtbl.add stats s 1

let print_stats chan =
  if !debug then
    let open Printf in
    fprintf chan "Stats:\n";
    Hashtbl.iter
      (fun s v -> Printf.fprintf chan "  %s : %d\n" s v)
      stats;
    fprintf chan "\n"

let timers = ref (Unix.gettimeofday(), [])

let time s =
  if !profile then
    let (old, times) = !timers in
    let time = Unix.gettimeofday () in
    timers := (time, (s, time-.old)::times)

let print_times chan =
  if !profile then
    let open Printf in
    fprintf chan "Timers:\n";
    let (_, times) = !timers in
    List.iter
      (fun (n, t) ->
         fprintf chan "  %s : %fs\n" n t)
      (List.rev times);
    fprintf chan "\n"
