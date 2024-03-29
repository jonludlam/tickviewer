(* Tick submission viewer *)
open Lwt.Infix

type submissions = CalendarLib.Calendar.Precise.t list
module TickMap = Map.Make(String)
module StudentMap = Map.Make(String)

let output_filename = "tick_submissions.txt"

let postamble = {|
For reference, the deadlines for the ticks are 1pm on the following dates:

        tick1 : 2019-10-17 (advisory; you may continue to submit after this)
        tick2 : 2019-10-24
        tick3 : 2019-10-31
        tick4 : 2019-11-07
        tick5 : 2020-01-16
|}

let get_all_ticks dir =
    Lwt_unix.opendir dir >>= fun d ->
    Lwt_unix.readdir_n d 10000 >>= fun arr ->
    let l = Array.to_list arr in
    let tick_opts = List.map (fun fname ->
        match Astring.String.cuts ~sep:"+" fname with
        | crsid::tick::time::_ ->
            let time = Astring.String.cuts ~sep:"." time |> List.hd in
            let c = CalendarLib.Printer.Precise_Calendar.from_fstring "%F %T" time in
            Some (crsid,tick,c)
        | _ ->
            None) l in
    let ticks = List.fold_left (fun acc l -> match l with | Some x -> x::acc | None -> acc) [] tick_opts in
    Lwt.return ticks 

let get_submissions dir =
    let add crsid tickname time subs =
        StudentMap.update crsid (fun students_ticks ->
            match students_ticks with
            | None -> Some (TickMap.singleton tickname [time])
            | Some ticks ->
                let newticks = TickMap.update tickname (fun subs ->
                    match subs with
                    | None -> Some [time]
                    | Some times -> Some (time::times)) ticks in
                Some newticks) subs
    in
    get_all_ticks dir >>= fun all ->
    let subs = List.fold_left (fun subs (crsid,tick,time) -> add crsid tick time subs) StudentMap.empty all in
    Lwt.return subs

let print_subs subs =
    StudentMap.iter (fun crsid ticks ->
        Printf.printf "%s\n" crsid;
        TickMap.iter (fun tickname subs ->
            let subs = List.sort CalendarLib.Calendar.Precise.compare subs in
            Printf.printf "   %s: (%d) [%s]\n%!"
                tickname (List.length subs) (String.concat "," (
                    List.map (fun t -> CalendarLib.Printer.Precise_Calendar.to_string t) subs))) ticks) subs

let output_subs subs test =
    Lwt_list.iter_s (fun (crsid,ticks) ->
        let output_dir = Printf.sprintf "/home/caelum/%s" crsid in
        let output_file =
            if test
            then Printf.sprintf "/tmp/%s.txt" crsid
            else Filename.concat output_dir output_filename
        in
        Lwt_io.with_file ~mode:Output output_file (fun ch ->
            Lwt_io.fprintf ch "Tick submissions\n" >>= fun () ->
            Lwt_io.fprintf ch "================\n\n" >>= fun () ->
            Lwt_io.fprintf ch "The following tick submissions have been received. They will be run through the autograder after the tick deadline.  You can run this grading yourself using the “validate” button, and resubmit before the deadline if necessary.\n\n" >>= fun () ->
            let lines = TickMap.fold (fun tickname subs strs ->
                let line =
                    Printf.sprintf "%20s: received %s UTC" tickname
                        (CalendarLib.Printer.Precise_Calendar.to_string (List.hd subs))
                in
                let line' =
                    if List.length subs = 2
                    then Printf.sprintf "%s (1 older submission received)" line
                    else if List.length subs > 2
                        then Printf.sprintf "%s (%d older submissions received)" line (List.length subs - 1)
                        else line
                in
                line'::strs) ticks []
            in
            Lwt_list.iter_s (fun line -> Lwt_io.fprintf ch "%s\n" line) lines >>= fun () ->
            Lwt_io.fprintf ch "\n%s\n" postamble
        ) >>= fun () ->
        Lwt_unix.chown output_file 1000 1000
    ) (StudentMap.bindings subs)

let run =
    get_submissions Sys.argv.(1) >>= fun subs ->
    output_subs subs (Array.length Sys.argv > 2)

let _ =
    Lwt_main.run run
