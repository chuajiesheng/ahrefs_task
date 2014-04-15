open Unix
open Printf
open Thread

let receive s =
  let cin = in_channel_of_descr s in
  printf "- In bound channel connected\n%!";
  let rec read c =
    let (loop, str) = try (true, input_line c) with
                        End_of_file -> (false, "Remote connection ended")
                      | Sys_error _ -> (false, "Socket not connected")
    in
    match (loop, str) with
      (true, str) -> printf "> %s\n%!" str; read c
    | (false, str) -> printf "- %s\n%!" str; close s
  in
  read cin

let send cout str =
  try fprintf cout "%s\r\n%!" str
  with Sys_error e -> failwith e

let read_and_send s =
  let cout = out_channel_of_descr s in
  printf "- Out bound channel connected\n%!";
  let rec read () =
    let send str =
      let status = try fprintf cout "%s\r\n%!" str; true with
                     Sys_error _ -> printf "- Connection closed.\n%!";
                                    false
      in
      match status with
        true -> read ()
      | false -> printf "- Failed to send content.\n%!"; ()
    in
    let (loop, input) = try (true, read_line ()) with
                          End_of_file -> (false, "")
    in
    match (loop, input) with
      (true, str) -> send input
    | (false, str) -> flush cout; close s
  in
  read ()
