open Unix
open Printf
open Thread

let receive s =
  let cin = in_channel_of_descr s in
  printf "- In bound channel connected\n%!";
  let rec read c =
    let (loop, str) = try (true, input_line c) with
                        End_of_file -> (false, "Remote connection ended")
                      | Sys_error "Socket is not connected" -> (false, "Socket not connected")
    in
    match (loop, str) with
      (true, str) -> printf "> %s\n%!" str; read c
    | (false, str) -> printf "- %s\n%!" str; close s
  in
  read cin

let read_and_send s =
  let cout = out_channel_of_descr s in
  printf "- Out bound channel connected\n%!";
  let rec read () =
    let send str =
      let status = try fprintf cout "%s\r\n%!" str; true with
                     Sys_error "Bad file descriptor" -> printf "- Connection closed.\n%!";
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

let rec accept_loop sock =
  let (s, a) = accept sock in
  let get_addr addr = match addr with
      ADDR_UNIX addr -> addr
    | ADDR_INET (addr, port) -> (string_of_inet_addr addr) ^ ":" ^ (string_of_int port) in
  printf "- Connected to %s\n%!" (get_addr a);
  let _ = Thread.create receive s in
  read_and_send s;
  accept_loop sock

let _ =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let port = 15000 in
  printf "- Listening on %s\n%!" (string_of_int port);
  bind sock (ADDR_INET (inet_addr_of_string "0.0.0.0", port));
  listen sock 5;
  accept_loop sock
