open Unix
open Printf
open Thread

let receive s =
  let cin = in_channel_of_descr s in
  let rec read c =
    let (loop, str) = try (true, input_line c) with
                        End_of_file -> (false, "Remote connection ended")
    in
    match (loop, str) with
      (true, str) -> printf "> %s\n%!" str; read c
    | (false, str) -> printf "> %s\n%!" str; close s; Pervasives.exit 0
  in
  read cin

let read_and_send s =
  let cout = out_channel_of_descr s in
  let shutdown () = shutdown s SHUTDOWN_ALL in
  let rec read () =
    let send str =
      let status = try fprintf cout "%s\r\n%!" str;
                       true with
                     Sys_error "Bad file descriptor" ->
                     printf "Connection closed.\n%!";
                     false
      in
      match status with
      true -> read ()
      | false -> ()
    in
    let (loop, input) = try (true, read_line ()) with
                  End_of_file -> (false, "")
    in
    match (loop, input) with
      (true, str) -> send input
    | (false, str) -> flush cout; close s; shutdown ()
  in
  read ()

let _ =
  let addr = try Sys.argv.(1) with
               Invalid_argument "index out of bounds" ->
               printf "Using default IP address\n%!";
               "127.0.0.1"
  in
  let port = try int_of_string Sys.argv.(2) with
               Invalid_argument "index out of bounds" ->
               printf "Using default port, 15000\n%!";
               15000
  in
  let _ = printf "Connecting to %s:%d.\n%!" addr port in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_KEEPALIVE true;
  connect sock (ADDR_INET (inet_addr_of_string addr, port));
  let _ = Thread.create receive sock in
  read_and_send sock