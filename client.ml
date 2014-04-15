open Unix
open Printf
open Thread
open Helper

let _ =
  let addr = try Sys.argv.(1) with
               Invalid_argument "index out of bounds" ->
               let addr = "127.0.0.1" in
               printf "- Using default IP address, %s\n%!" addr;
               addr
  in
  let port = try int_of_string Sys.argv.(2) with
               Invalid_argument "index out of bounds" ->
               let port = 15000 in
               printf "- Using default port, %d\n%!" port;
               port
  in
  let _ = printf "- Connecting to %s:%d.\n%!" addr port in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_KEEPALIVE true;
  let start () =
    try
      connect sock (ADDR_INET (inet_addr_of_string addr, port));
      let _ = Thread.create receive sock in
      read_and_send sock;
    with
      Unix_error (msg, _, _) -> printf "- %s\n%!" (error_message msg) in
  start ()
