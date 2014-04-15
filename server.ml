open Unix
open Printf
open Thread
open Helper

let sock = socket PF_INET SOCK_STREAM 0

let get_addr addr = match addr with
      ADDR_UNIX addr -> addr
    | ADDR_INET (addr, port) -> (string_of_inet_addr addr) ^ ":" ^ (string_of_int port)

let rec search_and_bind addr port =
  try
    let addr = ADDR_INET (inet_addr_of_string "0.0.0.0", port) in
    bind sock addr;
    printf "- Listening on %s\n%!" (string_of_int port)
  with
    e -> search_and_bind addr (port + 1)

let rec accept_and_reject sock =
  let (s, addr) = accept sock in
  close s;
  accept_and_reject sock

let rec accept_loop sock =
  let (s, addr) = accept sock in
  printf "- Connected to %s\n%!" (get_addr addr);
  try
    let _ = Thread.create accept_and_reject sock in
    let _ = Thread.create receive s in
    read_and_send s;
  with
    e -> eprintf "Caught: %s\n%!" (Printexc.to_string e)

let _ =
  setsockopt sock SO_REUSEADDR true;
  let starting_port = 1024 in
  try
    search_and_bind "0.0.0.0" starting_port;
    listen sock 5;
    accept_loop sock
  with
    e -> eprintf "Caught: %s\n%!" (Printexc.to_string e);
