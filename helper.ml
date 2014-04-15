open Unix
open Printf
open Thread

let receive_socket cin =
  try
    let str = input_line cin in
    printf "> %s\n%!" str;
  with e -> failwith (Printexc.to_string e)

let receive_err s e =
  eprintf "Caught: %s\n%!" (Printexc.to_string e);
  close s;
  Pervasives.exit 1

let receive s =
  let cin = in_channel_of_descr s in
  printf "- In bound channel connected\n%!";
  let rec loop () =
    try
      receive_socket cin;
      loop ();
    with e -> receive_err s e in
  loop ()

let read_input () =
  try read_line ()
  with e -> failwith (Printexc.to_string e)

let send cout str =
  try fprintf cout "%s\r\n%!" str
  with e -> failwith (Printexc.to_string e)

let send_err s e =
  eprintf "Caught: %s\n%!" (Printexc.to_string e);
  close s;
  Pervasives.exit 1

let read_and_send s =
  let cout = out_channel_of_descr s in
  let _  = printf "- Out bound channel connected\n%!" in
  let rec loop () =
    try
      let str = read_input () in
      send cout str;
      loop ()
    with e -> send_err s e in
  let _ = loop () in
  ()
