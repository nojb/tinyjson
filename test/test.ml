let () =
  let rec loop () =
    let lexbuf = Lexing.from_channel stdin in
    Format.printf "@[%a@]@." Json.pp (Json.value lexbuf);
    loop ()
  in
  loop ()
