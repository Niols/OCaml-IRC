
let () =
  Lwt_log_core.append_rule "*" Lwt_log_core.Debug

let () =
  Lwt_log.default :=
    Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()
  
let () =
  let server = new Irc.Server.server Irc.Server.default_config in
  server#start ()
