
let () =
  Lwt_log_core.append_rule "*" Lwt_log_core.Debug

let () =
  Lwt_log.default :=
    Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()
  
let () =
  let server = new Irc.Server.Simple.server Irc.Server.Simple.default_config in
  server#start ()
