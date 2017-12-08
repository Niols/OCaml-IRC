
let () =
  let server = new Irc.Server.server Irc.Server.default_config in
  server#start ()
