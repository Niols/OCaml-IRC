(******************************************************************************)
(*                                                                            *)
(*   Copyright (C) 2017 Nicolas "Niols" Jeannerod                             *)
(*                                                                            *)
(*   This file is part of OCaml-IRC.                                          *)
(*                                                                            *)
(*   OCaml-IRC is free software: you can redistribute it and/or modify it     *)
(*   under the terms of the GNU Lesser General Public License as published    *)
(*   by the Free Software Foundation, either version 3 of the License, or     *)
(*   (at your option) any later version.                                      *)
(*                                                                            *)
(*   OCaml-IRC is distributed in the hope that it will be useful, but         *)
(*   WITHOUT ANY WARRANTY; without even the implied warranty of               *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the             *)
(*   GNU Lesser General Public License for more details.                      *)
(*                                                                            *)
(*   You should have received a copy of the GNU Lesser General Public         *)
(*   License along with OCaml-IRC.                                            *)
(*   If not, see <http://www.gnu.org/licenses/>.                              *)
(*                                                                            *)
(******************************************************************************)

open ExtPervasives

let (>>=) = Lwt.bind

let debug = Lwt_log.debug
let debug_f = Lwt_log.debug_f
let info = Lwt_log.info
let info_f = Lwt_log.info_f
let warning = Lwt_log.warning
let warning_f = Lwt_log.warning_f
let error = Lwt_log.error
let error_f = Lwt_log.error_f


type config =
  { server_name : string }

let default_config =
  { server_name = "127.0.0.1" } (*FIXME*)

class server config = object (self)
  inherit Server_Skeleton.skeleton as skeleton

  val database = Server_Database.create ()

  method send_command (conn: Server_Connection.t) cmd : unit Lwt.t =
    Server_Connection.send conn Utils_Message.(make (Utils_Prefix.Servername config.server_name) cmd)

  method send_commands (conn: Server_Connection.t) cmds : unit Lwt.t =
    List.map
      (Utils_Message.make (Utils_Prefix.Servername config.server_name))
      cmds
    |> Server_Connection.send_multiple conn

  method on_open_connection _conn =
    Lwt.return ()

  method on_close_connection conn =
    Server_Database.remove database (Utils_Identity.nick (Server_Connection.identity conn));
    Lwt.return ()

  method on_unexpected_message conn msg =
    warning_f "Unexpected message: %s" (Utils_Message.to_string msg)
    >> Server_Connection.send conn Utils_Message.(make (Utils_Prefix.Servername config.server_name) (Utils_Command.Error (Format.sprintf "unexpected message: %s" (Utils_Message.to_string msg))))

  method welcome (conn: Server_Connection.t) : unit Lwt.t =
    let nick = Utils_Identity.nick (Server_Connection.identity conn) in
    self#send_commands
      conn
      [
        Utils_Command.Rpl (Utils_Reply.Welcome  (nick, Server_Connection.identity conn)) ;
        Utils_Command.Rpl (Utils_Reply.Yourhost (nick, config.server_name, "OCaml-IRC-0.1")) ;
        Utils_Command.Rpl (Utils_Reply.Created  (nick, "FIXME")) ;
        Utils_Command.Rpl (Utils_Reply.Myinfo   (nick, config.server_name, "OCaml-IRC-0.1", "", ""))
      ]

  method on_ping conn source _target =
    (*FIXME: check that target=None?*)
    self#send_command
      conn
      (Utils_Command.Pong (config.server_name, Some source))

  method on_pong _conn _source _target =
    Lwt.return ()

  method on_user conn user _mode _real =
    let identity = Server_Connection.identity conn in
    Server_Connection.set_identity conn (Utils_Identity.set_user identity user);
    if Utils_Identity.is_valid (Server_Connection.identity conn) then
      self#welcome conn
    else
      Lwt.return ()

  method on_nick conn nick =
    let identity = Server_Connection.identity conn in
    try
      Server_Database.nick database conn nick;
      if Utils_Identity.nick_opt identity <> None then
        Server_Connection.send conn (Utils_Message.make (Utils_Prefix.Identity identity) (Utils_Command.Nick nick))
      else
        Lwt.return ()
    with
      Server_Database.NickAlreadyInUse ->
      if (Utils_Identity.nick_opt identity) = None then
        Server_Database.nick database conn (Server_Database.fresh_nick database);
      Server_Connection.send conn (Utils_Message.make (Utils_Prefix.Servername config.server_name) (Utils_Command.Err (Utils_Error.NicknameInUse nick)))

  method on_join (conn: Server_Connection.t) (keyed_channels: Utils_Command.keyed_channel list) =
    Lwt_list.iter_s
      (fun (chan, _key) ->
        if Server_Database.is_in_chan database (Utils_Identity.nick (Server_Connection.identity conn)) chan then
          Lwt.return ()
        else
          (
            (*FIXME: handle keys*)
            Server_Database.join database (Utils_Identity.nick (Server_Connection.identity conn)) chan;
            Server_Database.iter_s
              database
              (fun conn' ->
                Server_Connection.send conn' Utils_Message.(make (Utils_Prefix.Identity (Server_Connection.identity conn)) (Utils_Command.Join [(chan,None)])))
              chan
          ))
      keyed_channels

  method on_privmsg conn target message =
    let message = Utils_Message.(make (Utils_Prefix.Identity (Server_Connection.identity conn)) (Utils_Command.Privmsg (target, message))) in
    match target with
    | Utils_Command.Channel c ->
       (*FIXME: check that we can do that*)
       Server_Database.iter_s
         database
         (fun conn' ->
           if Server_Connection.nequal conn conn' then
             Server_Connection.send conn' message
           else
             Lwt.return ())
         c
    | Utils_Command.Nickname n ->
       (
         try
           let conn' = Server_Database.find database n in
           Server_Connection.send conn' message
         with
           Not_found ->
           Server_Connection.send conn Utils_Message.(make (Utils_Prefix.Servername config.server_name) (Utils_Command.Err Utils_Error.NoRecipient))
       )
end
