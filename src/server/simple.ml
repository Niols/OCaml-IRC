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
open Utils
open Connection
open Skeleton

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
  inherit skeleton as skeleton

  val database = Database.create ()

  method send_command (conn: Connection.connection) cmd : unit Lwt.t =
    conn#send Message.(make (Prefix.Servername config.server_name) cmd)

  method send_commands (conn: Connection.connection) cmds : unit Lwt.t =
    List.map
      (Message.make (Message.Prefix.Servername config.server_name))
      cmds
    |> conn#send_multiple

  method on_open_connection _conn =
    Lwt.return ()

  method on_close_connection conn =
    Database.remove database (Identity.nick conn#identity);
    Lwt.return ()

  method on_unexpected_message conn msg =
    warning_f "Unexpected message: %s" (Message.to_string msg)
    >> conn#send Message.(make (Prefix.Servername config.server_name) (Command.Error (Format.sprintf "unexpected message: %s" (Message.to_string msg))))

  method welcome (conn: connection) : unit Lwt.t =
    let nick = Identity.nick conn#identity in
    self#send_commands
      conn
      [
        Command.Rpl_Welcome  (nick, conn#identity) ;
        Command.Rpl_Yourhost (nick, config.server_name, "OCaml-IRC-0.1") ;
        Command.Rpl_Created  (nick, "FIXME") ;
        Command.Rpl_Myinfo   (nick, config.server_name, "OCaml-IRC-0.1", "", "")
      ]

  method on_ping conn source _target =
    (*FIXME: check that target=None?*)
    self#send_command
      conn
      (Command.Pong (config.server_name, Some source))

  method on_pong _conn _source _target =
    Lwt.return ()

  method on_user conn user _mode _real =
    let identity = conn#identity in
    conn#set_identity (Identity.set_user identity user);
    if Identity.is_valid conn#identity then
      self#welcome conn
    else
      Lwt.return ()

  method on_nick conn nick =
    let identity = conn#identity in
    try
      Database.nick database conn nick;
      conn#send (Message.make (Message.Prefix.Identity identity) (Command.Nick nick))
    with
      Database.NickAlreadyInUse ->
      if (Identity.nick_opt identity) = None then
        Database.nick database conn (Database.fresh_nick database);
      conn#send (Message.make (Message.Prefix.Servername config.server_name) (Command.Err_NicknameInUse nick))

  method on_join (conn: Connection.connection) (keyed_channels: Command.keyed_channel list) =
    (*FIXME: tell others that .. just joined their channel*)
    Lwt_list.iter_s
      (fun (chan, _key) ->
        if Database.is_in_chan database (Identity.nick conn#identity) chan then
          Lwt.return ()
        else
          (
            (*FIXME: handle keys*)
            Database.join database (Identity.nick conn#identity) chan;
            Database.iter_s
              database
              (fun conn ->
                conn#send Message.(make (Prefix.Identity conn#identity) (Command.Join [(chan,None)])))
              chan
          ))
      keyed_channels

  method on_privmsg _conn _target _message =
    debug "Got PRIVMSG"
end
