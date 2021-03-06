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

type event =
  | Message of Utils_Message.t
  | Exception of exn

class virtual skeleton = object (self)
  val mutable socket = None

  method virtual on_user : Server_Connection.t -> Utils_Command.user -> Utils_Command.mode -> string -> unit Lwt.t
  method virtual on_nick : Server_Connection.t -> Utils_Nickname.t -> unit Lwt.t
  method virtual on_privmsg : Server_Connection.t -> Utils_Command.target -> string -> unit Lwt.t
  method virtual on_ping : Server_Connection.t -> Utils_Command.server -> Utils_Command.server option -> unit Lwt.t
  method virtual on_pong : Server_Connection.t -> Utils_Command.server -> Utils_Command.server option -> unit Lwt.t
  method virtual on_join : Server_Connection.t -> Utils_Command.keyed_channel list -> unit Lwt.t
               
  method virtual on_unexpected_message : Server_Connection.t -> Utils_Message.t -> unit Lwt.t
  method virtual on_open_connection : Server_Connection.t -> unit Lwt.t
  method virtual on_close_connection : Server_Connection.t -> unit Lwt.t
               
  method handle_message (conn: Server_Connection.t) (msg: Utils_Message.t) : unit Lwt.t =
    let open Utils_Command in
    match msg.Utils_Message.command with
    | Nick nick ->
       self#on_nick conn nick
    | User (user, mode, real) ->
       self#on_user conn user mode real
    | Ping (source, target) ->
       self#on_ping conn source target
    | Pong (source, target) ->
       self#on_pong conn source target
    | Join keyed_channels ->
       self#on_join conn keyed_channels
    | Privmsg (target, message) ->
       self#on_privmsg conn target message
    | _ ->
       self#on_unexpected_message conn msg

  method accept () =
    Lwt_unix.accept (unwrap socket)
    >>= (fun (fd, sockaddr) ->
      let conn = Server_Connection.make fd sockaddr in
      self#on_open_connection conn
      >> Lwt.return conn)

  method listen (conn: Server_Connection.t) =
    try%lwt
      Server_Connection.receive conn
      >>= (fun msg ->
        Lwt.return (conn, Message msg))
    with
      e -> Lwt.return (conn, Exception e)

  method loop accepter listeners : unit Lwt.t =
    (* FIXME: add pings *)

    (* we run in parallel the accepter and all the listeners *)
    match%lwt ExtLwt.choose_pair accepter (Lwt.nchoose_split listeners) with

    (* first case: the accepter answered *)
    | ExtLwt.First conn ->
       (* in the case, we keep accepting stuff, and we start listening
          on the new connection. FIXME: stop listening if max_clients *)
       info_f "New connection: %s" (Utils_Identity.host (Server_Connection.identity conn))
       >> self#loop (self#accept ()) ((self#listen conn) :: listeners)

    (* second case: some listener/s answered *)
    | ExtLwt.Second (values, remaining_listeners) ->
       (* then we loop over all the received values (pairs connection
          x Utils_Message.t) and we handle the message. we then decide
          whether we want to keep listening on that connection *)
       Lwt_list.fold_left_s
         (fun listeners (conn, ev) ->
           match ev with
           | Message msg ->
              self#handle_message conn msg
              >> Lwt.return ((self#listen conn) :: listeners)
           | Exception End_of_file ->
              info_f "Closed connection: %s" (Utils_Identity.host (Server_Connection.identity conn))
              >> self#on_close_connection conn
              >> Lwt.return listeners
           | Exception (Utils_Error.Exception err) ->
              Server_Connection.send conn Utils_Message.(make
                                         (Utils_Prefix.Servername "127.0.0.1") (*FIXME*)
                                         (Utils_Command.Err err))
              >> Lwt.return ((self#listen conn) :: listeners)
           | Exception e ->
              warning_f
                ~exn:e
                "[%s] Dropping connection because of unexpected exception" (Utils_Identity.host (Server_Connection.identity conn))
              >> self#on_close_connection conn
              >> Lwt.return listeners)
         remaining_listeners
         values
       >>= (fun listeners ->
        self#loop accepter listeners)

  method run () : unit Lwt.t =
    let sockaddr =
      Lwt_unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", (*FIXME*)
                          6667 (*FIXME*))
    in
    socket <- Some (Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Lwt_unix.SOCK_STREAM 0);
    info_f "Binding socket..."
    >> (
      try%lwt
        Lwt_unix.bind (unwrap socket) sockaddr
      with
        Unix.Unix_error(Unix.EADDRINUSE, _, _) as e ->
        Lwt_log.fatal "Cannot bind to socket: address already in use"
        >> raise e)
    >> (Lwt_unix.listen (unwrap socket) 512; (*FIXME*)
        info_f "Starting accepting loop")
    >> self#loop (self#accept ()) []

  method start_async () =
    Lwt.async self#run

  method start () : unit =
    Lwt_main.run (self#run ());
    assert false
end
