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

let info = Lwt_log.info
let info_f = Lwt_log.info_f
let warning = Lwt_log.warning
let warning_f = Lwt_log.warning_f
let error = Lwt_log.error
let error_f = Lwt_log.error_f

class connection fd sockaddr identity = object (self)

  val input = Lwt_io.of_fd ~mode:Lwt_io.input fd
  val output = Lwt_io.of_fd ~mode:Lwt_io.output fd
  val hostname = (Unix.getnameinfo sockaddr []).Unix.ni_hostname
  val identity = identity

  method hostname = hostname
               
  method send message =
    Lwt_io.write output (Utils.Message.to_string ~crlf:true message)

  method send_multiple messages =
    Lwt_list.iter_s (self#send) messages

  method receive () =
    Lwt_io.read_line input
    >>= (fun line ->
      try%lwt
        Lwt.return (Utils.Message.from_string line)
      with
      | End_of_file ->
         raise End_of_file
      | Lwt_io.Channel_closed descr ->
         warning_f "Lwt_io.Channel_closed(%s)" descr
         >>= self#receive
      | Unix.Unix_error (error, fname, fparam) ->
         error_f "Unix_error(%s, %s, %s)" (Unix.error_message error) fname fparam
         >>= self#receive
      | _ ->
         error "unexpected error"
         >>= self#receive
    )
end

exception DropConnection

type event =
  | Message of Utils.Message.t
  | Exception of exn
                          
class virtual skeleton = object (self)
  val mutable socket = None

  method virtual on_user : connection -> Utils.Nickname.t -> Utils.Command.mode -> string -> unit Lwt.t
  method virtual on_nick : connection -> Utils.Nickname.t -> unit Lwt.t
  method virtual on_privmsg : connection -> unit Lwt.t
  method virtual on_ping : connection -> Utils.Command.server -> Utils.Command.server option -> unit Lwt.t
  method virtual on_pong : connection -> Utils.Command.server -> Utils.Command.server option -> unit Lwt.t

  method handle_message (conn: connection) (msg: Utils.Message.t) : unit Lwt.t =
    let open Utils in
    let open Command in
    match msg.Message.command with
    | Nick nick ->
       self#on_nick conn nick
    | User (user, mode, real) ->
       self#on_user conn user mode real
    | Ping (source, target) ->
       self#on_ping conn source target
    | Pong (source, target) ->
       self#on_pong conn source target
    | _ ->
       warning_f "That message was unexpected: %s" (Message.to_string msg)
       >> Lwt.fail DropConnection

  method accept () =
    Lwt_unix.accept (unwrap socket)
    >>= (fun (fd, sockaddr) ->
      new connection fd sockaddr Utils.Identity.{
             nick = Utils.Nickname.of_string "" ;
             user = "" ;
             host = "" }
      |> Lwt.return)

  method listen (conn: connection) =
    try%lwt
      conn#receive ()
      >>= (fun msg ->
        Lwt.return (conn, Message msg))
    with
    | End_of_file -> Lwt.return (conn, Exception DropConnection)
    | _ as e -> Lwt.return (conn, Exception e)

  method loop accepter listeners : unit Lwt.t =
    (* FIXME: add pings *)

    (* we run in parallel the accepter and all the listeners *)
    match%lwt ExtLwt.choose_pair accepter (Lwt.nchoose_split listeners) with

    (* first case: the accepter answered *)
    | ExtLwt.First conn ->
       (* in the case, we keep accepting stuff, and we start listening
          on the new connection. FIXME: stop listening if max_clients *)
       info_f "Accepted connection from %s" conn#hostname
       >> self#loop (self#accept ()) ((self#listen conn) :: listeners)

    (* second case: some listener/s answered *)
    | ExtLwt.Second (values, remaining_listeners) ->
       (* then we loop over all the received values (pairs connection
          x Message.t) and we handle the message. we then decide
          whether we want to keep listening on that connection *)
       Lwt_list.fold_left_s
         (fun listeners (conn, ev) ->
           match ev with
           | Message msg ->
              (
                try%lwt
                  self#handle_message conn msg
                  >> Lwt.return ((self#listen conn) :: listeners)
                with
                  DropConnection ->
                  info_f "Dropping connection with %s" conn#hostname
                  >> Lwt.return listeners
              )
           | Exception DropConnection ->
              info_f "Dropping connection with %s" conn#hostname
              >> Lwt.return listeners
           | Exception e ->
              info_f
                ~exn:e
                "Dropping connection with %s" conn#hostname
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

type config =
  { server_name : string }

let default_config =
  { server_name = "127.0.0.1" } (*FIXME*)

class server config = object (self)
  inherit skeleton as skeleton

  val nicknames = Hashtbl.create 8
  val channels = Hashtbl.create 8

  method on_ping conn source _target =
    (*FIXME: check that target=None?*)
    let open Utils in
    Message.make
      (Message.Prefix.Servername config.server_name)
      (Command.Pong (config.server_name, Some source))
    |> conn#send

  method on_pong _conn _source _target =
    Lwt.return ()

  method on_user _conn _user _mode _real =
    Lwt.return ()

  method on_nick _conn _nick =
    Lwt.return ()

  method on_privmsg _conn =
    Lwt.return ()
end
