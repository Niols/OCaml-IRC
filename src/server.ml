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

let warning = Lwt_log_core.warning
let warning_f = Lwt_log_core.warning_f
let error = Lwt_log_core.error
let error_f = Lwt_log_core.error_f

class connection fd sockaddr = object (self)

  val input = Lwt_io.of_fd ~mode:Lwt_io.input fd
  val output = Lwt_io.of_fd ~mode:Lwt_io.output fd
  val identity = ()

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

type event =
  | Message of (connection * Utils.Message.t)
  | NewConnection of connection

exception DropConnection
                   
class virtual skeleton = object (self)
  val nicknames = Hashtbl.create 10
  val channels = Hashtbl.create 10
  val mutable socket = None

  method virtual on_user : Utils.Nickname.t -> Utils.Command.mode -> string -> unit Lwt.t
  method virtual on_nick : Utils.Nickname.t -> unit Lwt.t
  method virtual on_privmsg : unit Lwt.t
  method virtual on_ping : Utils.Command.server -> Utils.Command.server option -> unit Lwt.t
  method virtual on_pong : Utils.Command.server -> Utils.Command.server option -> unit Lwt.t

  method handle_message (conn: connection) (msg: Utils.Message.t) : bool Lwt.t =
    let open Utils in
    let open Command in
    try%lwt
      (
        match msg.Message.command with
        | Nick nick ->
           self#on_nick nick
        | User (user, mode, real) ->
           self#on_user user mode real
        | Ping (source, target) ->
           self#on_ping source target
        | Pong (source, target) ->
           self#on_pong source target
        | _ ->
           warning "That message was unexpected"
           >>= (fun () -> Lwt.fail DropConnection)
      )
      >>= (fun () -> Lwt.return_true)
    with
      DropConnection ->
      Lwt.return_false
               
  method accept () =
    Lwt_unix.accept (unwrap socket)
    >>= fun (fd, sockaddr) ->
    Lwt.return (new connection fd sockaddr)

  method listen (conn: connection) =
    conn#receive ()
    >>= (fun msg -> Lwt.return (conn, msg))

  method loop accepter listeners : unit Lwt.t =
    (* FIXME: add pings *)
    
    (* we run in parallel the accepter and all the listeners *)
    match%lwt ExtLwt.choose_pair accepter (Lwt.nchoose_split listeners) with

    (* first case: the accepter answered *)
    | ExtLwt.First conn ->
       (* in the case, we keep accepting stuff, and we start listening
          on the new connection. FIXME: stop listening if max_clients *)
       self#loop (self#accept ()) ((self#listen conn) :: listeners)

    (* second case: some listener/s answered *)
    | ExtLwt.Second (values, remaining_listeners) ->
       (* then we loop over all the received values (pairs connection
          x Message.t) and we handle the message. we then decide
          whether we want to keep listening on that connection *)
       Lwt_list.fold_left_s
         (fun listeners (conn, msg) ->
           self#handle_message conn msg
           >>= (function
                | true -> Lwt.return ((self#listen conn) :: listeners)
                | false -> Lwt.return listeners))
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
    Lwt_unix.bind (unwrap socket) sockaddr
    >>= (fun () ->
      Lwt_unix.listen (unwrap socket) 512; (*FIXME*)
      self#loop (self#accept ()) [])

  method start_async () =
    Lwt.async self#run

  method start () : unit =
    Lwt_main.run (self#run ());
    assert false
end
                       
