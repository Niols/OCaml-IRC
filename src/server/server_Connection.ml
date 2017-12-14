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

type t =
  { input : Lwt_io.input_channel ;
    output : Lwt_io.output_channel ;
    mutable identity : Utils_Identity.t }

let make fd sockaddr =
  { input = Lwt_io.of_fd ~mode:Lwt_io.input fd ;
    output = Lwt_io.of_fd ~mode:Lwt_io.output fd ;
    identity = Utils_Identity.make_opt None None (Some (Unix.getnameinfo sockaddr []).Unix.ni_hostname) }

let identity c = c.identity
let set_identity c id = c.identity <- id

let send conn message =
  debug_f "[>>> %s] %s" (Utils_Identity.host conn.identity) (Utils_Message.to_string message)
  >> Lwt_io.write conn.output (Utils_Message.to_string ~crlf:true message)

let send_multiple conn messages =
  Lwt_list.iter_s (send conn) messages

let rec receive conn =
  Lwt_io.read_line conn.input
  >>= (fun line ->
    try%lwt
       let msg = Utils_Message.from_string line in
           debug_f "[<<< %s] %s" (Utils_Identity.host conn.identity) (Utils_Message.to_string msg)
           >> Lwt.return msg
           with
           | End_of_file ->
              raise End_of_file
           | Lwt_io.Channel_closed descr ->
              warning_f "Lwt_io.Channel_closed(%s)" descr
              >> raise End_of_file
           | Unix.Unix_error (error, fname, fparam) ->
              error_f "Unix_error(%s, %s, %s)" (Unix.error_message error) fname fparam
              >> receive conn
           | e ->
              error ~exn:e "unexpected error (ignoring)"
              >> receive conn
  )

let equal c1 c2 =
  c1 == c2

let nequal c1 c2 =
  not (equal c1 c2)
