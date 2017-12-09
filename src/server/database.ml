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

open Utils
open Lwt_log
   
exception NickAlreadyInUse

type t =
  { nick_to_conn  : (Nickname.t, Connection.connection) Hashtbl.t ;
    nick_to_chans : (Nickname.t, Channel.t list) Hashtbl.t ;
    chan_to_nicks : (Channel.t, Nickname.t list) Hashtbl.t }

let create () =
  { nick_to_conn = Hashtbl.create 8 ;
    nick_to_chans = Hashtbl.create 8 ;
    chan_to_nicks = Hashtbl.create 8 }

let add db nick conn =
  Hashtbl.add db.nick_to_conn nick conn;
  Hashtbl.add db.nick_to_chans nick []

let remove db nick =
  Hashtbl.remove db.nick_to_conn nick;
  Hashtbl.remove db.nick_to_chans nick;
  Hashtbl.filter_map_inplace
    (fun _ nicks -> Some (List.filter ((<>) nick) nicks))
    db.chan_to_nicks

let nick db nick nick' =
  if Hashtbl.mem db.nick_to_conn nick' then
    raise NickAlreadyInUse
  else
    (
      let conn = Hashtbl.find db.nick_to_conn nick in
      Hashtbl.remove db.nick_to_conn nick;
      Hashtbl.add db.nick_to_conn nick' conn;
      let chans = Hashtbl.find db.nick_to_chans nick in
      Hashtbl.remove db.nick_to_chans nick;
      Hashtbl.add db.nick_to_chans nick' chans;
      Hashtbl.filter_map_inplace
        (fun _ nicks ->
          Some (nick' :: List.filter ((<>) nick) nicks))
        db.chan_to_nicks
    )

let join db nick chan =
  let chans = Hashtbl.find db.nick_to_chans nick in
  Hashtbl.replace db.nick_to_chans nick (chan :: chans);
  try
    let nicks = Hashtbl.find db.chan_to_nicks chan in
    Hashtbl.replace db.chan_to_nicks chan (nick :: nicks)
  with
    Not_found ->
    Hashtbl.add db.chan_to_nicks chan [nick]

let part db nick chan =
  let chans = Hashtbl.find db.nick_to_chans nick in
  Hashtbl.replace db.nick_to_chans nick (List.filter ((<>) chan) chans);
  try
    let nicks = Hashtbl.find db.chan_to_nicks chan in
    let nicks = List.filter ((<>) nick) nicks in
    if nicks = [] then
      Hashtbl.remove db.chan_to_nicks chan
    else
      Hashtbl.replace db.chan_to_nicks chan nicks
  with
    Not_found -> ()
