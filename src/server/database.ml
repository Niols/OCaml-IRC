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
    chan_to_conns : (Channel.t, Connection.connection list) Hashtbl.t }

let fpf = Format.fprintf

let pp_debug ppf db =
  fpf ppf "Nicknames:";
  Hashtbl.iter
    (fun nick _ ->
      fpf ppf " %a" Nickname.pp_print nick)
    db.nick_to_conn;
  fpf ppf "\n\nChannels per nick:\n";
  Hashtbl.iter
    (fun nick chans ->
      fpf ppf "- %a:" Nickname.pp_print nick;
      List.iter
        (fun chan ->
          fpf ppf " %s" (Channel.to_string chan))
        chans)
    db.nick_to_chans;
  fpf ppf "\nNicknames per channel:\n";
  Hashtbl.iter
    (fun chan conns ->
      fpf ppf "- %s:" (Channel.to_string chan);
      List.iter
        (fun conn ->
          fpf ppf " %a" Nickname.pp_print (Identity.nick conn#identity))
        conns)
    db.chan_to_conns


let create () =
  { nick_to_conn = Hashtbl.create 8 ;
    nick_to_chans = Hashtbl.create 8 ;
    chan_to_conns = Hashtbl.create 8 }

let remove db nick =
  let conn = Hashtbl.find db.nick_to_conn nick in
  Hashtbl.remove db.nick_to_conn nick;
  Hashtbl.remove db.nick_to_chans nick;
  Hashtbl.filter_map_inplace
    (fun _ conns -> Some (List.filter (Connection.nequal conn) conns))
    db.chan_to_conns

let nick db conn nick' =
  if Hashtbl.mem db.nick_to_conn nick' then
    raise NickAlreadyInUse
  else
    (
      let identity = conn#identity in
      conn#set_identity (Identity.set_nick identity nick');
      try
        let nick = Identity.nick identity in
        Hashtbl.remove db.nick_to_conn nick;
        Hashtbl.add db.nick_to_conn nick' conn;
        let chans = Hashtbl.find db.nick_to_chans nick in
        Hashtbl.remove db.nick_to_chans nick;
        Hashtbl.add db.nick_to_chans nick' chans
      with
        Invalid_argument _ ->
        Hashtbl.add db.nick_to_conn nick' conn;
        Hashtbl.add db.nick_to_chans nick' []
    )

let rec fresh_nick db =
  let nick = Nickname.of_string ("nick_" ^ string_of_int (Random.int 99999)) in
  if Hashtbl.mem db.nick_to_conn nick then
    fresh_nick db
  else
    nick
  
let join db nick chan =
  let conn = Hashtbl.find db.nick_to_conn nick in
  let chans = Hashtbl.find db.nick_to_chans nick in
  Hashtbl.replace db.nick_to_chans nick (chan :: chans);
  try
    let conns = Hashtbl.find db.chan_to_conns chan in
    Hashtbl.replace db.chan_to_conns chan (conn :: conns)
  with
    Not_found ->
    Hashtbl.add db.chan_to_conns chan [conn]

let part db nick chan =
  let conn = Hashtbl.find db.nick_to_conn nick in
  let chans = Hashtbl.find db.nick_to_chans nick in
  Hashtbl.replace db.nick_to_chans nick (List.filter ((<>) chan) chans);
  try
    let conns = Hashtbl.find db.chan_to_conns chan in
    let conns = List.filter (Connection.nequal conn) conns in
    if conns = [] then
      Hashtbl.remove db.chan_to_conns chan
    else
      Hashtbl.replace db.chan_to_conns chan conns
  with
    Not_found -> ()

let is_in_chan db nick chan =
  Hashtbl.find db.nick_to_chans nick
  |> List.mem chan

let iter_s db f chan =
  Hashtbl.find db.chan_to_conns chan
  |> Lwt_list.iter_s f
