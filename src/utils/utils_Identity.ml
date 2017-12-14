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

type t =
  { nick : Utils_Nickname.t option ;
    user : string option ;
    host : string option }

let nick_opt id = id.nick
let nick id = unwrap (nick_opt id)

let user_opt id = id.user
let user id = unwrap (user_opt id)

let host_opt id = id.host
let host id = unwrap (host_opt id)
            
let make_opt nick user host = { nick ; user ; host }
let make nick user host =
  { nick = Some nick ;
    user = if user = "" then None else Some user ;
    host = if host = "" then None else Some host }

let set_nick id nick =
  { id with nick = Some nick }

let set_user id user =
  { id with user = Some user }

let is_valid id =
  id.nick <> None && not (id.user <> None && id.host = None)

let pp_print ppf id =
  Utils_Nickname.pp_print ppf (nick id) ;
  if id.user <> None then fpf ppf "!%s" (user id);
  if id.host <> None then fpf ppf "%@%s" (host id)

let from_string str =
  let buf = NegLexing.of_string str in
  match NegLexing.next_sep '!' buf with
  | exception Not_found ->
     (
       match NegLexing.next_sep '@' buf with
       | exception Not_found ->
          { nick = Some (Utils_Nickname.of_string (NegLexing.remaining buf)) ;
            user = None ;
            host = None }
       | nick ->
          { nick = Some (Utils_Nickname.of_string nick) ;
            user = None ;
            host = Some (NegLexing.remaining buf) }
     )
  | nick ->
     (
       match NegLexing.next_sep '@' buf with
       | exception Not_found -> raise (Invalid_argument "Identity.from_string")
       | user ->
          { nick = Some (Utils_Nickname.of_string nick) ;
            user = Some user ;
            host = Some (NegLexing.remaining buf) }
     )
