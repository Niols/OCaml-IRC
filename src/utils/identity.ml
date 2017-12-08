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
  { nick : Nickname.t ;
    user : string ;
    host : string }

let make nick user host = { nick ; user ; host }
  
let is_valid nuh =
  Nickname.is_valid nuh.nick
  && not (nuh.user <> "" && nuh.host = "")

let pp_print ppf nuh =
  Nickname.pp_print ppf nuh.nick ;
  if nuh.user <> "" then fpf ppf "!%s" nuh.user;
  if nuh.host <> "" then fpf ppf "%@%s" nuh.host

let from_string str =
  let buf = NegLexing.of_string str in
  match NegLexing.next_sep '!' buf with
  | exception Not_found ->
     (
       match NegLexing.next_sep '@' buf with
       | exception Not_found ->
          { nick = Nickname.of_string (NegLexing.remaining buf) ;
            user = "" ;
            host = "" }
       | nick ->
          { nick = Nickname.of_string nick ;
            user = "" ;
            host = NegLexing.remaining buf }
     )
  | nick ->
     (
       match NegLexing.next_sep '@' buf with
       | exception Not_found -> raise (Invalid_argument "Identity.from_string")
       | user ->
          { nick = Nickname.of_string nick ;
            user ;
            host = NegLexing.remaining buf }
     )
