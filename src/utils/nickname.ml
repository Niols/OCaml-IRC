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

type t = string

let pp_print ppf nick =
  fpf ppf "%s" nick

let to_string nick =
  nick

let is_valid nick =
  try
    ignore (to_string nick);
    true
  with
    Invalid_argument _ -> false
  
let of_string nick =
  (* nickname   =  ( letter / special ) *8( letter / digit / special / "-" ) *)
  (* letter     =  %x41-5A / %x61-7A       ; A-Z / a-z *)
  (* digit      =  %x30-39                 ; 0-9 *)
  (* special    =  %x5B-60 / %x7B-7D *)
  (*                   ; "[", "]", "\\", "`", "_", "^", "{", "|", "}" *)
  if String.length nick = 0 then
    raise (Invalid_argument "Nickname.of_string");
  let c = Char.code nick.[0] in
  if not (c = 45 || (65 <= c && c <= 125)) then
    raise (Invalid_argument "Nickname.of_string");
  for i = 1 to String.length nick - 1 do
    let c = Char.code nick.[i] in
    if not (c = 45 || (48 <= c && c <= 57) || (65 <= c && c <= 125)) then
      raise (Invalid_argument "Nickname.of_string");
  done;
nick
