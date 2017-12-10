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

type t = string
type key = string

let is_valid s =
  (* FIXME: more complicated than that *)
  let l = String.length s in
  0 < l && l <= 50
  && (s.[0] = '&' || s.[0] = '#' || s.[0] = '+' || s.[0] = '!')
  && (String.index_opt s ' ' = None)
  && (String.index_opt s ',' = None)
  && (String.index_opt s (Char.chr 7) = None)

let of_string s =
  if is_valid s then
    Misc.lowercase s
  else
    raise (Invalid_argument "Channel.of_string")

let to_string s =
  s
