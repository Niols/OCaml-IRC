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

type t

val nick : t -> Utils_Nickname.t
val nick_opt : t -> Utils_Nickname.t option

val user : t -> string
val user_opt : t -> string option

val host : t -> string
val host_opt : t -> string option

val make_opt : Utils_Nickname.t option -> string option -> string option -> t
val make : Utils_Nickname.t -> string -> string -> t

val set_nick : t -> Utils_Nickname.t -> t
val set_user : t -> string -> t

val is_valid : t -> bool
  
val pp_print : Format.formatter -> t -> unit

val from_string : string -> t
