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

exception Error of string

type lexbuf =
  { content : string ;
    length : int ;
    mutable pos : int }

let of_string s =
  { content = s ;
    length = String.length s ;
    pos = 0 }

let peek_char_opt lb =
  if lb.pos >= lb.length
  then None
  else Some (lb.content.[lb.pos])

let peek_char lb =
  match peek_char_opt lb with
  | None -> raise (Error "end of lexbuf")
  | Some c -> c

let next_char lb =
  lb.pos <- lb.pos + 1

let pop_char_opt lb =
  let co = peek_char_opt lb in
  next_char lb;
  co

let pop_char lb =
  let c = peek_char lb in
  next_char lb;
  c

let next_sep sep lb =
  match String.index_from lb.content lb.pos sep with
  | exception Invalid_argument _ -> raise Not_found
  | exception Not_found -> raise Not_found
  | i ->
     let s = String.sub lb.content lb.pos (i - lb.pos) in
     lb.pos <- i + 1;
     s

let remaining lb =
  let s = String.sub lb.content lb.pos (lb.length - lb.pos) in
  lb.pos <- lb.length;
  s

let debug_repr lb =
  let open Format in
  printf "%s\n" lb.content;
  for i = 1 to lb.pos do
    printf " "
  done;
  printf "^@."
