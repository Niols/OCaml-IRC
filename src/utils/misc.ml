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

(* Because of IRC's Scandinavian origin, the characters {}|^ are
   considered to be the lower case equivalents of the characters []\~,
   respectively. This is a critical issue when determining the
   equivalence of two nicknames or channel names. *)
let lowercase_bytes b =
  let b = Bytes.lowercase_ascii b in
  Bytes.iteri
    (fun i -> function
      | '[' -> Bytes.set b i '{'
      | ']' -> Bytes.set b i '}'
      | '\\' -> Bytes.set b i '|'
      | '~' -> Bytes.set b i '^'
      | _ -> ())
    b;
  b

let lowercase s =
  Bytes.to_string (lowercase_bytes (Bytes.of_string s))
