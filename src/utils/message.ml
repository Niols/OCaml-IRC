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

module Prefix =
  struct
    type prefix =
      | Servername of string
      | Identity of Identity.t

    type t = prefix option

    let pp_print ppf = function
      | None -> ()
      | Some (Servername s) ->
         fpf ppf ":%s" s
      | Some (Identity id) ->
         fpf ppf ":%a " Identity.pp_print id
  end

type t =
  { prefix : Prefix.t ;
    command : Command.t }

let make prefix command =
  { prefix = Some prefix ; command }
let make_noprefix command =
  { prefix = None ; command }

let pp_print ?(crlf=false) ppf m =
  (if m.prefix = None
   then fpf ppf "%a"
   else fpf ppf "%a %a" Prefix.pp_print m.prefix) Command.pp_print m.command;
  if crlf then fpf ppf "\r\n"

let to_string ?(crlf=false) m =
  let buf = Buffer.create 8 in
  let ppf = Format.formatter_of_buffer buf in
  pp_print ~crlf ppf m;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let from_string str =
  let lb = NegLexing.of_string str in

  (* get prefix if there is one *)
  let prefix =
    match NegLexing.peek_char lb with
    | ':' ->
       NegLexing.next_char lb;
       (
         try Some (Prefix.Identity (Identity.from_string (NegLexing.next_sep ' ' lb)))
         with Not_found -> raise (Invalid_argument "Message.from_string: found a prefix but no command")
       )
    | _ ->
       None
  in

  (* get command; there must be one *)
  let command =
    try NegLexing.next_sep ' ' lb
    with Not_found -> NegLexing.remaining lb
  in

  (* get parameters *)
  let params =
    let rec find_params acc =
      try
        (
          match NegLexing.peek_char lb with
          | ':' ->
             (* trailing: we take everything from here until the end and
                check that we end indeed with a newline *)
             NegLexing.next_char lb;
             let trailing = NegLexing.remaining lb in
             trailing :: acc

          | _ ->
             (
               try
                 (* a regular parameter *)
                 find_params ((NegLexing.next_sep ' ' lb) :: acc)
               with
                 Not_found ->
                 (* the last parameter *)
                 let trailing = NegLexing.remaining lb in
                 trailing :: acc
             )
        )
      with
      | NegLexing.Error "end of lexbuf" ->
         acc
    in
    List.rev (find_params [])
  in

  { prefix = prefix ;
    command = Command.from_strings command params }
