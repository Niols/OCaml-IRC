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

type user = string
type mode = string
type mask = string
type service = string
type server = string
type keyed_channel = Channel.t * Channel.key option

type t =
  (* These commands are taken from RFC 2812 *)

  (* 3.1 Connection Registration *)
  | Pass of string
  | Nick of Nickname.t
  | User of user * mode * string
  | Oper of string * string
  (* | Mode of string * string list *) (*FIXME: chan vs. user modes*)
  | Service of string * string * string * string * string * string
  | Quit of string
  | Squit of string * string

  (* 3.2 Channel operations *)
  | Join of keyed_channel list
  | Part of Channel.t list * string
  | Mode of string * string list
  | Topic of string * string option
  | Names of string option * string option
  | List of string option * string option
  | Invite of string * string
  | Kick of string * string * string option

  (* 3.3 Sending messages *)
  | Privmsg of string * string
  | Notice of string * string

  (* 3.4 Server queries and commands *)
  | Motd of string option
  | Lusers of string option * string option
  | Version of string option
  | Stats of string option * string option
  | Links of string option * string option
  | Time of string option
  | Connect of string * int * string option
  | Trace of string option
  | Admin of string option
  | Info of string option

  (* 3.5 Squery *)
  | Servlist of mask option * string option
  | Squery of service * string

  (* 3.6 Who query *)
  | Who of mask * bool
  | Whois of mask list * server option
  | Whowas of Nickname.t * int option * string option

  (* 3.7 Miscellaneous messages *)
  | Kill of string * string
  | Ping of server * server option
  | Pong of server * server option
  | Error of string

  (* 5.1 Command responses *)
  | Rpl_Welcome of Nickname.t * Identity.t
  | Rpl_Yourhost of Nickname.t * string * string
  | Rpl_Created of Nickname.t * string
  | Rpl_Myinfo of Nickname.t * string * string * string * string

  (* 5.2 Error replies *)
  | Err_NicknameInUse of Nickname.t


let fpf = Format.fprintf

let rec pp_params ppf = function
  | [] -> ()
  | [param] -> fpf ppf " :%s" param
  | param :: params ->
     (* assert param does not contain spaces *)
     fpf ppf " %s%a" param pp_params params

let pp_print ppf = function

  (* JOIN *)
  | Join [] ->
     fpf ppf "JOIN 0"
  | Join keyed_channels ->
     fpf ppf "JOIN";
     let keyed_channels =
       List.sort
         (fun (_, k1) (_, k2) ->
           match k1 , k2 with
           | None , Some _ -> 1
           | Some _ , None -> -1
           | _ -> 0)
         keyed_channels
     in
     let channels , keys =
       List.split keyed_channels
     in
     let keys =
       List.filter (fun o -> o <> None) keys
       |> List.map (function Some k -> k | None -> assert false)
     in
     fpf ppf " %s" (String.concat "," (List.map Channel.to_string channels));
     if keys <> [] then
       fpf ppf " %s" (String.concat "," keys)

  (* NICK *)
  | Nick nick ->
     fpf ppf "NICK %a" Nickname.pp_print nick

  (* USER *)
  | User (user, mode, realname) ->
     fpf ppf "USER %s %s * :%s" user mode realname

  (* 3.3 Sending messages *)
  | Privmsg (target, message) ->
     fpf ppf "PRIVMSG %s :%s" target message

  (* 3.7 Miscellaneous messages *)
  | Ping (server, None) ->
     fpf ppf "PING :%s" server
  | Ping (server1, Some server2) ->
     fpf ppf "PING %s :%s" server1 server2
  | Pong (server, None) ->
     fpf ppf "PONG :%s" server
  | Pong (server1, Some server2) ->
     fpf ppf "PONG %s :%s" server1 server2
  | Error message ->
     fpf ppf "ERROR :%s" message

  (* 5.1 Command responses *)
  | Rpl_Welcome (nick, id) ->
     fpf ppf "001 %a :Welcome to the Internet Relay Network %a" Nickname.pp_print nick Identity.pp_print id
  | Rpl_Yourhost (nick, servername, ver) ->
     fpf ppf "002 %a :Your host is %s, running version %s" Nickname.pp_print nick servername ver
  | Rpl_Created (nick, date) ->
     fpf ppf "003 %a :This server was created %s" Nickname.pp_print nick date
  | Rpl_Myinfo (nick, servername, version, usermodes, channelmodes) ->
     fpf ppf "004 %a %s %s %s %s" Nickname.pp_print nick servername version usermodes channelmodes

  | Err_NicknameInUse nick ->
     fpf ppf "433 %a :Nickname is already in use" Nickname.pp_print nick
    
  | _ -> assert false


exception Malformed of string * string list

let from_strings command params =
  match command , params with

  (* RFC 2812 ; 3.2.1 Join message *)
  | "JOIN" , ["0"] ->
     Join []
  | "JOIN" , [channels] ->
     (
       try
         let keyed_channels =
           String.split_on_char ',' channels
           |> List.map (fun channel -> (Channel.of_string channel, None))
         in
         Join keyed_channels
       with
         Invalid_argument _ -> raise (Malformed (command, params))
     )

  (* RFC 2812 ; 3.1.2 Nick message *)
  | "NICK" , [nick] ->
     (
       try
         Nick (Nickname.of_string nick)
       with
         Invalid_argument _ -> raise (Malformed (command, params))
     )

  (* RFC 2812 ; 3.1.3 User message *)
  | "USER" , [user; mode; _unused; realname] ->
     User (user, mode, realname) (*FIXME: mode*)

  | "PING", [server] ->
     Ping (server, None)
  | "PING", [server1; server2] ->
     Ping (server1, Some server2)

  | "PRIVMSG", [target; message] ->
     Privmsg (target, message)

  (* others *)
  | _ ->
     raise (Malformed (command, params))
