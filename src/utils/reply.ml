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

type t =
  | Welcome of Nickname.t * Identity.t
  | Yourhost of Nickname.t * string * string
  | Created of Nickname.t * string
  | Myinfo of Nickname.t * string * string * string * string
  | Bounce
  | UserHost
  | Ison
  | Away
  | UnAway
  | NowAway
  | WhoisUser
  | WhoisServer
  | WhoisOperator
  | WhoisIdle
  | EndOfWhois
  | WhoisChannels
  | WhoWasUser
  | EndOfWhoWas
  | ListStart
  | List
  | ListEnd
  | UniqOpIs
  | ChannelModeIs
  | NoTopic
  | Topic
  | Inviting
  | Summoning
  | InviteList
  | EndOfInviteList
  | ExceptList
  | EndOfExceptList
  | Version
  | WhoReply
  | EndOfWho
  | NamReply
  | EndOfNames
  | Links
  | EndOfLinks
  | BanList
  | EndOfBanList
  | Info
  | EndOfInfo
  | MotdStart
  | Motd
  | EndOfMotd
  | YoureOper
  | Rehashing
  | YoureService
  | Time
  | UsersStart
  | Users
  | EndOfUsers
  | NoUsers
  | TraceLink
  | TraceConnecting
  | TraceHandshake
  | TraceUnknown
  | TraceOperator
  | TraceUser
  | TraceServer
  | TraceService
  | TraceNewType
  | TraceClass
  | TraceReconnect
  | TraceLog
  | TraceEnd
  | StatsLinkInfo
  | StatsCommands
  | EndOfStats
  | StatsUptime
  | StatsOline
  | UmodeIs
  | ServList
  | ServListEnd
  | LuserClient
  | LuserOp
  | LuserUnknown
  | LuserChannels
  | LuserMe
  | AdminMe
  | AdminLoc1
  | AdminLoc2
  | AdminEmail
  | TryAgain

let fpf = Format.fprintf

let pp_print ppf = function
  | Welcome (nick, id) ->
     fpf ppf "001 %a :Welcome to the Internet Relay Network %a" Nickname.pp_print nick Identity.pp_print id
  | Yourhost (nick, servername, ver) ->
     fpf ppf "002 %a :Your host is %s, running version %s" Nickname.pp_print nick servername ver
  | Created (nick, date) ->
     fpf ppf "003 %a :This server was created %s" Nickname.pp_print nick date
  | Myinfo (nick, servername, version, usermodes, channelmodes) ->
     fpf ppf "004 %a %s %s %s %s" Nickname.pp_print nick servername version usermodes channelmodes
  | _ -> assert false
