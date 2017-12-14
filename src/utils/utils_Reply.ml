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
  | Welcome of Utils_Nickname.t * Utils_Identity.t
  | Yourhost of Utils_Nickname.t * string * string
  | Created of Utils_Nickname.t * string
  | Myinfo of Utils_Nickname.t * string * string * string * string
  | Bounce of string * int
  | UserHost of (Utils_Nickname.t * bool * bool * string) list
  | IsOn of Utils_Nickname.t list
  | Away of Utils_Nickname.t * string
  | UnAway
  | NowAway
  | WhoisUser of Utils_Nickname.t * string * string * string
  | WhoisServer of Utils_Nickname.t * string * string
  | WhoisOperator of Utils_Nickname.t
  | WhoisIdle of Utils_Nickname.t * int
  | EndOfWhois of Utils_Nickname.t
  | WhoisChannels
  | WhoWasUser of Utils_Nickname.t * string * string * string
  | EndOfWhoWas of Utils_Nickname.t
  | List of Utils_Channel.t * int * string
  | ListEnd
  | UniqOpIs of Utils_Channel.t * Utils_Nickname.t
  | ChannelModeIs of Utils_Channel.t * string * string
  | NoTopic of Utils_Channel.t
  | Topic of Utils_Channel.t * string
  | Inviting of Utils_Channel.t * Utils_Nickname.t
  | Summoning of string
  | InviteList of Utils_Channel.t * string
  | EndOfInviteList of Utils_Channel.t
  | ExceptList of Utils_Channel.t * string
  | EndOfExceptList of Utils_Channel.t
  | Version of string * string * string * string
  | WhoReply of Utils_Channel.t * string * string * string * Utils_Nickname.t * string * string * string * string * string
  | EndOfWho of string
  | NamReply
  | EndOfNames of Utils_Channel.t
  | Links of string * string * string * string
  | EndOfLinks of string
  | BanList of Utils_Channel.t * string
  | EndOfBanList of Utils_Channel.t
  | Info of string
  | EndOfInfo
  | MotdStart of string
  | Motd of string
  | EndOfMotd
  | YoureOper
  | Rehashing of string
  | YoureService of string
  | Time of string * string
  | UsersStart
  | Users of string * string * string
  | EndOfUsers
  | NoUsers
  | TraceLink
  | TraceConnecting of string * string
  | TraceHandshake of string * string
  | TraceUnknown of string * string
  | TraceOperator of string * Utils_Nickname.t
  | TraceUser of string * Utils_Nickname.t
  | TraceServer
  | TraceService of string * string * string * string
  | TraceNewType of string * string
  | TraceClass of string * string
  | TraceLog of string * string
  | TraceEnd of string * string
  | StatsLinkInfo of string * string * string * string * string * string * string
  | StatsCommands of string * string * string * string
  | EndOfStats of string
  | StatsUptime of int * int * int * int
  | StatsOline
  | UmodeIs of string
  | ServList of string * string * string * string * string * string
  | ServListEnd of string * string
  | LuserClient of int * int * int
  | LuserOp of int
  | LuserUnknown of int
  | LuserChannels of int
  | LuserMe of int * int
  | AdminMe of string
  | AdminLoc1 of string
  | AdminLoc2 of string
  | AdminEmail of string
  | TryAgain of string

let fpf = Format.fprintf

let pp_print ppf = function
  | Welcome (nick, id) ->
     fpf ppf "001 %a :Welcome to the Internet Relay Network %a" Utils_Nickname.pp_print nick Utils_Identity.pp_print id
  | Yourhost (nick, servername, ver) ->
     fpf ppf "002 %a :Your host is %s, running version %s" Utils_Nickname.pp_print nick servername ver
  | Created (nick, date) ->
     fpf ppf "003 %a :This server was created %s" Utils_Nickname.pp_print nick date
  | Myinfo (nick, servername, version, usermodes, channelmodes) ->
     fpf ppf "004 %a %s %s %s %s" Utils_Nickname.pp_print nick servername version usermodes channelmodes
  | Bounce (server, port) ->
     fpf ppf "005 :Try server %s, port %d" server port
  | UserHost (nick_op_away_hostname_list) ->
     fpf ppf "302 :"; (*FIXME: space*)
     List.iter
       (fun (nick, op, away, hostname) ->
         fpf ppf " %a%s=%c%s"
           Utils_Nickname.pp_print nick
           (if op then "*" else "")
           (if away then '+' else '-')
           hostname)
       nick_op_away_hostname_list
  | IsOn nick_list ->
     fpf ppf "303 :"; (*FIXME: space*)
     List.iter
       (fun nick ->
         fpf ppf " %a" Utils_Nickname.pp_print nick)
       nick_list
  | Away (nick, message) ->
     fpf ppf "301 %a :%s" Utils_Nickname.pp_print nick message
  | UnAway ->
     fpf ppf "305 :You are no longer marked as being away"
  | NowAway ->
     fpf ppf "306 :You have been marked as being away"
  | WhoisUser (nick, user, host, real) ->
     fpf ppf "311 %a %s %s * :%s" Utils_Nickname.pp_print nick user host real
  | WhoisServer (nick, server, info) ->
     fpf ppf "312 %a %s :%s" Utils_Nickname.pp_print nick server info
  | WhoisOperator nick ->
     fpf ppf "313 %a :is an IRC operator" Utils_Nickname.pp_print nick
  | WhoisIdle (nick, time) ->
     fpf ppf "317 %a %d :seconds idle" Utils_Nickname.pp_print nick time
  | EndOfWhois nick ->
     fpf ppf "318 %a :End of WHOIS list" Utils_Nickname.pp_print nick
  | WhoisChannels ->
     assert false
  | WhoWasUser (nick, user, host, real) ->
     fpf ppf "314 %a %s %s * :%s" Utils_Nickname.pp_print nick user host real
  | EndOfWhoWas nick ->
     fpf ppf "369 %a :End of WHOWAS" Utils_Nickname.pp_print nick
  | List (channel, visible, topic) ->
     fpf ppf "322 %a %d :%s" Utils_Channel.pp_print channel visible topic
  | ListEnd ->
     fpf ppf "323 :End of LIST"
  | UniqOpIs (channel, nickname) ->
     fpf ppf "325 %a %a" Utils_Channel.pp_print channel Utils_Nickname.pp_print nickname
  | ChannelModeIs (channel, mode, params) ->
     fpf ppf "324 %a %s %s" Utils_Channel.pp_print channel mode params
  | NoTopic channel ->
     fpf ppf "331 %a :No topic is set" Utils_Channel.pp_print channel
  | Topic (channel, topic) ->
     fpf ppf "332 %a :%s" Utils_Channel.pp_print channel topic
  | Inviting (channel, nick) ->
     fpf ppf "341 %a %a" Utils_Channel.pp_print channel Utils_Nickname.pp_print nick
  | Summoning user ->
     fpf ppf "342 %s :Summoning user to IRC" user
  | InviteList (channel, invitemask) ->
     fpf ppf "346 %a %s" Utils_Channel.pp_print channel invitemask
  | EndOfInviteList channel ->
     fpf ppf "347 %a :End of channel invite list" Utils_Channel.pp_print channel
  | ExceptList (channel, exceptionmask) ->
     fpf ppf "348 %a %s" Utils_Channel.pp_print channel exceptionmask
  | EndOfExceptList channel ->
     fpf ppf "349 %a :End of channel exception list" Utils_Channel.pp_print channel
  | Version (version, debuglevel, server, comments) ->
     fpf ppf "351 %s.%s %s :%s" version debuglevel server comments
  | WhoReply (channel, user, host, server, nick, hg, op, voice, hopcount, real) ->
     fpf ppf "352 %a %s %s %s %a %s%s%s :%s %s" Utils_Channel.pp_print channel user host server Utils_Nickname.pp_print nick hg op voice hopcount real
  | EndOfWho name ->
     fpf ppf "315 %s :End of WHO list" name
  | NamReply ->
     assert false
  | EndOfNames channel ->
     fpf ppf "366 %a :End of NAMES list" Utils_Channel.pp_print channel
  | Links (mask, server, hopcount, info) ->
     fpf ppf "364 %s %s :%s %s" mask server hopcount info
  | EndOfLinks mask ->
     fpf ppf "365 %s :End of LINKS list" mask
  | BanList (channel, banmask) ->
     fpf ppf "367 %a %s" Utils_Channel.pp_print channel banmask
  | EndOfBanList channel ->
     fpf ppf "368 %a :End of channel ban list" Utils_Channel.pp_print channel
  | Info info ->
     fpf ppf "371 :%s" info
  | EndOfInfo ->
     fpf ppf "374 :End of INFO list"
  | MotdStart server ->
     fpf ppf "375 :- %s Message of the day - " server
  | Motd text ->
     fpf ppf "372 :- %s" text
  | EndOfMotd ->
     fpf ppf "376 :End of MOTD command"
  | YoureOper ->
     fpf ppf "381 :You are now an IRC operator"
  | Rehashing configfile ->
     fpf ppf "382 %s :Rehashing" configfile
  | YoureService name ->
     fpf ppf "383 You are service %s" name
  | Time (server, time) ->
     fpf ppf "391 %s :%s" server time
  | UsersStart ->
     fpf ppf "392 :UserID   Terminal  Host"
  | Users (username, ttyline, hostname) ->
     fpf ppf "393 :%s %s %s" username ttyline hostname
  | EndOfUsers ->
     fpf ppf "394 :End of users"
  | NoUsers ->
     fpf ppf "395 :Nobody logged in"
  | TraceLink ->
     assert false
  | TraceConnecting (clas, server) ->
     fpf ppf "201 Try. %s %s" clas server
  | TraceHandshake (clas, server) ->
     fpf ppf "202 H.S. %s %s" clas server
  | TraceUnknown (clas, ip) ->
     fpf ppf "203 ???? %s %s" clas ip
  | TraceOperator (clas, nick) ->
     fpf ppf "204 Oper %s %a" clas Utils_Nickname.pp_print nick
  | TraceUser (clas, nick) ->
     fpf ppf "205 User %s %a" clas Utils_Nickname.pp_print nick
  | TraceServer ->
     assert false
  | TraceService (clas, name, typ, active_type) ->
     fpf ppf "207 Service %s %s %s %s" clas name typ active_type
  | TraceNewType (newtype, clientname) ->
     fpf ppf "208 %s 0 %s" newtype clientname
  | TraceClass (clas, count) ->
     fpf ppf "209 Class %s %s" clas count
  | TraceLog (logfile, debuglevel) ->
     fpf ppf "261 File %s %s" logfile debuglevel
  | TraceEnd (servername, versiondebuglevel) ->
     fpf ppf "262 %s %s :End of TRACE" servername versiondebuglevel
  | StatsLinkInfo (linkname, sendq, sent_messages, sent_kbytes, received_messages, received_kbytes, time_open) ->
     fpf ppf "211 %s %s %s %s %s %s %s" linkname sendq sent_messages sent_kbytes received_messages received_kbytes time_open
  | StatsCommands (command, count, byte_count, remote_count) ->
     fpf ppf "212 %s %s %s %s" command count byte_count remote_count
  | EndOfStats stats_letter ->
     fpf ppf "219 %s :End of STATS report" stats_letter
  | StatsUptime (days, hours, minutes, seconds) ->
     fpf ppf "242 :Server Up %d days %d:%02d:%02d" days hours minutes seconds
  | StatsOline ->
     assert false
  | UmodeIs user_mode_string ->
     fpf ppf "221 %s" user_mode_string
  | ServList (name, server, mask, typ, hopcount, info) ->
     fpf ppf "234 %s %s %s %s %s %s" name server mask typ hopcount info
  | ServListEnd (mask, typ) ->
     fpf ppf "235 %s %s :End of service listing" mask typ
  | LuserClient (users, services, servers) ->
     fpf ppf "251 :There are %d users and %d services on %d servers" users services servers
  | LuserOp operators ->
     fpf ppf "252 %d :operator(s) online" operators
  | LuserUnknown unknown_connections ->
     fpf ppf "253 %d :unknown connection(s)" unknown_connections
  | LuserChannels channels ->
     fpf ppf "254 %d :channels formed" channels
  | LuserMe (clients, servers) ->
     fpf ppf "255 :I have %d clients and %d servers" clients servers
  | AdminMe server ->
     fpf ppf "256 %s :Administrative info" server
  | AdminLoc1 info ->
     fpf ppf "257 :%s" info
  | AdminLoc2 info ->
     fpf ppf "258 :%s" info
  | AdminEmail info ->
     fpf ppf "259 :%s" info
  | TryAgain command ->
     fpf ppf "263 %s :Please wait a while and try again." command
