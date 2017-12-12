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

(** A module that contains the possible error replies according the the RFC. *)

type t =
  (** A type for error replies *)

  | NoSuchNick of Nickname.t
  (** Used to indicate the nickname parameter supplied to a command is
     currently unused. *)

  | NoSuchServer of string
  (** Used to indicate the server name given currently does not
     exist. *)

  | NoSuchChannel of string
  (** Used to indicate the given channel name is invalid. *)

  | CannotSendToChan of Channel.t
  (** Sent to a user who is either (a) not on a channel which is mode
     +n or (b) not a chanop (or mode +v) on a channel which has mode
     +m set or where the user is banned and is trying to send a
     PRIVMSG message to that channel. *)

  | TooManyChannels of Channel.t
  (** Sent to a user when they have joined the maximum number of
     allowed channels and they try to join another channel. *)

  | WasNoSuchNick of Nickname.t
  (** Returned by WHOWAS to indicate there is no history information
     for that nickname. *)

  | TooManyTargets of string * string * string
  (** Returned to a client which is attempting to send a
     PRIVMSG/NOTICE using the user@host destination format and for a
     user@host which has several occurrences.

     Returned to a client which trying to send a PRIVMSG/NOTICE to too
     many recipients.

     Returned to a client which is attempting to JOIN a safe channel
     using the shortname when there are more than one such channel. *)

  | NoSuchService of string
  (** Returned to a client which is attempting to send a SQUERY to a
     service which does not exist. *)

  | NoOrigin
  (** PING or PONG message missing the originator parameter. *)

  | NoRecipient
  | NoTextToSend
  | NoTopLevel of string
  | WildTopLevel of string
  | BadMask of string

  | UnknownCommand of string
  (** Returned to a registered client to indicate that the command
     sent is unknown by the server. *)

  | NoMotd
  (** Server's MOTD file could not be opened by the server. *)

  | NoAdminInfo of string
  (** Returned by a server in response to an ADMIN message when there
     is an error in finding the appropriate information. *)

  | FileError of string * string
  (** Generic error message used to report a failed file operation
     during the processing of a message. *)

  | NoNicknameGiven
  (** Returned when a nickname parameter expected for a command and
     isn't found. *)

  | ErroneousNickname of string
  (** Returned after receiving a NICK message which contains
     characters which do not fall in the defined set. *)

  | NicknameInUse of Nickname.t
  (** Returned when a NICK message is processed that results in an
     attempt to change to a currently existing nickname. *)

  | NickCollision of string * string * string
  (** Returned by a server to a client when it detects a nickname
     collision (registered of a NICK that already exists by another
     server). *)

  | UnavailResource of string
  (** Returned by a server to a user trying to join a channel
     currently blocked by the channel delay mechanism.

     Returned by a server to a user trying to change nickname when the
     desired nickname is blocked by the nick delay mechanism. *)

  | UserNotInChannel of Nickname.t * Channel.t
  (** Returned by the server to indicate that the target user of the
     command is not on the given channel. *)

  | NotOnChannel of Channel.t
  (** Returned by the server whenever a client tries to perform a
     channel affecting command for which the client isn't a member. *)

  | UserOnChannel of Nickname.t * Channel.t
  (** Returned when a client tries to invite a user to a channel they
     are already on. *)

  | NoLogin of string
  (** Returned by the summon after a SUMMON command for a user was
     unable to be performed since they were not logged in. *)

  | SummonDisabled
  (** Returned as a response to the SUMMON command.  MUST be returned
     by any server which doesn't implement it. *)

  | UsersDisabled
  (** Returned as a response to the USERS command.  MUST be returned
     by any server which does not implement it. *)

  | NotRegistered
  (** Returned by the server to indicate that the client MUST be
     registered before the server will allow it to be parsed in
     detail. *)

  | NeedMoreParams of string
  (** Returned by the server by numerous commands to indicate to the
     client that it didn't supply enough parameters. *)

  | AlreadyRegistered
  (** Returned by the server to any link which tries to change part of
     the registered details (such as password or user details from
     second USER message). *)

  | NoPermFromHost
  (** Returned to a client which attempts to register with a server
     which does not been setup to allow connections from the host the
     attempted connection is tried. *)

  | PasswdMismatch
  (** Returned to indicate a failed attempt at registering a
     connection for which a password was required and was either not
     given or incorrect. *)

  | YoureBannedCreep
  (** Returned after an attempt to connect and register yourself with
     a server which has been setup to explicitly deny connections to
     you. *)

  | YouWillBeBanned
  (** Sent by a server to a user to inform that access to the server
     will soon be denied. *)

  | KeySet of Channel.t
  | ChannelIsFull of Channel.t
  | UnknownMode of char * Channel.t
  | InviteOnlyChan of Channel.t
  | BannedFromChan of Channel.t
  | BadChannelKey of Channel.t
  | BadChanMask of Channel.t
  | NoChanModes of Channel.t
  | BanListFull of Channel.t * char

  | NoPrivileges
  (** Any command requiring operator privileges to operate MUST return
     this error to indicate the attempt was unsuccessful. *)

  | ChanopPrivNeeded of Channel.t
  (** Any command requiring 'chanop' privileges (such as MODE
     messages) MUST return this error if the client making the attempt
     is not a chanop on the specified channel. *)

  | CantKillServer
  (** Any attempts to use the KILL command on a server are to be
     refused and this error returned directly to the client. *)

  | Restricted
  (** Sent by the server to a user upon connection to indicate the
     restricted nature of the connection (user mode "+r"). *)

  | UniqOpPrivNeeded
  (** Any MODE requiring "channel creator" privileges MUST return this
     error if the client making the attempt is not a chanop on the
     specified channel. *)

  | NoOperHost
  (** If a client sends an OPER message and the server has not been
     configured to allow connections from the client's host as an
     operator, this error MUST be returned. *)

  | UModeUnknownFlag
  (** Returned by the server to indicate that a MODE message was sent
     with a nickname parameter and that the a mode flag sent was not
     recognized. *)

  | UsersDontMatch
  (** Error sent to any user trying to view or change the user mode for
    a user other than themselves. *)

exception Exception of t
                     
val pp_print : Format.formatter -> t -> unit
