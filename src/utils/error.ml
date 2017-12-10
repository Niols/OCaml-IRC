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
  | NoSuchNick of Nickname.t
  | NoSuchServer of string (*server*)
  | NoSuchChannel of Channel.t
  | CannotSendToChan of Channel.t
  | TooManyChannels of Channel.t
  | WasNoSuchNick of Nickname.t
  | TooManyTargets
  | NoSuchService
  | NoOrigin
  | NoRecipient
  | NoTextToSend
  | NoTopLevel
  | WildTopLevel
  | BadMask
  | UnknownCommand
  | NoMotd
  | NoAdminInfo
  | FileError
  | NoNicknameGiven
  | ErroneousNickname
  | NicknameInUse of Nickname.t
  | NickCollision
  | UnavailResource
  | UserNotInChannel
  | NotOnChannel
  | UserOnChannel
  | NoLogin
  | SummonDisabled
  | UserDisabled
  | NotRegistered
  | NeedMoreParams
  | AlreadyRegistered
  | NoPermFromHost
  | PasswdMismatch
  | YoureBannedCreep
  | YouWillBeBanned
  | KeySet
  | ChannelIsFull
  | UnknownMode
  | InviteOnlyChan
  | BannedFromChan
  | BadChannelKey
  | BadChanMask
  | NoChanModes
  | BanListFull
  | NoPrivileges
  | ChanopPrivNeeded
  | CantKillServer
  | Restricted
  | UniqOpPrivNeeded
  | NoOperHost
  | UModeUnknownFlag
  | UsersDontMatch

let fpf = Format.fprintf

let pp_print ppf = function
  | NoSuchNick nick ->
     fpf ppf "401 %a :No such nick/channel" Nickname.pp_print nick
  | NoRecipient ->
     fpf ppf "411 :No recipient given"
  | NicknameInUse nick ->
     fpf ppf "433 %a :Nickname is already in use" Nickname.pp_print nick
  | _ -> assert false
