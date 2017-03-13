(*
 * This file is part of Kaputt.
 * Copyright (C) 2008-2012 Xavier Clerc.
 *
 * Kaputt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Kaputt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** This module provides functions reducing values,
    in order to produce smaller counterexamples. *)


(** {6 Reducer definition} *)

type 'a t = 'a -> 'a list
(** The type of reducer, a function returning a list of some {i smaller}
    elements. *)


(** {6 Predefined reducers} *)

val unit : unit t
(** Reducer for [unit] values. *)

val bool : bool t
(** Reducer for [bool] values. *)

val int : int t
(** Reducer for [int] values. *)

val int32 : int32 t
(** Reducer for [int32] values. *)

val int64 : int64 t
(** Reducer for [int64] values. *)

val nativeint : nativeint t
(** Reducer for [nativeint] values. *)

val char : char t
(** Reducer for [char] values. *)

val string : string t
(** Reducer for [string] values. *)

val float : float t
(** Reducer for [float] values. *)

val complex : Complex.t t
(** Reducer for [Complex.t] values. *)


(** {6 Reducers for containers} *)

val array : 'a array t
(** Reducer for [array] values. *)

val list : 'a list t
(** Reducer for [list] values. *)

val option : 'a option t
(** Reducer for [option] values. *)

module Map (M : Map.S) : sig
  val red : 'a M.t t
end
(** Functor used to build reducers for [Map.S.t] values. *)

module Set (S : Set.S) : sig
  val red : S.t t
end
(** Functor used to build reducers for [Set.S.t] values. *)

val hashtbl : ('a, 'b) Hashtbl.t t
(** Reducer for [Hashtbl.t] values. *)

val queue : 'a Queue.t t
(** Reducer for [Queue.t] values. *)

val stack : 'a Stack.t t
(** Reducer for [Stack.t] values. *)

val weak : 'a Weak.t t
(** Reducer for [Weak.t] values. *)
