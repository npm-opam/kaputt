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

(** This module provides utility functions for the other modules. *)


(** {6 Conversion utilities} *)

val string_of_unit : unit -> string
(** Always returns ["()"]. *)

val string_of_char : char -> string
(** Converts a character into an escaped string, for display. *)

val string_of_string : string -> string
(** Converts a string into an escaped string, for display. *)

val string_of_complex : Complex.t -> string
(** Converts a complex into a string. *)

val string_of_buffer : Buffer.t -> string
(** Converts a buffer into an escaped string. *)

val make_string_of_array : ('a -> string) -> 'a array -> string
(** Converts an array into a string, the passed function being used to
    convert array elements. *)

val make_string_of_list : ('a -> string) -> 'a list -> string
(** Converts a list into a string, the passed function being used to
    convert list elements. *)

val make_string_of_option : ('a -> string) -> 'a option -> string
(** Converts an option into a string, the passed function being used to
    convert the embedded element (if any). *)

val make_string_of_ref : ('a -> string) -> 'a ref -> string
(** Converts a reference into a string, the passed function being used to
    convert the embedded element. *)

val make_string_of_hashtbl : ('a -> string) -> ('b -> string) -> ('a, 'b) Hashtbl.t -> string
(** Converts a hash table into a string, the passed functions being used
    to convert table keys and values. *)

val make_string_of_queue : ('a -> string) -> 'a Queue.t -> string
(** Converts a queue into a string, the passed function being used to
    convert queue elements. *)

val make_string_of_stack : ('a -> string) -> 'a Stack.t -> string
(** Converts a stack into a string, the passed function being used to
    convert stack elements. *)

val make_string_of_weak : ('a option -> string) -> 'a Weak.t -> string
(** Converts a weak array into a string, the passed function being used to
    convert array elements. *)

val make_string_of_tuple1 : ('a -> string) -> 'a -> string
(** The identity function. *)

val make_string_of_tuple2 : ('a -> string) -> ('b -> string) -> ('a * 'b) -> string
(** Converts a couple into a string, the passed functions being used to
    convert the various components. *)

val make_string_of_tuple3 : ('a -> string) -> ('b -> string) -> ('c -> string) -> ('a * 'b * 'c) -> string
(** Converts a triple into a string, the passed functions being used to
    convert the various components. *)

val make_string_of_tuple4 : ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) -> ('a * 'b * 'c * 'd) -> string
(** Converts a quadruple into a string, the passed functions being used to
    convert the various components. *)

val make_string_of_tuple5 : ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) -> ('e -> string) -> ('a * 'b * 'c * 'd * 'e) -> string
(** Converts a quintuple into a string, the passed functions being used to
    convert the various components. *)
