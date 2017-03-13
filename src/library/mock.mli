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

(** This module provides support for {i mock} functions, that record
    their calls. *)


(** {6 Exceptions and types} *)

exception Unexpected_value of string
(** The exception raised when a function is called with an unexpected
    parameter value. *)

exception End_of_sequence
(** The exception raised when a function is called while it has alredy
    returned all predefined results. *)

type ('a, 'b) t
(** The type of {i mock} functions from ['a] to ['b]. *)


(** {6 Mock builders} *)

val from_mapping : ?cmp:('a -> 'a -> int) -> ?prn:('a -> string) -> ('a * 'b) list -> ('a, 'b) t
(** [from_mapping ~cmp ~prn l] returns a mock function based on the
    association list [l] whose couples are [(input, output)] indicating
    that the function should return [output] when call with paramer
    [input]. [cmp] (defaulting to [Pervasives.compare]) is used to
    compare input values), while [prn] is used to convert input values
    into strings.

    Raises [Unexpected_value] if the function received an input value
    that do not appear in [l]. *)

val from_sequence : ?cmp:('a -> 'a -> int) -> ?prn:('a -> string) -> ('a * 'b) list -> ('a, 'b) t
(** [from_sequence ~cmp ~prn l] returns a mock function based on the
    association list [l] whose couples are [(input_i, output_i)]
    indicating that the [i]th call to the function should have parameter
    [input_i] and should return [output_i]. [cmp] (defaulting to
    [Pervasives.compare]) is used to compare input values), while [prn]
    is used to convert input values into strings.

    Raises [Unexpected_value] if the function received as the [i]th input
    value a value that is different from [fst (List.nth l i)].

    Raises [End_of_sequence] if more than [List.length l] calls. *)

val from_function : ?cmp:('a -> 'a -> int) -> ('a -> 'b) -> ('a, 'b) t
(** [from_function ~cmp f] returns a mock function based on the
    function [f], only recording calls actually made to the function.
    [cmp] (defaulting to [Pervasives.compare]) is used to compare input
    values).

    [f] should raise [Unexpected_value] if presented an input value it
    cannot handle. *)


(** {6 Mock functions} *)

val func : ('a, 'b) t -> 'a -> 'b
(** Returns the function bare to be used for tests. *)

val func2 : (('a1 * 'a2), 'b) t -> 'a1 -> 'a2 -> 'b
(** Returns the function bare to be used for tests, as a binary
    function. *)

val func3 : (('a1 * 'a2 * 'a3), 'b) t -> 'a1 -> 'a2 -> 'a3 -> 'b
(** Returns the function bare to be used for tests, as a ternary
    function. *)

val func4 : (('a1 * 'a2 * 'a3 * 'a4), 'b) t -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b
(** Returns the function bare to be used for tests, as a quaternary
    function. *)

val func5 : (('a1 * 'a2 * 'a3 * 'a4 * 'a5), 'b) t -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b
(** Returns the function bare to be used for tests, as a quinary
    function. *)

val count : ('a, 'b) t -> 'a -> int
(** [count f x] returns how many time the function [f] has been called
    with a parameter value equal to [x]. *)

val total : ('a, 'b) t -> int
(** Returns how many times the function [f] has been called. *)

val calls : ('a, 'b) t -> 'a list
(** Returns the list of parameter values passed to the function. *)
