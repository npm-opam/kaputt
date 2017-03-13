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

(** This module provides functions for the 'bigarray' library. *)


(** {6 Generator} *)

module Generator : sig

  val bigarray : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> int array Kaputt.Generator.t -> 'a Kaputt.Generator.t -> ('a, 'b, 'c) Bigarray.Genarray.t Kaputt.Generator.t
  (** [bigarray k l d e] constructs a generator for [Bigarray.Genarray.t] values.
      [k] is the kind of generated arrays and [l] is the layout of generated arrays.
      [d] is used to determine the dimensions of the array, while [e] is used to
      generate elements. *)

end


(** {6 Enumerator} *)

module Enumerator : sig

  val bigarray : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> int array -> 'a Kaputt.Enumerator.t -> ('a, 'b, 'c) Bigarray.Genarray.t Kaputt.Enumerator.t
  (** [bigarray k l dims e] constructs an enumerator for [Bigarray.Genarray.t]
      values of kind [k], layout [l], and dimensions [dims]. [e] is used to
      enumerate elements. *)

end


(** {6 Assertion} *)

module Assertion : sig

  val make_equal_bigarray : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> ('a, 'b, 'c) Bigarray.Genarray.t -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
  (** [make_equal_bigarray eq prn] returns a function for testing equality of
      big arrays. [eq] is used to compare elements, while [prn] is used to
      print them. *)

  val make_not_equal_bigarray : ('a -> 'a -> bool) -> ('a -> string) -> ?msg:string -> ('a, 'b, 'c) Bigarray.Genarray.t -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
  (** [make_not_equal_bigarray eq prn] returns a function for testing inequality
      of big arrays. [eq] is used to compare elements, while [prn] is used to
      print them. *)

end


(** {6 Specification} *)

module Specification : sig

  val is_empty_bigarray : (('a, 'b, 'c) Bigarray.Genarray.t) Kaputt.Specification.predicate
  (** Predicate testing whether the [Bigarray.Genarray.t] argument is empty. *)

  val is_nonempty_bigarray : (('a, 'b, 'c) Bigarray.Genarray.t) Kaputt.Specification.predicate
  (** Predicate testing whether the [Bigarray.Genarray.t] argument is not empty. *)

  val exists_bigarray : 'a Kaputt.Specification.predicate -> (('a, 'b, 'c) Bigarray.Genarray.t) Kaputt.Specification.predicate
  (** [exists_bigarray p] constructs a predicate that evaluates to [true]
      iff any of the elements makes [p] evaluate to [true]. *)

  val for_all_bigarray : 'a Kaputt.Specification.predicate -> (('a, 'b, 'c) Bigarray.Genarray.t) Kaputt.Specification.predicate
  (** [for_all_bigarray p] constructs a predicate that evaluates to [true]
      iff all of the elements make [p] evaluate to [true]. *)

end


(** {6 Reducer} *)

module Reducer : sig

  val bigarray_c : (('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t) Kaputt.Reducer.t
  (** Reducer for [Bigarray.Genarray.t] values. *)

  val bigarray_fortran : (('a, 'b, Bigarray.fortran_layout) Bigarray.Genarray.t) Kaputt.Reducer.t
  (** Reducer for [Bigarray.Genarray.t] values. *)

end
