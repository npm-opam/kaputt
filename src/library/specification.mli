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

(** This module provides type definitions, base functions, and combinators
    allowing to encode specifications. *)


(** {6 Type definitions} *)

type 'a predicate = 'a -> bool
(** The type of unary predicates. *)

type ('a, 'b) t = {
    precond : 'a predicate;
    postcond : ('a * 'b) predicate
  }
(** The type of specifications, that is: a precondition and a postcondition:
    - ['a] is the type for the domain of the function to be tested;
    - ['b] is the type for the codomain (or range) of the function to be tested.
    The precondition is thus evaluated over values of the domain. The postcondition
    is evaluated over couples where the first element is a value of the domain
    while the second one is its image through the tested function. *)

val implies : 'a predicate -> ('a * 'b) predicate -> ('a, 'b) t
(** [implies p1 p2] is equivalent to [{ precond = p1; postcond = p2 }]. *)

val (=>) : 'a predicate -> ('a * 'b) predicate -> ('a, 'b) t
(** Shorthand for [implies]. *)

val implies' : 'a predicate -> 'b predicate -> ('a, 'b) t
(** [implies' p1 p2] is a simplified version of [implies] where the predicate
    of the postcondition is only applied to the function result. *)

val (==>) : 'a predicate -> 'b predicate -> ('a, 'b) t
(** Shorthand for [implies']. *)

type 'a outcome =
  | Result of 'a (** Encodes the result returned by the function. *)
  | Exception of exn (** Encodes the exception raised by the function. *)
(** The type used to model partial functions, that is functions that may
    either return a value or raise an exception. *)

val is_exception : exn predicate -> 'a outcome predicate
(** [is_exception p] returns a predicate that ensures that the outcome is
    an exception satisfying predicate [p]. *)

val is_result : 'a predicate -> 'a outcome predicate
(** [is_result p] returns a predicate that ensures that the outcome is
    a result satisfying predicate [p]. *)


(** {6 Predifined predicates} *)

val always : 'a predicate
(** Predicate that always evaluates to [true]. *)

val never : 'a predicate
(** Predicate that always evaluates to [false]. *)

val is_pos_int : int predicate
(** Predicate testing whether the [int] argument is greater than or equal to zero. *)

val is_neg_int : int predicate
(** Predicate testing whether the [int] argument is lower than or equal to zero. *)

val is_zero_int : int predicate
(** Predicate testing whether the [int] argument is equal to zero. *)

val is_nonzero_int : int predicate
(** Predicate testing whether the [int] argument is different from zero. *)

val is_even_int : int predicate
(** Predicate testing whether the [int] argument is even. *)

val is_odd_int : int predicate
(** Predicate testing whether the [int] argument is odd. *)

val is_pos_int32 : int32 predicate
(** Predicate testing whether the [int32] argument is greater than or equal to zero. *)

val is_neg_int32 : int32 predicate
(** Predicate testing whether the [int32] argument is lower than or equal to zero. *)

val is_zero_int32 : int32 predicate
(** Predicate testing whether the [int32] argument is equal to zero. *)

val is_nonzero_int32 : int32 predicate
(** Predicate testing whether the [int32] argument is different from zero. *)

val is_even_int32 : int32 predicate
(** Predicate testing whether the [int32] argument is even. *)

val is_odd_int32 : int32 predicate
(** Predicate testing whether the [int32] argument is odd. *)

val is_pos_int64 : int64 predicate
(** Predicate testing whether the [int64] argument is greater than or equal to zero. *)

val is_neg_int64 : int64 predicate
(** Predicate testing whether the [int64] argument is lower than or equal to zero. *)

val is_zero_int64 : int64 predicate
(** Predicate testing whether the [int64] argument is equal to zero. *)

val is_nonzero_int64 : int64 predicate
(** Predicate testing whether the [int64] argument is different from zero. *)

val is_even_int64 : int64 predicate
(** Predicate testing whether the [int64] argument is even. *)

val is_odd_int64 : int64 predicate
(** Predicate testing whether the [int64] argument is odd. *)

val is_pos_nativeint : nativeint predicate
(** Predicate testing whether the [nativeint] argument is greater than or equal to zero. *)

val is_neg_nativeint : nativeint predicate
(** Predicate testing whether the [nativeint] argument is lower than or equal to zero. *)

val is_zero_nativeint : nativeint predicate
(** Predicate testing whether the [nativeint] argument is equal to zero. *)

val is_nonzero_nativeint : nativeint predicate
(** Predicate testing whether the [nativeint] argument is different from zero. *)

val is_even_nativeint : nativeint predicate
(** Predicate testing whether the [nativeint] argument is even. *)

val is_odd_nativeint : nativeint predicate
(** Predicate testing whether the [nativeint] argument is odd. *)

val is_pos_float : float predicate
(** Predicate testing whether the [float] argument is greater than or equal to zero. *)

val is_neg_float : float predicate
(** Predicate testing whether the [float] argument is lower than or equal to zero. *)

val is_zero_float_eps : float -> float predicate
(** Predicate testing whether the [float] argument is equal to zero.
    The parameter is the epsilon used for comparison. *)

val is_nonzero_float_eps : float -> float predicate
(** Predicate testing whether the [float] argument is different from zero.
    The parameter is the epsilon used for comparison. *)

val is_zero_float : float predicate
(** Predicate testing whether the [float] argument is equal to zero.
    The epsilon used for comparison is [epsilon_float]. *)

val is_nonzero_float : float predicate
(** Predicate testing whether the [float] argument is different from zero.
    The epsilon used for comparison is [epsilon_float]. *)

val is_nan_float : float predicate
(** Predicate testing whether the [float] argument is equal to [nan]. *)

val is_nonnan_float : float predicate
(** Predicate testing whether the [float] argument is different from [nan]. *)

val is_letter_char : char predicate
(** Predicate testing whether the [char] argument is a letter. *)

val is_digit_char : char predicate
(** Predicate testing whether the [char] argument is a (decimal) digit. *)

val is_digit_bin_char : char predicate
(** Predicate testing whether the [char] argument is a (binary) digit. *)

val is_digit_oct_char : char predicate
(** Predicate testing whether the [char] argument is a (octal) digit. *)

val is_digit_hex_char : char predicate
(** Predicate testing whether the [char] argument is a (hexadecimal) digit. *)

val is_space_char : char predicate
(** Predicate testing whether the [char] argument is either a bare space or a tabulation. *)

val is_alphanum_char : char predicate
(** Predicate testing whether the [char] argument is either an underscore or a letter/digit. *)

val is_empty_string : string predicate
(** Predicate testing whether the [string] argument is empty. *)

val is_nonempty_string : string predicate
(** Predicate testing whether the [string] argument is not empty. *)

val is_empty_list : ('a list) predicate
(** Predicate testing whether the [list] argument is empty. *)

val is_nonempty_list : ('a list) predicate
(** Predicate testing whether the [list] argument is not empty. *)

val is_empty_array : ('a array) predicate
(** Predicate testing whether the [array] argument is empty. *)

val is_nonempty_array : ('a array) predicate
(** Predicate testing whether the [array] argument is not empty. *)

val is_none_option : ('a option) predicate
(** Predicate testing whether the [option] argument is equal to [None]. *)

val is_some_option : ('a option) predicate
(** Predicate testing whether the [option] argument is different from [None]. *)


(** {6 Predicates over containers} *)

val exists_string : char predicate -> string predicate
(** [exists_string p] constructs a predicate that evaluates to [true]
    iff any of the characters makes [p] evaluate to [true]. *)

val for_all_string : char predicate -> string predicate
(** [for_all_string p] constructs a predicate that evaluates to [true]
    iff all of the characters make [p] evaluate to [true]. *)

val exists_list : 'a predicate -> ('a list) predicate
(** [exists_list p] constructs a predicate that evaluates to [true]
    iff any of the elements makes [p] evaluate to [true]. *)

val for_all_list : 'a predicate -> ('a list) predicate
(** [for_all_list p] constructs a predicate that evaluates to [true]
    iff all of the elements make [p] evaluate to [true]. *)

val exists_array : 'a predicate -> ('a array) predicate
(** [exists_array p] constructs a predicate that evaluates to [true]
    iff any of the elements makes [p] evaluate to [true]. *)

val for_all_array : 'a predicate -> ('a array) predicate
(** [for_all_array p] constructs a predicate that evaluates to [true]
    iff all of the elements make [p] evaluate to [true]. *)

module type Pred = sig
  type p
  (** The type of predicate parameters. *)

  val p : p predicate
  (** Actual predicate over [p] values. *)
end
(** Module type used for functor-based predicates. *)

module Map (M : Map.S) (P : Pred with type p = M.key) : sig
  val exists : 'a predicate -> 'a M.t predicate
  (** [exists p] constructs a predicate that evaluates to [true]
      iff any of the map element [(k, v)] makes [P.p k && p v] evaluate
      to [true]. *)

  val for_all : 'a predicate -> 'a M.t predicate
  (** [for_all p] constructs a predicate that evaluates to [true]
      iff all of the map elements [(k, v)] make [P.p k && p v] evaluate
      to [true]. *)
end
(** Functor used to build predicates for [Map.S.t] values. *)

module Set (S : Set.S) (P : Pred with type p = S.elt) : sig
  val exists : S.t predicate
  (** Predicates that evaluates to [true] iff any of the set elements
      makes [P.p] evaluate to [true]. *)

  val for_all : S.t predicate
  (** Predicates that evaluates to [true] iff all of the set elements
      make [P.p] evaluate to [true]. *)
end
(** Functor used to build predicates for [Set.S.t] values. *)

val exists_hashtbl : ('a * 'b) predicate -> (('a, 'b) Hashtbl.t) predicate
(** [exists_hashtbl p] constructs a predicate that evaluates to [true]
    iff any of the elements makes [p] evaluate to [true]. *)

val for_all_hashtbl : ('a * 'b) predicate -> (('a, 'b) Hashtbl.t) predicate
(** [for_all_hashtbl p] constructs a predicate that evaluates to [true]
    iff all of the elements make [p] evaluate to [true]. *)

val exists_queue : 'a predicate -> ('a Queue.t) predicate
(** [exists_queue p] constructs a predicate that evaluates to [true]
    iff any of the elements makes [p] evaluate to [true]. *)

val for_all_queue : 'a predicate -> ('a Queue.t) predicate
(** [for_all_queue p] constructs a predicate that evaluates to [true]
    iff all of the elements make [p] evaluate to [true]. *)

val exists_stack : 'a predicate -> ('a Stack.t) predicate
(** [exists_stack p] constructs a predicate that evaluates to [true]
    iff any of the elements makes [p] evaluate to [true]. *)

val for_all_stack : 'a predicate -> ('a Stack.t) predicate
(** [for_all_stack p] constructs a predicate that evaluates to [true]
    iff all of the elements make [p] evaluate to [true]. *)

val exists_weak : ('a option) predicate -> ('a Weak.t) predicate
(** [exists_weak p] constructs a predicate that evaluates to [true]
    iff any of the elements makes [p] evaluate to [true]. *)

val for_all_weak : ('a option) predicate -> ('a Weak.t) predicate
(** [for_all_weak p] constructs a predicate that evaluates to [true]
    iff all of the elements make [p] evaluate to [true]. *)

module Weak (W : Weak.S) (P : Pred with type p = W.data) : sig
  val exists : W.t predicate
  (** Predicates that evaluates to [true] iff any of the weak hashtable
      elements makes [P.p] evaluate to [true]. *)

  val for_all : W.t predicate
  (** Predicates that evaluates to [true] iff all of the weak hashtable
      elements make [P.p] evaluate to [true]. *)
end
(** Functor used to build predicates for [Weak.S.t] values. *)


(** {6 Combinators over predicates} *)

val logand : 'a predicate -> 'a predicate -> 'a predicate
(** Constructs a predicate that is the conjunction of the passed ones. *)

val (&&&) : 'a predicate -> 'a predicate -> 'a predicate
(** Shorthand for [logand]. *)

val logand_list : 'a predicate list -> 'a predicate
(** Constructs a predicate that is the conjunction of the passed ones. *)

val (&&&&) : 'a predicate list -> 'a predicate
(** Shorthand for [logand_list]. *)

val logor : 'a predicate -> 'a predicate -> 'a predicate
(** Constructs a predicate that is the disjunction of the passed ones. *)

val (|||) : 'a predicate -> 'a predicate -> 'a predicate
(** Shorthand for [logor]. *)

val logor_list : 'a predicate list -> 'a predicate
(** Constructs a predicate that is the disjunction of the passed ones. *)

val (||||) : 'a predicate list -> 'a predicate
(** Shorthand for [logor_list]. *)

val logxor : 'a predicate -> 'a predicate -> 'a predicate
(** Constructs a predicate that is the exclusive disjunction of the passed ones. *)

val (^^^) : 'a predicate -> 'a predicate -> 'a predicate
(** Shorthand for [logxor]. *)

val logxor_list : 'a predicate list -> 'a predicate
(** Constructs a predicate that is the exclusive disjunction of the passed ones. *)

val (^^^^) : 'a predicate list -> 'a predicate
(** Shorthand for [logxor_list]. *)

val not : 'a predicate -> 'a predicate
(** Constructs a predicate that is the negation of the passed one. *)

val zip1 : 'a predicate -> 'a predicate
(** Identity function. *)

val zip2 : 'a predicate -> 'b predicate -> ('a * 'b) predicate
(** [zip2 p1 p2] zips [p1] and [p2] into a predicate over couples. *)

val zip3 : 'a predicate -> 'b predicate -> 'c predicate -> ('a * 'b * 'c) predicate
(** [zip3 p1 p2 p3] zips [p1], [p2] and [p3] into a predicate over triples. *)

val zip4 : 'a predicate -> 'b predicate -> 'c predicate -> 'd predicate -> ('a * 'b * 'c * 'd) predicate
(** [zip4 p1 p2 p3 p4] zips [p1], [p2], [p3] and [p4] into a predicate over quadruples. *)

val zip5 : 'a predicate -> 'b predicate -> 'c predicate -> 'd predicate -> 'e predicate -> ('a * 'b * 'c * 'd * 'e) predicate
(** [zip5 p1 p2 p3 p4 p5] zips [p1], [p2], [p3], [p4] and [p5] into a predicate over quintuples. *)

(**/**)

val create_int_functions : ('a -> 'a -> int) -> 'a -> ('a -> 'a -> 'a) -> 'a ->
  ('a predicate * 'a predicate *'a predicate * 'a predicate * 'a predicate * 'a predicate)
