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

(** This module provides functions generating values. *)


(** {6 Random source} *)

type random = Random.State.t
(** The type of random sources. *)

val make_random : unit -> random
(** Creates a new random source. *)

val make_random_seed : int -> random
(** Creates a new random source, using the passed value as the seed. *)

val make_random_full : int array -> random
(** Creates a new random source, using the passed value as the seed. *)


(** {6 Generator definition} *)

type 'a t = (random -> 'a) * ('a -> string)
(** The type of generators, a couple of functions where:
    - the first function generates random values for a given type;
    - the second function converts values from this type into strings. *)


(** {6 Predefined generators} *)

val unit : unit t
(** Dummy generator for [unit] value. *)

val bool : bool t
(** Generator for [bool] values. *)

val make_bool : int -> int -> bool t
(** [make_bool w1 w2] constructs a generator for [bool] values.
    [w1] and [w2] are the weights for respectively [true] and [false].
    Raises [Invalid_arg] if either [w1] or [w2] is negative. *)

val int : int t
(** Generator for [int] values. *)

val pos_int : int t
(** Generator for positive [int] values. *)

val neg_int : int t
(** Generator for negative [int] values. *)

val make_int : int -> int -> int t
(** [make_int m n] constructs a generator for [int] values.
    Generated values are between [m] (inclusive) and [n] (exclusive).
    Raises [Invalid_arg] if [m >= n]. *)

val int32 : int32 t
(** Generator for [int32] values. *)

val pos_int32 : int32 t
(** Generator for positive [int32] values. *)

val neg_int32 : int32 t
(** Generator for negative [int32] values. *)

val make_int32 : int32 -> int32 -> int32 t
(** [make_int32 m n] constructs a generator for [int32] values.
    Generated values are between [m] (inclusive) and [n] (exclusive).
    Raises [Invalid_arg] if [m >= n]. *)

val int64 : int64 t
(** Generator for [int64] values. *)

val pos_int64 : int64 t
(** Generator for positive [int64] values. *)

val neg_int64 : int64 t
(** Generator for negative [int64] values. *)

val make_int64 : int64 -> int64 -> int64 t
(** [make_int64 m n] constructs a generator for [int64] values.
    Generated values are between [m] (inclusive) and [n] (exclusive).
    Raises [Invalid_arg] if [m >= n]. *)

val nativeint : nativeint t
(** Generator for [nativeint] values. *)

val pos_nativeint : nativeint t
(** Generator for positive [nativeint] values. *)

val neg_nativeint : nativeint t
(** Generator for negative [nativeint] values. *)

val make_nativeint : nativeint -> nativeint -> nativeint t
(** [make_nativeint m n] constructs a generator for [nativeint] values.
    Generated values are between [m] (inclusive) and [n] (exclusive).
    Raises [Invalid_arg] if [m >= n]. *)

val char : char t
(** Generator for [char] values. *)

val digit : char t
(** Generator for [char] values representing (decimal) digits. *)

val digit_bin : char t
(** Generator for [char] values representing (binary) digits. *)

val digit_oct : char t
(** Generator for [char] values representing (octal) digits. *)

val digit_hex : char t
(** Generator for [char] values representing (hexadecimal) digits. *)

val letter : char t
(** Generator for [char] values representing letters. *)

val alphanum : char t
(** Generator for [char] values representing alphanumeric characters
    ({i i.e.} letters, decimal digits, as well as underscores). *)

val string : int t -> char t -> string t
(** [string i c] constructs a generator for [string] values.
    [i] is the generator used to determine the string length,
    while [c] is the generator used to generate characters. *)

val strings : string -> int t -> string t -> string t
(** [strings sep i s] constructs a generator for [string] values.
    The generated strings are the concatenation of strings generated
    by [s], separated by [sep]. The number of strings is determined
    by [i]. *)

val number : int t -> string t
(** [number n] constructs a generator for [string] values representing
    numbers. [n] is used to determine the number of (decimal) digits. *)

val number_bin : int t -> string t
(** [number_bin n] constructs a generator for [string] values
    representing numbers. [n] is used to determine the number of (binary)
    digits. *)

val number_oct : int t -> string t
(** [number_oct n] constructs a generator for [string] values representing
    numbers. [n] is used to determine the number of (octal) digits. *)

val number_hex : int t -> string t
(** [number_hex n] constructs a generator for [string] values representing
    numbers. [n] is used to determine the number of (hexadecimal) digits. *)

val word : int t -> string t
(** [word n] constructs a generator for [string] values representing words.
    [n] is used to determine the number of letters. *)

val words : int t -> int t -> string t
(** [words n p] constructs a generator for [string] values representing
    sequences of words. [n] is used to determine the number of words,
    while [p] is used to determine the number of letters for a word.
    Words are separated by single whitespaces. *)

val float : float t
(** Generator for [float] values. *)

val make_float : float -> float -> float t
(** [make_float f g] constructs a generator for [float] values.
    Generated values are between [f] (inclusive) and [g] (exclusive). *)

val complex : float t -> float t -> Complex.t t
(** [complex r i] constructs a generator for [Complex.t] values.
    [r] is the generator used to generate the real part,
    while [i] is the generator used to generate the imaginary part. *)


(** {6 Generators for functions} *)

type 'a outcome =
  | Value of 'a (** The function returns a bare value. *)
  | Exception of exn (** The function raises an exception. *)
(** The type of function outcomes. *)

val total_function : 'b t -> ('a -> 'b) t
(* [total_function r] constructs a generator for functions of codomain
   (a.k.a. range) ['b] whose values are generated by [r]. A hash table
   is used to keep the image of already passed elements, in such a way
   that the generated function will always return the same value for
   the same input. *)

val partial_function : 'b outcome t -> ('a -> 'b) t
(* [partial_function] is similar to [total_function] except that the
   passed generator can either generate bare values or exceptional
   values. *)


(** {6 Generators for containers} *)

val array : int t -> 'a t -> ('a array) t
(** [array l e] constructs a generator for arrays.
    [l] is used to determine the array length,
    while [e] is used to generate the array elements. *)

val list : int t -> 'a t -> ('a list) t
(** [list l e] constructs a generator for lists.
    [l] is used to determine the list length,
    while [e] is used to generate the list elements. *)

val option : bool t -> 'a t -> ('a option) t
(** [option b e] constructs a generator for option values.
    [b] is used to determine if the option is [None] (when [b] returns
    [false]), while [e] is used to generate the embedded element. *)

val ref : 'a t -> ('a ref) t
(** [ref e] constructs a generator for reference values.
    [e] is used to generate the embedded element. *)

val buffer : string t -> Buffer.t t
(** [buffer b] constructs a generator for [Buffer.t] values.
    [b] is used to generate the embedded element. *)

module type Gen = sig
  type g
  (** The type of generated values. *)

  val g : g t
  (** Actual generator for [g] values. *)
end
(** Module type used for functor-based generators. *)

module Map (M : Map.S) (G : Gen with type g = M.key) : sig
  val gen : int t -> 'a t -> 'a M.t t
  (** [gen i g] constructs a generator for maps from [G.g] to ['a].
      [i] is the generator used to determine the map size,
      while [g] is used to generate ['a] values. *)
end
(** Functor used to build generators for [Map.S.t] values. *)

module Set (S : Set.S) (G : Gen with type g = S.elt) : sig
  val gen : int t -> S.t t
  (** [gen i] constructs a generator for sets of [G.g] values.
      [i] is the generator used to determine the set size. *)
end
(** Functor used to build generators for [Set.S.t] values. *)

val hashtbl : int t -> 'a t -> 'b t -> ('a, 'b) Hashtbl.t t
(** [hashtbl s k v] constructs a generator for [Hashtbl.t] values.
    [s] is used to determine the size of the hash table, while
    [k] and [v] are respectively used to generate keys and values. *)

val queue : int t -> 'a t -> 'a Queue.t t
(** [queue s e] constructs a generator for [Queue.t] values.
    [s] is used to determine the size of the queue, while [e] is used
    to generate elements. *)

val stack : int t -> 'a t -> 'a Stack.t t
(** [stack s e] constructs a generator for [Stack.t] values.
    [s] is used to determine the size of the stack, while [e] is used
    to generate elements. *)

val weak : int t -> 'a option t -> 'a Weak.t t
(** [weak s e] constructs a generator for [Weak.t] values.
    [s] is used to determine the size of the weak array, while [e] is
    used to generate elements. *)

module Weak (W : Weak.S) (G : Gen with type g = W.data) : sig
  val gen : int t -> W.t t
  (** [gen i] constructs a generator for weak hashtables of [G.g] values.
      [i] is the generator used to determine the hashtable size. *)
end
(** Functor used to build generators for [Weak.S.t] values. *)


(** {6 Combinators over generators} *)

val lift : 'a -> string -> 'a t
(** [lift e s] constructs a generator that always returns [e],
    [s] is the string representation of [e]. *)

val select_list : 'a list -> ('a -> string) -> 'a t
(** [select_list l f] constructs a generator that returns an element of
    [l]. [f] is used to converts values into strings.
    Raises [Invalid_argument] if [l] is empty. *)

val select_list_weighted : ('a * int) list -> ('a -> string) -> 'a t
(** [select_list_weighted l f] constructs a generator that returns an
    element of [l]. [f] is used to converts values into strings. The
    integers of [l] are interpreted as the weights of the associated
    elements.
    Raises [Invalid_argument] if [l] is empty or if a weight is negative. *)

val select_array : 'a array -> ('a -> string) -> 'a t
(** [select_array a f] constructs a generator that returns an element of
    [a]. [f] is used to converts values into strings.
    Raises [Invalid_argument] if [a] is empty. *)

val select_array_weighted : ('a * int) array -> ('a -> string) -> 'a t
(** [select_array_weighted a f] constructs a generator that returns an
    element of [a]. [f] is used to converts values into strings.
    The integers of [a] are interpreted as the weights of the associated
    elements.
    Raises [Invalid_argument] if [a] is empty or if a weight is negative. *)

val choose_list : ('a t) list -> 'a t
(** [choose_list l] constructs a generator that returns an element by
    first selecting a generator from [l] and using it to generate the
    actual value.
    Raises [Invalid_argument] if [l] is empty. *)

val choose_list_weighted : (('a t) * int) list -> 'a t
(** [choose_list_weighted l] constructs a generator that returns an
    element by first selecting a generator from [l] and using it to
    generate the actual value. The integers of [l] are interpreted as the
    weights of the associated elements.
    Raises [Invalid_argument] if [l] is empty or if a weight is negative. *)

val choose_array : ('a t) array -> 'a t
(** [choose_array a] constructs a generator that returns an element by
    first selecting a generator from [a] and using it to generate the
    actual value.
    Raises [Invalid_argument] if [a] is empty. *)

val choose_array_weighted : (('a t) * int) array -> 'a t
(** [choose_array_weigthed a] constructs a generator that returns an
    element by first selecting a generator from [a] and using it to
    generate the actual value. The integers of [a] are interpreted as the
    weights of the associated elements.
    Raises [Invalid_argument] if [a] is empty or if a weight is negative. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f g] constructs a generator equivalent to [g] filtered by
    [f], that is skips all values [x] such that [f x] returns [false]. *)

val transform : ('a -> 'a) -> 'a t -> 'a t
(** [transform f g] constructs a generator applying the function [f]
    to the values generated by [g]. *)

val map1 : ('a -> 'b) -> ('b -> string) -> 'a t -> 'b t
(** [map1 f p g] constructs a generator applying the function [f]
    to the values generated by [g]. [p] is used to convert the
    values generated by the returned generator into strings. *)

val map2 : ('a -> 'b -> 'c) -> ('c -> string) -> (('a t) * ('b t)) -> 'c t
(** [map2 f p (g1, g2)] constructs a generator applying the function [f]
    to the values generated by [g1] and [g2]. [p] is used to convert the
    values generated by the returned generator into strings. *)

val map3 : ('a -> 'b -> 'c -> 'd) -> ('d -> string) -> (('a t) * ('b t) * ('c t)) -> 'd t
(** [map3 f p (g1, g2, g3)] constructs a generator applying the function
    [f] to the values generated by [g1], [g2] and [g3]. [p] is used to
    convert the values generated by the returned generator into strings. *)

val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('e -> string) -> (('a t) * ('b t) * ('c t) * ('d t)) -> 'e t
(** [map4 f p (g1, g2, g3, g4)] constructs a generator applying the
    function [f] to the values generated by [g1], [g2], [g3] and [g4].
    [p] is used to convert the values generated by the returned generator
    into strings. *)

val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('f -> string) -> (('a t) * ('b t) * ('c t) * ('d t) * ('e t)) -> 'f t
(** [map5 f p (g1, g2, g3, g4, g5)] constructs a generator applying the
    function [f] to the values generated by [g1], [g2], [g3], [g4]
    and [g5]. [p] is used to convert the values generated by the returned
    generator into strings. *)

val zip1 : 'a t -> 'a t
(** The identity function. *)

val zip2 : 'a t -> 'b t -> ('a * 'b) t
(** [zip2 g1 g2] zips [g1] and [g2] into a generator producing couple
    values. *)

val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [zip3 g1 g2 g3] zips [g1], [g2] and [g3] into a generator producing
    triple values. *)

val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [zip4 g1 g2 g3 g4] zips [g1], [g2], [g3] and [g4] into a generator
    producing quadruple values. *)

val zip5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
(** [zip5 g1 g2 g3 g4 g5] zips [g1], [g2], [g3], [g4] and [g5] into a
    generator producing quintuple values. *)


(** {6 Currying functions} *)

val apply1 : ('a -> 'b) -> 'a -> 'b
(** [apply1 f x] is equivalent to [f x]. *)

val apply2 : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
(** [apply2 f (x, y)] is equivalent to [f x y]. *)

val apply3 : ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) -> 'd
(** [apply3 f (x, y, z)] is equivalent to [f x y z]. *)

val apply4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('a * 'b * 'c * 'd) -> 'e
(** [apply4 f (x, y, z, t)] is equivalent to [f x y z t]. *)

val apply5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('a * 'b * 'c * 'd * 'e) -> 'f
(** [apply5 f (x, y, z, t, u)] is equivalent to [f x y z t u]. *)

val tuple_apply1 : ('a -> 'b) -> ('a -> 'b)
(** [tuple_apply1 f x] is equivalent to [f x]. *)

val tuple_apply2 : (('a * 'b) -> 'c) -> ('a -> 'b -> 'c)
(** [tuple_apply2 f x y] is equivalent to [f (x, y)]. *)

val tuple_apply3 : (('a * 'b * 'c) -> 'd) -> ('a -> 'b -> 'c -> 'd)
(** [tuple_apply3 f x y z] is equivalent to [f (x, y, z)]. *)

val tuple_apply4 : (('a * 'b * 'c * 'd) -> 'e) -> ('a -> 'b -> 'c -> 'd -> 'e)
(** [tuple_apply4 f x y z t] is equivalent to [f (x, y, z, t)]. *)

val tuple_apply5 : (('a * 'b * 'c * 'd * 'e) -> 'f) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
(** [tuple_apply5 f x y z t u] is equivalent to [f (x, y, z, t, u)]. *)
