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

(** This module provides functions enumerating values. *)


(** {6 Enumerator definition} *)

type 'a lazy_list = Nil | Cons of 'a * ('a lazy_list lazy_t)
(** The type of lazy lists. *)

type 'a t = (unit -> 'a lazy_list) * ('a -> string)
(** The type of enumerators, a couple of functions where:
    - the first function returns a lazy list used to enumerate values for
      a given type (calls should return equal lists);
    - the second function converts values from this type into strings. *)


(** {6 Utility functions} *)

val iter_list : ('a -> unit) -> 'a lazy_list -> unit
(** [iter_list f l] applies [f] to the elements of [l]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f e] applies [f] to the elements of [e]. *)


(** {6 Predefined enumerators} *)

val empty : 'a t
(** Dummy empty enumerator. *)

val unit : unit t
(** Dummy enumerator for [unit] value. *)

val bool : bool t
(** Enumerator for [bool] values. *)

val int : int -> int -> int t
(** [int x y] constructs an enumerator for [int] values between [x] and [y]
    (both inclusive). *)

val int32 : int32 -> int32 -> int32 t
(** [int32 x y] constructs an enumerator for [int32] values between [x] and [y]
    (both inclusive). *)

val int64 : int64 -> int64 -> int64 t
(** [int64 x y] constructs an enumerator for [int64] values between [x] and [y]
    (both inclusive). *)

val nativeint : nativeint -> nativeint -> nativeint t
(** [nativeint x y] constructs an enumerator for [nativeint] values between
    [x] and [y] (both inclusive). *)

val make_char : char -> char -> char t
(** [make_char x y] constructs an enumerator for [char] values between [x] and [y]
    (both inclusive). *)

val char : char t
(** Enumerator for [char] values. *)

val string : char t -> int -> string t
(** [string ch len] constructs an enumerator for [string] values of length [len],
    using [ch] to generate the characters. *)

val float : float -> float -> int -> float t
(** [float x y steps] constructs an enumerator for [float] values between [x]
    and [y], producing [steps] values. [steps] should be above of equal to [2]
    except if [x] equal [y], raising [Invalid_argument] otherwise. *)

val complex : float t -> float t -> Complex.t t
(** [complex re im] constructs an enumerator for [Complex.t] values, using [re]
    and [im] to respectively generate the real and imaginary parts. *)


(** {6 Enumerators for containers} *)

val array : 'a t -> int -> 'a array t
(** [array e l] constructs an enumerator for arrays of length [l],
    [e] is used to generate the array elements. *)

val list : 'a t -> int -> 'a list t
(** [list e l] constructs an enumerator for lists of length [l],
    [e] is used to generate the list elements. *)

val option : 'a t -> 'a option t
(** [option e] constructs an enumerator for option values.
    [e] is used to generate the embedded element. *)

val ref : 'a t -> 'a ref t
(** [ref e] constructs an enumerator for reference values.
    [e] is used to generate the embedded element. *)

val buffer : char t -> int -> Buffer.t t
(** [buffer e l] constructs an enumerator for buffers of length [l],
    [e] is used to generate the buffer elements. *)

val queue : 'a t -> int -> 'a Queue.t t
(** [queue e l] constructs an enumerator for queues of length [l],
    [e] is used to generate the queue elements. *)

val stack : 'a t -> int -> 'a Stack.t t
(** [stack e l] constructs an enumerator for stacks of length [l],
    [e] is used to generate the stack elements. *)

val weak : 'a option t -> int -> 'a Weak.t t
(** [array e l] constructs an enumerator for weak arrays of length [l],
    [e] is used to generate the array elements. *)


(** {6 Enumerators for files} *)

val file_chars : string -> char t
(** [file_chars fn] constructs an enumerator returning the characters
    from file [fn]. Raises [Sys_error] if an i/o error occurs. *)

val file_bytes : string -> int t
(** [file_bytes fn] constructs an enumerator returning the bytes
    from file [fn]. Raises [Sys_error] if an i/o error occurs. *)

val file_lines : string -> string t
(** [file_lines fn] constructs an enumerator returning the lines
    from file [fn]. Raises [Sys_error] if an i/o error occurs. *)

val file_values : string -> ('a -> string) -> 'a t
(** [file_values fn p] constructs an enumerator returning the values
    (as stored by marshalling) from file [fn], using [p] to convert
    elements into their string representation. Raises [Sys_error]
    if an i/o error occurs. *)


(** {6 Combinators over enumerators} *)

val lift : 'a -> string -> 'a t
(** [lift e s] constructs an enumerator that only returns [e],
    [s] is the string representation of [e]. *)

val lift_list : 'a list -> ('a -> string) -> 'a t
(** [lift_list l p] constructs an enumerator that returns the elements
    from [l], using [p] to convert elements into their string representation. *)

val lift_array : 'a array -> ('a -> string) -> 'a t
(** [lift_array a p] constructs an enumerator that returns the elements
    from [a], using [p] to convert elements into their string representation. *)

val lift_string : string -> char t
(** [lift_string s] constructs an enumerator that returns the characters
    from [s]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f e] constructs an enumerator equivalent to [e] filtered by [f],
    that is skips all values [x] such that [f x] returns [false]. *)

val transform : ('a -> 'a) -> 'a t -> 'a t
(** [transform f e] constructs an enumerator applying the function [f] to the
    values enumerated by [e]. *)

val sequence : 'a t list -> 'a t
(** [sequence l] constructs an enumerator that is the {i concatenation} of the
    enumerators of [l]. *)

val for_each : int -> int -> (int -> 'a t) -> 'a t
(** [for_each x y f] constructs an enumerator that is the {i concatenation} of the
    enumerators [f i] where [i] successively takes the values from [x] to [y]
    (both inclusive). *)

val map1 : ('a -> 'b) -> ('b -> string) -> 'a t -> 'b t
(** [map1 f p e] constructs an enumerator applying the function [f] to the
    values enumerated by [e]. [p] is used to convert the values enumerated
    by the returned enumerator into strings. *)

val map2 : ('a -> 'b -> 'c) -> ('c -> string) -> 'a t * 'b t -> 'c t
(** [map2 f p (e1, e2)] constructs an enumerator applying the function [f]
    to the values enumerated by [e1] and [e2]. [p] is used to convert the
    values enumerated by the returned enumerator into strings. *)

val map3 : ('a -> 'b -> 'c -> 'd) -> ('d -> string) -> 'a t * 'b t * 'c t -> 'd t
(** [map3 f p (e1, e2, e3)] constructs an enumerator applying the function [f]
    to the values enumerated by [e1], [e2], and [e3]. [p] is used to convert the
    values enumerated by the returned enumerator into strings. *)

val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> ('e -> string) -> 'a t * 'b t * 'c t * 'd t -> 'e t
(** [map4 f p (e1, e2, e3, e4)] constructs an enumerator applying the function [f]
    to the values enumerated by [e1], [e2], [e3], and [e4]. [p] is used to convert the
    values enumerated by the returned enumerator into strings. *)

val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('f -> string) -> 'a t * 'b t * 'c t * 'd t * 'e t -> 'f t
(** [map5 f p (e1, e2, e3, e4, e5)] constructs an enumerator applying the function [f]
    to the values enumerated by [e1], [e2], [e3], [e4], and [e5]. [p] is used to convert the
    values enumerated by the returned enumerator into strings. *)

val zip1 : 'a t -> 'a t
(** The identity function. *)

val zip2 : 'a t -> 'b t -> ('a * 'b) t
(** [zip2 e1 e2] zips [e1] and [e2] into an enumerator producing couple values. *)

val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [zip3 e1 e2 e3] zips [e1], [e2] and [e3] into an enumerator producing triple values. *)

val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [zip4 e1 e2 e3 e4] zips [e1], [e2], [e3] and [e4] into an enumerator producing quadruple values. *)

val zip5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
(** [zip5 e1 e2 e3 e4 e5] zips [e1], [e2], [e3], [e4] and [e5] into an enumerator producing quintuple values. *)

(**/**)

val create_int_functions : ?inf_eq:('a -> 'a -> bool) -> ('a -> 'a) -> ('a -> string) -> 'a -> 'a -> 'a t
(** [create_int_functions s p x y] constructs an enumerator for {i int-like}
    values. [s] is the successor function, [p] is the function used to convert
    values into strings, while [x] and [y] are respectively the lower and upper
    bounds (both inclusive). The [inf_eq] parameter (defaulting to [(<=)]) is
    used to test whether a value is inferior or equal to another one. *)

module State : sig
  type 'a state = {
      init : 'a t array; (** Enumerators. *)
      curr : 'a lazy_list array; (** Current lists of enumerators. *)
    }
  (** The type of state for state-based enumerators. *)

  val get : 'a state -> int -> 'a
  (** [get s i] returns the value of the [i]th enumerator of the state [s]. *)
end
(** A State is an array of enumerators, keeping their current values as
    well as way to {i restart} them. *)

val create_state_based : (unit -> 'a t array) -> ('a State.state -> 'b) -> 'c -> (unit -> 'b lazy_list) * 'c
(** [create_state_based init encode print] constructs a state-based enumerator:
    - [init] is the function returning the value of the [init] field for the state;
    - [encode] is the function used to convert state into value;
    - [print] is the function used to convert values into strings. *)
