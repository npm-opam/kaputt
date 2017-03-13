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

(** This module provides abbreviations of modules and functions for easier use. *)


(** {6 Shorthands for modules} *)

(** Shorthand for [Assertion] module. *)
module Assert : sig
  type failure = Assertion.failure = {
      expected_value : string;
      actual_value : string;
      message : string;
    }
  include module type of Assertion
    with type failure := Assertion.failure
end

(** Shorthand for [Generator] module. *)
module Gen : sig
  type random = Generator.random
  type 'a t = 'a Generator.t
  type 'a outcome = 'a Generator.outcome =
    | Value of 'a
    | Exception of exn
  include module type of Generator
    with type random := Generator.random
     and type 'a t := 'a Generator.t
     and type 'a outcome := 'a Generator.outcome
end

(** Shorthand for [Reducer] module. *)
module Red : sig
  include module type of Reducer
    with type 'a t := 'a Reducer.t
end

(** Shorthand for [Enumerator] module. *)
module Enum : sig
  type 'a lazy_list = 'a Enumerator.lazy_list = Nil | Cons of 'a * ('a lazy_list lazy_t)
  type 'a t = 'a Enumerator.t
  include module type of Enumerator
    with type 'a lazy_list := 'a Enumerator.lazy_list
     and type 'a t := 'a Enumerator.t
end

(** Shorthand for [Specification] module. *)
module Spec : sig
  type 'a predicate = 'a Specification.predicate
  type ('a, 'b) t = ('a, 'b) Specification.t = {
      precond : 'a predicate;
      postcond : ('a * 'b) predicate
    }
  type 'a outcome =
    | Result of 'a
    | Exception of exn
  include module type of Specification
    with type 'a predicate := 'a Specification.predicate
     and type ('a, 'b) t := ('a, 'b) Specification.t
     and type 'a outcome := 'a Specification.outcome
end

(** Bare alias for [Shell] module. *)
module Shell : sig
  type ('a, 'b, 'c) command = ('a, 'b, 'c) Shell.command
  type configuration = Shell.configuration = {
      pipe : string;
      redirect_output : string;
      redirect_append_output : string;
      redirect_error : string;
      redirect_append_error : string;
    }
  include module type of Shell
    with type ('a, 'b, 'c) command := ('a, 'b, 'c) Shell.command
     and type configuration := Shell.configuration
end

(** Bare alias for [Mock] module. *)
module Mock : sig
  type ('a, 'b) t = ('a, 'b) Mock.t
  include module type of Mock
      with type ('a, 'b) t := ('a, 'b) Mock.t
end


(** Bare alias for [Test] module. *)
module Test : sig
  type result = Test.result =
    | Passed
    | Failed of Assertion.failure
    | Uncaught of exn * string
    | Report of int * int * int * (string list) * ((string * int) list)
    | Exit_code of int
  type 'a classifier = 'a Test.classifier
  type t = Test.t
  type output_mode = Test.output_mode =
    | Text_output of out_channel
    | Html_output of out_channel
    | Xml_output of out_channel
    | Xml_junit_output of out_channel
    | Csv_output of out_channel * string
  include module type of Test
    with type result := Test.result
     and type 'a classifier := 'a Test.classifier
     and type t := Test.t
     and type output_mode := Test.output_mode
end


(** {6 Shorthands for functions} *)

val (=>) : 'a Specification.predicate -> ('a * 'b) Specification.predicate -> ('a, 'b) Specification.t
(** Shorthand for [Specification.implies] function. *)

val (==>) : 'a Specification.predicate -> 'b Specification.predicate -> ('a, 'b) Specification.t
(** Shorthand for [Specification.implies'] function. *)

val (&&&) : 'a Specification.predicate -> 'a Specification.predicate -> 'a Specification.predicate
(** Shorthand for [Specification.logand] function. *)

val (|||) : 'a Specification.predicate -> 'a Specification.predicate -> 'a Specification.predicate
(** Shorthand for [Specification.logor] function. *)

val (^^^) : 'a Specification.predicate -> 'a Specification.predicate -> 'a Specification.predicate
(** Shorthand for [Specification.logxor] function. *)

val check : ?title:string -> ?nb_runs:int -> ?nb_tries:int -> ?classifier:'a Test.classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b) Specification.t) list -> unit
(** Shorthand for [Test.check] function. *)

val check_partial : ?title:string -> ?nb_runs:int -> ?nb_tries:int -> ?classifier:'a Test.classifier -> ?random_src:Generator.random -> 'a Generator.t -> ('a -> 'b) -> (('a, 'b Specification.outcome) Specification.t) list -> unit
(** Shorthand for [Test.check_partial] function. *)

val (|>) : ('a, [`Output], 'c1) Shell.command -> ([`Input], 'b , 'c2) Shell.command -> ('a, 'b, 'c2) Shell.command
(** Shorthand for [Shell.pipe] function. *)

val (>>) : ('a, [`Output], 'c) Shell.command -> string -> ('a, [`No_output], 'c) Shell.command
(** Shorthand for [Shell.redirect_output] function. *)

val (>>>) : ('a, [`Output], 'c) Shell.command -> string -> ('a, [`No_output], 'c) Shell.command
(** Shorthand for [Shell.redirect_append] function. *)

val (>>>>) : ('a, 'b, [`Error]) Shell.command -> string -> ('a, 'b, [`No_error]) Shell.command
(** Shorthand for [Shell.redirect_error] function. *)

val (>>>>>) : ('a, 'b, [`Error]) Shell.command -> string -> ('a, 'b, [`No_error]) Shell.command
(** Shorthand for [Shell.redirect_append_error] function. *)
