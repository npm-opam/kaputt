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
    allowing to encode shell scripts. *)


(** {6 Type definitions} *)

type ('a, 'b, 'c) command
  constraint 'a = [< `Input | `No_input ]
  constraint 'b = [< `Output | `No_output]
  constraint 'c = [< `Error | `No_error]
(** The type of commands, the type parameters being phantom types indicating:
    - whether the command reads data from standard input;
    - whether the command writes data to standard output;
    - whether the command writes data to standard error. *)

type configuration = {
    pipe : string; (** The notation used for shell pipes. *)
    redirect_output : string; (** The notation used to redirect ouput. *)
    redirect_append_output : string; (** The notation used to redirect ouput in append mode. *)
    redirect_error : string; (** The notation used to redirect error. *)
    redirect_append_error : string; (** The notation used to redirect error in append mode. *)
  }
(** The type of shell configuration, defining some syntactic elements. *)


(** {6 Configuration} *)

val bash : configuration
(** The configuration for bash shells. *)

val set_configuration : configuration -> unit
(** [set_configuration c] changes the configuration to [c].

    The default configuration is [bash]. *)


(** {6 Utility functions} *)

val read_lines : string -> string list
(** [read_lines file] returns the lines from file [file].
    Raises an exception if an i/o error occurs. *)

val write_lines : string list -> string -> unit
(** [write_lines l file] writes the strings from [l] to file [file],
    a ['\n'] character being written after each string.
    Raises an exception if an i/o error occurs. *)

val command : string -> ('a, 'b, 'c) command
(** [command s] lifts [s] to a command. The user should carefully set the
    parameter types of the command type. Should be used only to provide
    base commands not defined in this module. *)

val coerce : ('a, 'b, 'c) command -> ('d, 'e, 'f) command
(** [coerce c] allows one to change the parameter types of the command type. *)

val ignore : ('a, 'b, 'c) command -> ('a, [`No_output], [`No_error]) command
(** A specialized version of [coerce] that ignores both outputs. *)

val run : ('a, 'b, 'c) command -> int
(** [run c] runs the command [c], returning its exit code. *)

val run_list : ('a, 'b, 'c) command list -> int
(* [run_list l] runs the commands from [l] as long as they return [0].
   Returns the exit code of the last command, or the exit code of the
   first command from [l] that does not return [0]. *)


(** {6 Imported functions} *)

val file_exists : string -> bool
(** Synonym for [Sys.file_exists]. *)

val is_directory : string -> bool
(** Synonym for [Sys.is_directory]. *)

val getenv : string -> string
(** Synonym for [Sys.getenv]. *)

val files : string -> string list
(** Equivalent to [Sys.readdir] except that a list is returned. *)

val files_with_filter : (string -> bool) -> string -> string list
(** Equivalent to [files] except that the returned list is filtered by
    the passed function. *)

val files_with_suffix : string -> string -> string list
(** [files_with_suffix s d] equivalent to [files d] except that only
    the files with the suffix [s] are returned. *)

val current_dir_name : string
(** Equivalent to [Filename.current_dir_name]. *)

val parent_dir_name : string
(** Equivalent to [Filename.parent_dir_name]. *)

val is_relative : string -> bool
(** Equivalent to [Filename.is_relative]. *)

val is_implicit : string -> bool
(** Equivalent to [Filename.is_implicit]. *)

val check_suffix : string -> string -> bool
(** Equivalent to [Filename.check_suffix]. *)

val chop_suffix : string -> string -> string
(** Equivalent to [Filename.chop_suffix]. *)

val chop_extension : string -> string
(** Equivalent to [Filename.chop_extension]. *)

val basename : string -> string
(** Equivalent to [Filename.basename]. *)

val dirname : string -> string
(** Equivalent to [Filename.dirname]. *)

val concatname : string -> string -> string
(** Equivalent to [Filename.concatname]. *)

val temp_file : ?temp_dir:string -> string -> string -> string
(** Equivalent to [Filename.temp_file]. *)

val quote : string -> string
(** Equivalent to [Filename.quote]. *)


(** {6 Directory functions} *)

val pwd : unit -> string
(** Returns the current directory. *)

val cd : string -> unit
(** Changes the current directory to the value of the passed argument.
    Raises [Sys_error] if the directory does not exist. *)

val pushd : string -> unit
(** Pushes the current directory to the directory stack, then changes the
    current directory to the value of the passed argument.
    Raises [Sys_error] if the directory does not exist. *)

val popd : unit -> string
(** Changes the current directory to the value poped from the directory stack,
    and returns it.
    Raise [Stack.Empty] if the directory stack is empty.
    Raises [Sys_error] if the directory does not exist. *)


(** {6 Basic commands} *)

val exit : int -> ([`No_input], [`No_output], [`No_error]) command
(** [exit n] builds a command returning the exit code [n]. *)


(** {6 Directory commands} *)

val pwdir : unit -> ([`No_input], [`Output], [`No_error]) command
(** Equivalent to [pwd], as a command. *)

val chdir : string -> ([`No_input], [`No_output], [`Error]) command
(** Equivalent to [cd], as a command. *)

val mkdir : ?options:string list -> string -> ([`No_input], [`No_output], [`Error]) command
(** [mkdir ~options:options path] builds a command creating the directory [path],
    passing [options] to the mkdir executable. [options] defaults to [[]]. *)

val rmdir : ?options:string list -> string -> ([`No_input], [`No_output], [`Error]) command
(** [rmdir ~options:options path] builds a command deleting the directory [path],
    passing [options] to the rmdir executable. [options] defaults to [[]]. *)


(** {6 File commands} *)

val ls : ?options:string list -> string list -> ([`No_input], [`Output], [`Error]) command
(** [ls ~options:options l] builds a command listing elements from [l],
    passing [options] to the ls executable. [options] defaults to [[]]. *)

val cp : ?options:string list -> string list -> string -> ([`No_input], [`No_output], [`Error]) command
(** [cp ~options:options l d] builds a command copying elements from [l] to [d],
    passing [options] to the cp executable. [options] defaults to [[]]. *)

val rm : ?options:string list -> string list -> ([`No_input], [`No_output], [`Error]) command
(** [rm ~options:options l] builds a command deleting elements from [l],
    passing [options] to the rm executable. [options] defaults to [[]]. *)

val mv : ?options:string list -> string list -> string -> ([`No_input], [`No_output], [`Error]) command
(** [mv ~options:options l d] builds a command moving elements from [l] to [d],
    passing [options] to the mv executable. [options] defaults to [[]]. *)

val touch : ?options:string list -> string list -> ([`No_input], [`No_output], [`Error]) command
(** [touch ~options:options l] builds a command touching elements from [l],
    passing [options] to the touch executable. [options] defaults to [[]]. *)

val cat : ?options:string list -> string list -> ([`No_input], [`Output], [`Error]) command
(** [cat ~options:options l] builds a command displaying elements from [l],
    passing [options] to the cat executable. [options] defaults to [[]]. *)

val echo : ?options:string list -> string -> ([`No_input], [`Output], [`Error]) command
(** [echo ~options:options x] builds a command printing [x],
    passing [options] to the echo executable. [options] defaults to [[]]. *)

val diff : ?options:string list -> string -> string -> ([`No_input], [`Output], [`Error]) command
(** [diff ~options:options x y] builds a command computing the difference between [x] and [y],
    passing [options] to the diff executable. [options] defaults to [[]]. *)

val grep : ?options:string list -> string -> ([`Input], [`Output], [`Error]) command
(** [grep ~options:options x] builds a command filtering the input with the expression [x],
    passing [options] to the grep executable. [options] defaults to [[]]. *)

val grep_files : ?options:string list -> string -> string list -> ([`No_input], [`Output], [`Error]) command
(** [grep_files ~options:options x l] builds a command filtering the files from [l]
    with the expression [x], passing [options] to the grep executable.
    [options] defaults to [[]]. *)

val sed : ?options:string list -> string -> ([`Input], [`Output], [`Error]) command
(** [sed ~options:options x] builds a command applying the expression [x] to input,
    passing [options] to the sed executable. [options] defaults to [[]]. *)

val sort : ?options:string list -> string list -> ([`Input], [`Output], [`Error]) command
(** [sed ~options:options l] builds a command sorting the data it receives,
    passing [options] to the sort executable. [options] defaults to [[]]. *)

val cut : ?options:string list -> string list -> ([`Input], [`Output], [`Error]) command
(** [cut ~options:options l] builds a command extract elements from the data it receives,
    passing [options] to the cut executable. [options] defaults to [[]]. *)


(** {6 Miscellaneous commands} *)

val sleep : int -> ([`No_input], [`No_output], [`No_error]) command
(** [sleep x] suspends the execution for [x] seconds. *)


(** {6 Combinators over commands} *)

val pipe : ('a, [`Output], 'c1) command -> ([`Input], 'b , 'c2) command -> ('a, 'b, 'c2) command
(** [pipe c1 c2] builds a command that is equivalent to [c1] piped to [c2]. *)

val (|>) : ('a, [`Output], 'c1) command -> ([`Input], 'b , 'c2) command -> ('a, 'b, 'c2) command
(** Bare synonym for [pipe]. *)

val redirect_output : ('a, [`Output], 'c) command -> string -> ('a, [`No_output], 'c) command
(** [redirect_output c f] builds a command that redirect the output of command [c] to file [f]. *)

val (>>) : ('a, [`Output], 'c) command -> string -> ('a, [`No_output], 'c) command
(** Bare synonym for [redirect_output]. *)

val redirect_append : ('a, [`Output], 'c) command -> string -> ('a, [`No_output], 'c) command
(** [redirect_append c f] builds a command that redirect the output of command [c] to file [f] in append mode. *)

val (>>>) : ('a, [`Output], 'c) command -> string -> ('a, [`No_output], 'c) command
(** Bare synonym for [redirect_append]. *)

val redirect_error : ('a, 'b, [`Error]) command -> string -> ('a, 'b, [`No_error]) command
(** [redirect_error c f] builds a command that redirect the error of command [c] to file [f]. *)

val (>>>>) : ('a, 'b, [`Error]) command -> string -> ('a, 'b, [`No_error]) command
(** Bare synonym for [redirect_error]. *)

val redirect_append_error : ('a, 'b, [`Error]) command -> string -> ('a, 'b, [`No_error]) command
(** [redirect_error c f] builds a command that redirect the error of command [c] to file [f] in append mode. *)

val (>>>>>) : ('a, 'b, [`Error]) command -> string -> ('a, 'b, [`No_error]) command
(** Bare synonym for [redirect_append_error]. *)
