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


(* Type definition *)

type ('a, 'b, 'c) command  = string
  constraint 'a = [< `Input | `No_input ]
  constraint 'b = [< `Output | `No_output]
  constraint 'c = [< `Error | `No_error]

type configuration = {
    pipe : string;
    redirect_output : string;
    redirect_append_output : string;
    redirect_error : string;
    redirect_append_error : string;
  }


(* Configuration *)

let bash = {
  pipe = "|";
  redirect_output = ">";
  redirect_append_output = ">>";
  redirect_error = "2>";
  redirect_append_error = "2>>";
}

let configuration = ref bash

let set_configuration c =
  configuration := c


(* Utility functions *)

let read_lines fn =
  let ch = open_in fn in
  let res = ref [] in
  try
    while true do
      res := (input_line ch) :: !res
    done;
    assert false
  with
  | End_of_file ->
      close_in_noerr ch;
      List.rev !res
  | e ->
      close_in_noerr ch;
      raise e

let write_lines l fn =
  let ch = open_out fn in
  try
    List.iter
      (fun s ->
        output_string ch s;
        output_char ch '\n')
      l;
    close_out_noerr ch
  with e ->
    close_out_noerr ch;
    raise e

external command : string -> ('a, 'b, 'c) command = "%identity"

external coerce : ('a, 'b, 'c) command -> ('d, 'e, 'f) command = "%identity"

external ignore : ('a, 'b, 'c) command -> ('a, [`No_output], [`No_error]) command = "%identity"

let run = Sys.command

let run_list l =
  let rec r acc = function
  | hd :: tl ->
      let res = run hd in
      if res <> 0 then
        res
      else
        r res tl
  | [] -> acc in
  r 0 l


(* Imported functions *)

let file_exists = Sys.file_exists

let is_directory = Sys.is_directory

let getenv = Sys.getenv

let files s = Array.to_list (Sys.readdir s)

let files_with_filter f s =
  let res = files s in
  List.filter f res

let files_with_suffix suff s =
  files_with_filter (fun x -> Filename.check_suffix x suff) s

let current_dir_name = Filename.current_dir_name

let parent_dir_name = Filename.parent_dir_name

let is_relative = Filename.is_relative

let is_implicit = Filename.is_implicit

let check_suffix = Filename.check_suffix

let chop_suffix = Filename.chop_suffix

let chop_extension = Filename.chop_extension

let basename = Filename.basename

let dirname = Filename.dirname

let concatname = Filename.concat

let temp_file = Filename.temp_file

let quote = Filename.quote


(* Generic command builder *)

let make_command x l1 l2 =
  let buff = Buffer.create 32 in
  Buffer.add_string buff x;
  let add_strings l =
    List.iter
      (fun x ->
        Buffer.add_char buff ' ';
        Buffer.add_string buff x)
      l in
  add_strings l1;
  add_strings l2;
  Buffer.contents buff


(* Directory functions *)

let directory_stack = Stack.create ()

let pwd = Sys.getcwd

let cd = Sys.chdir

let pushd x =
  Stack.push (pwd ()) directory_stack;
  cd x

let popd () =
  let res = Stack.pop directory_stack in
  cd res;
  res


(* Basic commands *)

let exit n = make_command "exit" [] [string_of_int n]


(* Directory commands *)

let pwdir _ = make_command "pwd" [] []

let chdir x = make_command "cd" [] [x]

let mkdir ?(options=[]) x = make_command "mkdir" options [x]

let rmdir ?(options=[]) x = make_command "rmdir" options [x]


(* File commands *)

let ls ?(options=[]) l = make_command "ls" options l

let cp ?(options=[]) l x = make_command "cp" options (l @ [x])

let rm ?(options=[]) l = make_command "rm" options l

let mv ?(options=[]) l x = make_command "mv" options (l @ [x])

let touch ?(options=[]) l = make_command "touch" options l

let cat ?(options=[]) l = make_command "cat" options l

let echo ?(options=[]) e = make_command "echo" options [e]

let diff ?(options=[]) x y = make_command "diff" options [x; y]

let grep ?(options=[]) e = make_command "grep" options [e]

let grep_files ?(options=[]) e l = make_command "grep" options (e :: l)

let sed ?(options=[]) e = make_command "sed" options [e]

let sort ?(options=[]) l = make_command "sort" options l

let cut ?(options=[]) l = make_command "cut" options l


(* Miscellaneous commands *)

let sleep x = make_command "sleep" [string_of_int x] []


(* Combinators over commands *)

let pipe c1 c2 =
  Printf.sprintf "(%s) %s (%s)" c1 !configuration.pipe c2

let (|>) = pipe

let redirect_output c f =
  Printf.sprintf "((%s) %s %s)" c !configuration.redirect_output (quote f)

let (>>) = redirect_output

let redirect_append c f =
  Printf.sprintf "((%s) %s %s)" c !configuration.redirect_append_output (quote f)

let (>>>) = redirect_append

let redirect_error c f =
  Printf.sprintf "((%s) %s %s)" c !configuration.redirect_error (quote f)

let (>>>>) = redirect_error

let redirect_append_error c f =
  Printf.sprintf "((%s) %s %s)" c !configuration.redirect_append_error (quote f)

let (>>>>>) = redirect_append_error
