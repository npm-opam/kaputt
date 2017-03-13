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


(* Conversion utilities *)

let buffer_size = 256

let string_of_unit () =
  String.copy "()"

let string_of_char c =
  Printf.sprintf "%C" c

let string_of_string s =
  Printf.sprintf "%S" s

let string_of_complex x =
  if x.Complex.im > 0. then
    Printf.sprintf "%f+%fi" x.Complex.re x.Complex.im
  else
    Printf.sprintf "%f%fi" x.Complex.re x.Complex.im

let string_of_buffer x =
  string_of_string (Buffer.contents x)

let make_string_of_array f a =
  let buf = Buffer.create buffer_size in
  Buffer.add_string buf "[| ";
  Array.iter
    (fun x ->
      Buffer.add_string buf (f x);
      Buffer.add_string buf "; ")
    a;
  Buffer.add_string buf "|]";
  Buffer.contents buf

let make_string_of_list f l =
  let buf = Buffer.create buffer_size in
  Buffer.add_string buf "[ ";
  List.iter
    (fun x ->
      Buffer.add_string buf (f x);
      Buffer.add_string buf "; ")
    l;
  Buffer.add_string buf "]";
  Buffer.contents buf

let make_string_of_option f = function
  | None -> "None"
  | Some v -> "Some (" ^ (f v) ^ ")"

let make_string_of_ref f x =
  "ref (" ^ (f !x) ^ ")"

let make_string_of_hashtbl f g h =
  let buf = Buffer.create buffer_size in
  Hashtbl.iter
    (fun k v ->
      Buffer.add_string buf (f k);
      Buffer.add_string buf " -> ";
      Buffer.add_string buf (g v);
      Buffer.add_string buf "; ")
    h;
  Buffer.contents buf

let make_string_of_queue f q =
  let buf = Buffer.create buffer_size in
  Queue.iter
    (fun e ->
      Buffer.add_string buf (f e);
      Buffer.add_string buf "; ")
    q;
  Buffer.contents buf

let make_string_of_stack f s =
  let buf = Buffer.create buffer_size in
  Stack.iter
    (fun e ->
      Buffer.add_string buf (f e);
      Buffer.add_string buf "; ")
    s;
  Buffer.contents buf

let make_string_of_weak f w =
  let buf = Buffer.create buffer_size in
  let len = Weak.length w in
  Buffer.add_string buf "[|| ";
  for i = 0 to (pred len) do
    Buffer.add_string buf (f (Weak.get w i));
    Buffer.add_string buf "; ";
  done;
  Buffer.add_string buf "||]";
  Buffer.contents buf

external make_string_of_tuple1 : 'a -> 'a = "%identity"

let make_string_of_tuple2 f1 f2 (x1, x2) =
  let y1 = f1 x1 in
  let y2 = f2 x2 in
  Printf.sprintf "(%s, %s)" y1 y2

let make_string_of_tuple3 f1 f2 f3 (x1, x2, x3) =
  let y1 = f1 x1 in
  let y2 = f2 x2 in
  let y3 = f3 x3 in
  Printf.sprintf "(%s, %s, %s)" y1 y2 y3

let make_string_of_tuple4 f1 f2 f3 f4 (x1, x2, x3, x4) =
  let y1 = f1 x1 in
  let y2 = f2 x2 in
  let y3 = f3 x3 in
  let y4 = f4 x4 in
  Printf.sprintf "(%s, %s, %s, %s)" y1 y2 y3 y4

let make_string_of_tuple5 f1 f2 f3 f4 f5 (x1, x2, x3, x4, x5) =
  let y1 = f1 x1 in
  let y2 = f2 x2 in
  let y3 = f3 x3 in
  let y4 = f4 x4 in
  let y5 = f5 x5 in
  Printf.sprintf "(%s, %s, %s, %s, %s)" y1 y2 y3 y4 y5
