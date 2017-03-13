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


(* Exceptions and types *)

exception Unexpected_value of string

exception End_of_sequence

type ('a, 'b) t = {
    implementation : 'a -> 'b;
    get_count : 'a -> int;
    get_total : unit -> int;
    get_calls : unit -> 'a list;
  }


(* Mock builders *)

let default_printer _ = ""

let from_mapping (type s) ?(cmp=Pervasives.compare) ?(prn=default_printer) l =
  let module M = Map.Make (struct type t = s let compare = cmp end) in
  let map =
    List.fold_right
      (fun (x, y) acc ->
        M.add x (y, ref 0) acc)
      l
      M.empty in
  let total = ref 0 in
  let calls = ref [] in
  let implementation =
    fun x ->
      try
        let res, count = M.find x map in
        incr count;
        incr total;
        calls := x :: !calls;
        res
      with Not_found ->
        raise (Unexpected_value (prn x)) in
  let get_count =
    fun x ->
      !(snd (M.find x map)) in
  let get_total () = !total in
  let get_calls () = List.rev !calls in
  { implementation ; get_count ; get_total ; get_calls }

let from_sequence (type s) ?(cmp=Pervasives.compare) ?(prn=default_printer) l =
  let module M = Map.Make (struct type t = s let compare = cmp end) in
  let map = ref M.empty in
  let remaining = ref l in
  let total = ref 0 in
  let calls = ref [] in
  let implementation =
    fun x ->
      match !remaining with
      | (x', y) :: tl ->
          if (cmp x x') = 0 then begin
            remaining := tl;
            let old = try M.find x !map with Not_found -> 0 in
            map := M.add x (succ old) !map;
            incr total;
            calls := x :: !calls;
            y
          end else
            raise (Unexpected_value (prn x))
      | [] -> raise End_of_sequence in
  let get_count =
    fun x ->
      M.find x !map in
  let get_total () = !total in
  let get_calls () = List.rev !calls in
  { implementation ; get_count ; get_total ; get_calls }

let from_function (type s) ?(cmp=Pervasives.compare) f =
  let module M = Map.Make (struct type t = s let compare = cmp end) in
  let map = ref M.empty in
  let total = ref 0 in
  let calls = ref [] in
  let implementation =
    fun x ->
      let y = f x in
      let old = try M.find x !map with Not_found -> 0 in
      map := M.add x (succ old) !map;
      incr total;
      calls := x :: !calls;
      y in
  let get_count =
    fun x ->
      M.find x !map in
  let get_total () = !total in
  let get_calls () = List.rev !calls in
  { implementation ; get_count ; get_total ; get_calls }


(* Mock functions *)

let func m =
  m.implementation

let func2 m =
  fun x y -> m.implementation (x, y)

let func3 m =
  fun x y z -> m.implementation (x, y, z)

let func4 m =
  fun x y z t -> m.implementation (x, y, z, t)

let func5 m =
  fun x y z t u -> m.implementation (x, y, z, t, u)

let count m x =
  m.get_count x

let total m =
  m.get_total ()

let calls m =
  m.get_calls ()
