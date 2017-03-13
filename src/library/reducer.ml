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


(* Reducer definition *)

type 'a t = 'a -> 'a list


(* Predefined reducers *)

let unit () =
  []

let bool _ =
  []

let int x =
  [ -2; -1; 0; 1; 2; x / 2 ]

let two32 = Int32.of_int 2

let int32 x =
  [ -2l; -1l; 0l; 1l; 2l; Int32.div x two32 ]

let two64 = Int64.of_int 2

let int64 x =
  [ -2L; -1L; 0L; 1L; 2L; Int64.div x two64 ]

let twonat = Nativeint.of_int 2

let nativeint x =
  [ -2n; -1n; 0n; 1n; 2n; Nativeint.div x twonat ]

let char x =
  [ ' '; '\t'; '\n'; Char.chr ((Char.code x) / 2) ]

let string x =
  if x <> "" then
    let len = String.length x in
    let len2 = (len + 1) / 2 in
    let s1 = String.sub x 0 len2 in
    let s2 = String.sub x len2 (len - len2) in
    [ ""; s1; s2 ]
  else
    []

let float x =
  [ neg_infinity; -0.; 0.; infinity; x /. 2. ]

let complex x =
  List.map2
    (fun re im ->
      { Complex.re = re;
        Complex.im = im; })
    (float x.Complex.re)
    (float x.Complex.im)


(* Reducers for containers *)

let array x =
  if x <> [||] then
    let len = Array.length x in
    let len2 = (len + 1) / 2 in
    let s1 = Array.sub x 0 len2 in
    let s2 = Array.sub x len2 (len - len2) in
    [ [||]; s1; s2 ]
  else
    []

let list x =
  match x with
  | head :: tail ->
      let rec split acc n = function
        | (hd :: tl) as l ->
            if n = 0 then
              [ []; [head]; List.rev acc; tail; l ]
            else
              split (hd :: acc) (pred n) tl
        | [] -> [ []; [head]; List.rev acc; tail ] in
      split [] ((List.length x + 1) / 2) x
  | [] ->
      []

let option x =
  match x with
  | Some _ -> [ None ]
  | None -> []

module Map (M : Map.S) = struct
  let red x =
    if not (M.is_empty x) then
      let m1, _, m2 = M.split (fst (M.choose x)) x in
      [ M.empty; m1; m2 ]
    else
      []
end

module Set (S : Set.S) = struct
  let red x =
    if not (S.is_empty x) then
      let s1, _, s2 = S.split (S.choose x) x in
      [ S.empty; s1; s2 ]
    else
      []
end

let hashtbl x =
  let len = Hashtbl.length x in
  let create () = Hashtbl.create 17 in
  if len <> 0 then
    let h1 = create () in
    let h2 = create () in
    let _ =
      Hashtbl.fold
        (fun x y acc ->
          if acc <= 0 then
            Hashtbl.add h1 x y
          else
            Hashtbl.add h2 x y;
          pred acc)
        x
        (len / 2) in
    [ create (); h1; h2 ]
  else
    []

let queue x =
  let len = Queue.length x in
  if len <> 0 then
    let res = Queue.copy x in
    for _i = 1 to len / 2 do
      ignore (Queue.pop res)
    done;
    [ Queue.create (); res ]
  else
    []

let stack x =
  let len = Stack.length x in
  if len <> 0 then
    let res = Stack.copy x in
    for _i = 1 to len / 2 do
      ignore (Stack.pop res)
    done;
    [ Stack.create (); res ]
  else
    []

let weak x =
  let sub x i l =
    let res = Weak.create l in
    Weak.blit x i res 0 l;
    res in
  let len = Weak.length x in
  if len <> 0 then
    let len2 = (len + 1) / 2 in
    let s1 = sub x 0 len2 in
    let s2 = sub x len2 (len - len2) in
    [ (Weak.create 0); s1; s2 ]
  else
    []
