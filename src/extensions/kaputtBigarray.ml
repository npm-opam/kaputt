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


(* Bigarray utilities *)

exception End_of_array

(* returns a couple where the first component is the coordinates of the first
   element of the array, while the second component is a function that will
   update the coordinates to point to the next element of the array (raising
   End_of_array if the coordinates already designate the last element of the
   array). *)
let iterator ba =
  let is_c_layout = Bigarray.Genarray.layout ba = (Obj.magic Bigarray.c_layout) in
  let dims = Bigarray.Genarray.dims ba in
  let nb_dims = Array.length dims in
  let coords =
    if is_c_layout then
      Array.make nb_dims 0
    else
      Array.make nb_dims 1 in
  let next () =
    if is_c_layout then begin
      let res = ref 0 in
      let i = ref (pred nb_dims) in
      while (!i >= 0) && (coords.(!i) = (pred dims.(!i))) do
        coords.(!i) <- 0;
        incr res;
        decr i
      done;
      if !i >= 0 then begin
        coords.(!i) <- succ coords.(!i);
        !res
      end else
        raise End_of_array
    end else begin
      let res = ref 0 in
      let i = ref 0 in
      while (!i < nb_dims) && (coords.(!i) = dims.(!i)) do
        coords.(!i) <- 1;
        incr res;
        incr i
      done;
      if !i < nb_dims then begin
        coords.(!i) <- succ coords.(!i);
        !res
      end else
        raise End_of_array
    end in
  coords, next

(* [bigarray_iter f a] applies [f] in turn to all elements of [a]. *)
(*let bigarray_iter f ba =
  let coords, next = iterator ba in
  try
    while true do
      f (Bigarray.Genarray.get ba coords);
      ignore (next ())
    done
  with End_of_array -> ()*)

(* Same as [bigarray_iter], except that the function also receives the coordinates of the
   elements as the first argument. *)
let bigarray_iteri f ba =
  let coords, next = iterator ba in
  try
    while true do
      f (Array.copy coords) (Bigarray.Genarray.get ba coords);
      ignore (next ())
    done
  with End_of_array -> ()

(* [bigarray_iter2 f a b] applies [f] in turn to all elements of [a] and [b]. *)
let bigarray_iter2 f ba1 ba2 =
  let coords1, next1 = iterator ba1 in
  let coords2, next2 = iterator ba2 in
  try
    while true do
      f (Bigarray.Genarray.get ba1 coords1) (Bigarray.Genarray.get ba2 coords2);
      ignore (next1 ());
      ignore (next2 ())
    done
  with End_of_array -> ()

(* Same as [bigarray_iter2], except that the function also receives the coordinates of the
   elements as the first argument. *)
let bigarray_iteri2 f ba1 ba2 =
  let coords1, next1 = iterator ba1 in
  let coords2, next2 = iterator ba2 in
  try
    while true do
      f (Array.copy coords1) (Bigarray.Genarray.get ba1 coords1) (Bigarray.Genarray.get ba2 coords2);
      ignore (next1 ());
      ignore (next2 ())
    done
  with End_of_array -> ()

(* [string_of_bigarray f a] converts [a] into a string, using [f] to convert each element. *)
let string_of_bigarray f ba =
  let buf = Buffer.create 16 in
  let nb_dims = Bigarray.Genarray.num_dims ba in
  let last = ref nb_dims in
  let print s =
    for _i = 1 to !last do
      Buffer.add_string buf s
    done in
  let coords, next = iterator ba in
  try
    while true do
      print "[| ";
      let x = Bigarray.Genarray.get ba coords in
      Buffer.add_string buf (try f x with _ -> "?");
      Buffer.add_string buf "; ";
      last := next ();
      print "|]; "
    done;
    assert false
  with End_of_array ->
    last := nb_dims;
    print "|]; ";
    let len = Buffer.length buf in
    Buffer.sub buf 0 (len - 2)


(* Generator *)

module Generator = struct

  let bigarray k l (gen_dims, _) (gen_e, prn_e) =
    (fun r ->
      let dims = gen_dims r in
      let res = Bigarray.Genarray.create k l dims in
      bigarray_iteri
        (fun c _ ->
          let e = gen_e r in
          Bigarray.Genarray.set res c e)
        res;
      res),
    (string_of_bigarray prn_e)

end


(* Enumerator *)

module Enumerator = struct

  let bigarray k l dims elem =
    let sz =
      Array.fold_left
        (fun acc elem ->
          if elem < 0  then
            invalid_arg "KaputtBigarray.Enumerator.bigarray"
          else
            acc * elem)
        1
        dims in
    Kaputt.Enumerator.create_state_based
      (fun () -> Array.init sz (fun _ -> elem))
      (fun s ->
        let res = Bigarray.Genarray.create k l dims in
        let i = ref 0 in
        bigarray_iteri
          (fun c _ ->
            Bigarray.Genarray.set res c (Kaputt.Enumerator.State.get s !i);
            incr i)
          res;
        res)
      (string_of_bigarray (snd elem))

end


(* Assertion *)

module Assertion = struct

  let make_equal_bigarray eq prn ?(msg="") x y =
    let dx = Bigarray.Genarray.num_dims x in
    let dy = Bigarray.Genarray.num_dims y in
    if dx = dy then
      let dsx = Bigarray.Genarray.dims x in
      let dsy = Bigarray.Genarray.dims y in
      for i = 0 to pred dx do
        if dsx.(i) <> dsy.(i) then
          let fm =
            Printf.sprintf "%s (bigarrays have different sizes for dimension %d - %d and %d)"
              msg
              i
              dx
              dy in
          Kaputt.Assertion.fail_msg fm
      done;
      bigarray_iteri2
        (fun i ex ey ->
          if not (eq ex ey) then
            let index = Kaputt.Utils.make_string_of_array string_of_int in
            let fm = Printf.sprintf "%s (at index %s)" msg (index i) in
            Kaputt.Assertion.fail (prn ex) (prn ey) fm)
        x
        y
    else
      let fm =
        Printf.sprintf "%s (bigarrays of different dimensions - %d and %d)"
          msg
          dx
          dy in
      Kaputt.Assertion.fail_msg fm

  let make_not_equal_bigarray eq _ ?(msg="") x y =
    let dx = Bigarray.Genarray.num_dims x in
    let dy = Bigarray.Genarray.num_dims y in
    if dx = dy then begin
      let dsx = Bigarray.Genarray.dims x in
      let dsy = Bigarray.Genarray.dims y in
      let i = ref 0 in
      while (!i < dx) && (dsx.(!i) = dsy.(!i)) do
        incr i
      done;
      if (!i = dx) then begin
        try
          bigarray_iter2
            (fun ex ey ->
              if not (eq ex ey) then raise End_of_array)
            x
            y;
          Kaputt.Assertion.fail_msg msg
        with End_of_array -> ();
      end
    end

end


(* Specification *)

module Specification = struct

  let is_empty_bigarray x =
    let dims = Bigarray.Genarray.dims x in
    ((Array.length dims) = 0) || ((Array.fold_left ( * ) 1 dims) = 0)

  let is_nonempty_bigarray x =
    let dims = Bigarray.Genarray.dims x in
    ((Array.length dims) > 0) && ((Array.fold_left ( * ) 1 dims) > 0)

  let exists_bigarray p x =
    let coords, next = iterator x in
    try
      while true do
        if p (Bigarray.Genarray.get x coords) then
          raise End_of_array;
        ignore (next ())
      done;
      false
    with End_of_array -> true

  let for_all_bigarray p x =
    let coords, next = iterator x in
    try
      while true do
        if not (p (Bigarray.Genarray.get x coords)) then
          raise End_of_array;
        ignore (next ())
      done;
      true
    with End_of_array -> false

end


(* Reducer *)

module Reducer = struct

  let bigarray_c x =
    if not (Specification.is_empty_bigarray x) then
      let dims = Bigarray.Genarray.dims x in
      [ Bigarray.Genarray.create
          (Bigarray.Genarray.kind x)
          Bigarray.c_layout
          [| 0 |];
        Bigarray.Genarray.sub_left
          x
          0
          (dims.(0) / 2) ]
    else
      []

  let bigarray_fortran x =
    if not (Specification.is_empty_bigarray x) then
      let dims = Bigarray.Genarray.dims x in
      [ Bigarray.Genarray.create
          (Bigarray.Genarray.kind x)
          Bigarray.fortran_layout
          [| 0 |];
        Bigarray.Genarray.sub_right
          x
          0
          (dims.(pred (Array.length dims)) / 2) ]
    else
      []

end
