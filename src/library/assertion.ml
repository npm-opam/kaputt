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


(* Exception *)

type failure = {
    expected_value : string; (** Expected value converted to string. *)
    actual_value : string; (** Actual value converted to string. *)
    message : string; (** Short message associated with failure. *)
  }

exception Failed of failure

let fail x y z =
  let f = { expected_value = x; actual_value = y; message = z; } in
  raise (Failed f)

let fail_msg = fail "" ""


(* Generic functions *)

let default_printer _ = ""

let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if not (eq x y) then fail (prn x) (prn y) msg

let not_equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if eq x y then fail (prn x) (prn y) msg

let same ?(prn=default_printer) ?(msg="") x y =
  if not (x == y) then fail (prn x) (prn y) msg

let not_same ?(prn=default_printer) ?(msg="") x y =
  if x == y then fail (prn x) (prn y) msg


(* Function builders *)

let make_equal e p = equal ~eq:e ~prn:p

let make_not_equal e p = not_equal ~eq:e ~prn:p

let make_equal_array eq prn ?(msg="") x y =
  let lx = Array.length x in
  let ly = Array.length y in
  if lx = ly then
    for i = 0 to pred lx do
      let xi = x.(i) in
      let yi = y.(i) in
      if not (eq xi yi) then
        let fm = Printf.sprintf "%s (at index %d)" msg i in
        fail (prn xi) (prn yi) fm
    done
  else
    let fm =
      Printf.sprintf "%s (arrays of different sizes - %d and %d)"
        msg
        lx
        ly in
    fail_msg fm

let make_not_equal_array eq _ ?(msg="") x y =
  let lx = Array.length x in
  let ly = Array.length y in
  if lx = ly then begin
    let i = ref 0 in
    while (!i < lx) && (eq x.(!i) y.(!i)) do
      incr i
    done;
    if !i = lx then fail_msg msg
  end

let make_equal_list eq prn ?(msg="") x y =
  let rec iter i x y =
    match x, y with
    | hd_x :: tl_x, hd_y :: tl_y ->
        if eq hd_x hd_y then
          iter (succ i) tl_x tl_y
        else
          let fm = Printf.sprintf "%s (at index %d)" msg i in
          fail (prn hd_x) (prn hd_y) fm
    | _ :: _, [] | [], _ :: _ ->
        let fm = Printf.sprintf "%s (lists of different sizes)" msg in
        fail_msg fm
    | [], [] ->
        () in
  iter 0 x y

let make_not_equal_list eq _ ?(msg="") x y =
  let rec iter x y =
    match x, y with
    | hd_x :: tl_x, hd_y :: tl_y ->
        if eq hd_x hd_y then
          iter tl_x tl_y
        else
          ()
    | _ :: _, [] | [], _ :: _ ->
        ()
    | [], [] ->
        fail_msg msg in
  iter x y

let make_equal_hashtbl eq prn prn' ?(msg="") x y =
  let lx = Hashtbl.length x in
  let ly = Hashtbl.length y in
  if lx = ly then
    Hashtbl.iter
      (fun k v ->
        let l = try Hashtbl.find_all y k with _ -> [] in
        if not (List.exists (fun x -> eq x v) l) then
          let fm =
            Printf.sprintf "%s (%s, %s present only in first table)"
              msg
              (prn k)
              (prn' v) in
          fail_msg fm)
      x
  else
    let fm =
      Printf.sprintf "%s (hashtables of different sizes - %d and %d)"
        msg
        lx
        ly in
    fail_msg fm

let make_not_equal_hashtbl eq _ _ ?(msg="") x y =
  let lx = Hashtbl.length x in
  let ly = Hashtbl.length y in
  if lx = ly then begin
    try
      Hashtbl.iter
        (fun k v ->
          let l = try Hashtbl.find_all y k with _ -> [] in
          if not (List.exists (fun x -> eq x v) l) then raise Not_found)
        x;
      fail_msg msg
    with Not_found -> ()
  end

module type Printer = sig
  type t
  val to_string : t -> string
end

module Map (M : Map.S) (P : Printer with type t = M.key) = struct
  let make_equal eq prn ?(msg="") x y =
    let _ =
      M.merge
        (fun k v v' ->
          match v, v' with
          | Some _, None ->
              let fm =
                Printf.sprintf "%s (key %s present only in first table)"
                  msg
                  (P.to_string k) in
              fail_msg fm
          | None, Some _ ->
              let fm =
                Printf.sprintf "%s (key %s present only in second table)"
                  msg
                  (P.to_string k) in
              fail_msg fm
          | Some xi', Some yi' when not (eq xi' yi') ->
              let fm =
                Printf.sprintf "%s (for key %s)"
                  msg
                  (P.to_string k) in
              fail fm (prn xi') (prn yi')
          | _ -> None)
        x
        y in
    ()

  let make_not_equal eq _ ?(msg="") x y =
    if (M.cardinal y) = (M.cardinal x) then begin
      let res =
        M.merge
          (fun _ v v' ->
            match v, v' with
            | Some xi', Some yi' when eq xi' yi' -> Some xi'
            | _ -> None)
          x
          y in
      if (M.cardinal res) = (M.cardinal x) then
        fail_msg msg
    end
end

module Set (S : Set.S) (P : Printer with type t = S.elt) = struct
  let equal ?(msg="") x y =
    let diff = S.diff x y in
    if not (S.is_empty diff) then
      let fm =
        Printf.sprintf "%s (element %s present only in second table)"
          msg
          (P.to_string (S.choose diff)) in
      fail_msg fm
    else begin
      let diff' = S.diff y x in
      if not (S.is_empty diff') then
        let fm =
          Printf.sprintf "%s (element %s present only in first table)"
            msg
            (P.to_string (S.choose diff)) in
        fail_msg fm
    end
    
  let not_equal ?(msg="") x y =
    let lx = S.cardinal x in
    let ly = S.cardinal y in
    if (lx = ly) && (S.subset x y) then
      fail_msg msg
end

let make_equal_queue eq prn ?(msg="") x y =
  let lx = Queue.length x in
  let ly = Queue.length y in
  if lx = ly then
    let x' = Queue.copy x in
    let i = ref 0 in
    Queue.iter
      (fun yi ->
        let xi = Queue.pop x' in
        if not (eq xi yi) then
          let fm = Printf.sprintf "%s (at index %d)" msg !i in
          fail (prn xi) (prn yi) fm
        else
          incr i)
      y
  else
    let fm =
      Printf.sprintf "%s (queues of different sizes - %d and %d)"
        msg
        lx
        ly in
    fail_msg fm

let make_not_equal_queue eq _ ?(msg="") x y =
  let lx = Queue.length x in
  let ly = Queue.length y in
  if lx = ly then begin
    let x' = Queue.copy x in
    try
      Queue.iter
        (fun yi ->
          let xi = Queue.pop x' in
          if not (eq xi yi) then raise Not_found)
        y;
      fail_msg msg
    with Not_found -> ()
  end

let make_equal_stack eq prn ?(msg="") x y =
  let lx = Stack.length x in
  let ly = Stack.length y in
  if lx = ly then
    let x' = Stack.copy x in
    let i = ref 0 in
    Stack.iter
      (fun yi ->
        let xi = Stack.pop x' in
        if not (eq xi yi) then
          let fm = Printf.sprintf "%s (at index %d)" msg !i in
          fail (prn xi) (prn yi) fm
        else
          incr i)
      y
  else
    let fm =
      Printf.sprintf "%s (stacks of different sizes - %d and %d)"
        msg
        lx
        ly in
    fail_msg fm

let make_not_equal_stack eq _ ?(msg="") x y =
  let lx = Stack.length x in
  let ly = Stack.length y in
  if lx = ly then begin
    let x' = Stack.copy x in
    try
      Stack.iter
        (fun yi ->
          let xi = Stack.pop x' in
          if not (eq xi yi) then raise Not_found)
        y;
      fail_msg msg
    with Not_found -> ()
  end

let make_equal_weak eq prn ?(msg="") x y =
  let lx = Weak.length x in
  let ly = Weak.length y in
  if lx = ly then
    for i = 0 to pred lx do
      let xi = Weak.get x i in
      let yi = Weak.get y i in
      let prn' = function
        | Some z -> "Some " ^ (prn z)
        | None -> "None" in
      let fail' () =
        let fm = Printf.sprintf "%s (at index %d)" msg i in
        fail (prn' xi) (prn' yi) fm in
      match xi, yi with
      | Some _, None
      | None, Some _ -> fail' ()
      | Some xi', Some yi' when not (eq xi' yi') -> fail' ()
      | _ -> ()
    done
  else
    let fm =
      Printf.sprintf "%s (weak arrays of different sizes - %d and %d)"
        msg
        lx
        ly in
    fail_msg fm

let make_not_equal_weak eq _ ?(msg="") x y =
  let lx = Weak.length x in
  let ly = Weak.length y in
  if lx = ly then begin
    let i = ref 0 in
    while (!i < lx) &&
      (match (Weak.get x !i), (Weak.get y !i) with
      | None, None -> true
      | Some xi, Some yi when eq xi yi -> true
      | _ -> false) do
      incr i
    done;
    if !i = lx then fail_msg msg
  end


(* Specialized functions *)

let equal_bool = make_equal (=) string_of_bool

let not_equal_bool = make_not_equal (=) string_of_bool

let equal_int = make_equal (=) string_of_int

let not_equal_int = make_not_equal (=) string_of_int

let equal_int32 = make_equal (=) Int32.to_string

let not_equal_int32 = make_not_equal (=) Int32.to_string

let equal_int64 = make_equal (=) Int64.to_string

let not_equal_int64 = make_not_equal (=) Int64.to_string

let equal_nativeint = make_equal (=) Nativeint.to_string

let not_equal_nativeint = make_not_equal (=) Nativeint.to_string

let equal_char = make_equal (=) Utils.string_of_char

let not_equal_char = make_not_equal (=) Utils.string_of_char

let equal_string = make_equal (=) Utils.string_of_string

let not_equal_string = make_not_equal (=) Utils.string_of_string

let make_float_eq eps =
  fun x y -> let delta = y -. x in (abs_float delta) <= eps

let equal_float ?(eps=epsilon_float) =
  equal ~eq:(make_float_eq eps) ~prn:string_of_float

let not_equal_float ?(eps=epsilon_float) =
  not_equal ~eq:(make_float_eq eps) ~prn:string_of_float

let make_complex_eq eps =
  fun x y ->
    let delta = y.Complex.re -. x.Complex.re in
    if (abs_float delta) > eps then
      false
    else
      let delta' = y.Complex.im -. x.Complex.im in
      (abs_float delta') <= eps

let equal_complex ?(eps=epsilon_float) =
  equal ~eq:(make_complex_eq eps) ~prn:Utils.string_of_complex

let not_equal_complex ?(eps=epsilon_float) =
  not_equal ~eq:(make_complex_eq eps) ~prn:Utils.string_of_complex


(* Miscellaneous *)

let is_true ?(msg="") x =
  if not x then fail "true" "false" msg

let is_false ?(msg="") x =
  if x then fail "false" "true" msg

let is_some ?(msg="") x =
  if x = None then fail "Some _" "None" msg

let is_none ?(msg="") x =
  if x <> None then fail "None" "Some _" msg

let raises ?(msg="") f =
  let exn = try ignore (f ()); false with _ -> true in
  if not exn then fail "any exception" "no exception" msg

let no_raise ?(msg="") f =
  let exn = try ignore (f ()); false with _ -> true in
  if exn then fail "no exception" "an exception" msg

let make_raises eq prn ?(msg="") f =
  try
    ignore (f ());
    fail_msg msg
  with e ->
    if not (eq e) then fail "a specific exception" (prn e) msg
