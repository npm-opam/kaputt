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


(* Enumerator definition *)

type 'a lazy_list = Nil | Cons of 'a * ('a lazy_list lazy_t)

type 'a t = (unit -> 'a lazy_list) * ('a -> string)


(* Utility functions *)

let rec iter_list f l =
  match l with
  | Nil -> ()
  | Cons (hd, tl) -> f hd; iter_list f (Lazy.force tl)

let iter f e =
  iter_list f ((fst e) ())


(* Predefined enumerators *)

let empty =
  (fun () -> Nil),
  (fun _ -> "")

let unit =
  (fun () -> Cons ((), lazy Nil)),
  Utils.string_of_unit

let bool =
  (fun () -> Cons (false, lazy (Cons (true, lazy Nil)))),
  string_of_bool

let create_int_functions ?(inf_eq=(<=)) succ prn =
  let rec f x y =
    fun () ->
      if inf_eq x y then
        Cons (x, lazy (f (succ x) y ()))
      else
        Nil in
  fun x y -> (f x y, prn)

let int = create_int_functions succ string_of_int

let int32 = create_int_functions Int32.succ Int32.to_string

let int64 = create_int_functions Int64.succ Int64.to_string

let nativeint = create_int_functions Nativeint.succ Nativeint.to_string

let make_char x y =
  let rec ch x y =
    fun () ->
      if x <= y then
        Cons (Char.chr x, lazy (ch (succ x) y ()))
      else
        Nil in
  (ch (Char.code x) (Char.code y)), Char.escaped

let char = make_char (Char.chr 0) (Char.chr 255)

module State = struct

  type 'a state = {
      init : 'a t array;
      curr : 'a lazy_list array;
    }

  let create a =
    { init = a;
      curr = Array.init (Array.length a) (fun i -> (fst a.(i)) ()); }

  let is_last s =
    let len = Array.length s.curr in
    let i = ref 0 in
    while (!i < len) && (s.curr.(!i) <> Nil) do
      incr i
    done;
    !i < len

  let rec update s i =
    if i >= 0 then
      match s.curr.(i) with
      | Cons (_, tl) ->
          let tmp = Lazy.force tl in
          s.curr.(i) <- tmp;
          if tmp = Nil then begin
            s.curr.(i) <- (fst s.init.(i)) ();
            update s (pred i)
          end
      | _ -> ()
          else
            s.curr.(0) <- Nil

  let next s =
    update s (pred (Array.length s.curr))

  let unwrap = function
    | Cons (hd, _) -> hd
    | _ -> assert false

  let get s i =
    unwrap s.curr.(i)

end

let create_state_based init encode print =
  let state = State.create (init ()) in
  let rec f () =
    if State.is_last state then
      Nil
    else Cons (encode state, lazy (State.next state; f ())) in
  f, print

let string ch len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.string";
  create_state_based
    (fun () -> (Array.init len (fun _ -> ch)))
    (fun s ->
      let res = String.create len in
      for i = 0 to pred len do
        res.[i] <- match s.State.curr.(i) with Cons (hd, _) -> hd | _ -> assert false
      done;
      res)
    Utils.string_of_string

let float x y steps =
  if (x = y) && (steps < 1) then
    invalid_arg "Kaputt.Enumerator.float"
  else if (x <> y) && (steps < 2) then
    invalid_arg "Kaputt.Enumerator.float";
  let delta = (y -. x) /. (float_of_int (pred steps)) in
  let x0 = x in
  let rec float x y k () =
    if x <= y then
      Cons (x, lazy (float (x0 +. delta *. (float_of_int k)) y (succ k) ()))
    else
      Nil in
  (float x y 1), string_of_float

let complex re im =
  create_state_based
    (fun () -> [| re; im |])
    (fun s ->
      { Complex.re = State.get s 0;
        Complex.im = State.get s 1; })
    Utils.string_of_complex


(* Enumerators for containers *)

let array elem len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.array";
  create_state_based
    (fun () -> Array.init len (fun _ -> elem))
    (fun s -> Array.init len (fun i -> State.get s i))
    (Utils.make_string_of_array (snd elem))

let list elem len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.list";
  create_state_based
    (fun () -> Array.init len (fun _ -> elem))
    (fun s -> Array.fold_right (fun e acc -> (State.unwrap e) :: acc) s.State.curr [])
    (Utils.make_string_of_list (snd elem))

let option elem =
  let f, p =
    create_state_based
      (fun () -> [| elem |])
      (fun s -> Some (State.get s 0))
      (snd elem) in
  (fun () -> Cons (None, lazy (f ()))),
  Utils.make_string_of_option p

let ref elem =
  create_state_based
    (fun () -> [| elem |])
    (fun s -> ref (State.get s 0))
    (Utils.make_string_of_ref (snd elem))

let buffer ch len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.buffer";
  create_state_based
    (fun () -> (Array.init len (fun _ -> ch)))
    (fun s ->
      let res = Buffer.create len in
      Array.iter (fun x -> Buffer.add_char res (State.unwrap x)) s.State.curr;
      res)
    Utils.string_of_buffer

let queue elem len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.queue";
  create_state_based
    (fun () -> Array.init len (fun _ -> elem))
    (fun s ->
      let res = Queue.create () in
      Array.iter (fun x -> Queue.push (State.unwrap x) res) s.State.curr;
      res)
    (Utils.make_string_of_queue (snd elem))

let stack elem len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.stack";
  create_state_based
    (fun () -> Array.init len (fun _ -> elem))
    (fun s ->
      let res = Stack.create () in
      Array.iter (fun x -> Stack.push (State.unwrap x) res) s.State.curr;
      res)
    (Utils.make_string_of_stack (snd elem))

let weak elem len =
  if len < 0 then invalid_arg "Kaputt.Enumerator.weak";
  create_state_based
    (fun () -> Array.init len (fun _ -> elem))
    (fun s ->
      let res = Weak.create len in
      for i = 0 to (pred len) do
        Weak.set res i (State.get s i)
      done;
      res)
    (Utils.make_string_of_weak (snd elem))


(* Combinators for files *)

let file_chars fn =
  let rec fc ch =
    fun () ->
      try
        Cons (input_char ch, lazy (fc ch ()))
      with End_of_file -> close_in_noerr ch; Nil in
  (fc (open_in fn)), Utils.string_of_char

let file_bytes fn =
  let rec fb ch =
    fun () ->
      try
        Cons (input_byte ch, lazy (fb ch ()))
      with End_of_file -> close_in_noerr ch; Nil in
  (fb (open_in fn)), string_of_int

let file_lines fn =
  let rec fl ch =
    fun () ->
      try
        Cons (input_line ch, lazy (fl ch ()))
      with End_of_file -> close_in_noerr ch; Nil in
  (fl (open_in fn)), Utils.string_of_string

let file_values fn p =
  let rec fv ch =
    fun () ->
      try
        Cons (input_value ch, lazy (fv ch ()))
      with End_of_file -> close_in_noerr ch; Nil in
  (fv (open_in fn)), p


(* Combinators over enumerators *)

let lift x s =
  let s = String.copy s in
  (fun () -> Cons (x, lazy Nil)),
  (fun _ -> String.copy s)

let lift_list x prn =
  let rec l rem =
    fun () ->
      match rem with
      | hd :: tl -> Cons (hd, lazy (l tl ()))
      | [] -> Nil in
  (l x), prn

let lift_array x prn =
  let rec a i =
    fun () ->
      if i < Array.length x then
        Cons (x.(i), lazy (a (succ i) ()))
      else
        Nil in
  (a 0), prn

let lift_string x =
  let rec s i =
    fun () ->
      if i < String.length x then
        Cons (x.[i], lazy (s (succ i) ()))
      else
        Nil in
  (s 0), Utils.string_of_char

let filter p e =
  let rec f l =
    match l with
    | Nil -> Nil
    | Cons (hd, tl) ->
        if p hd then
          Cons (hd, lazy (f (Lazy.force tl)))
        else
          f (Lazy.force tl) in
  (fun () -> f ((fst e) ())),
  (snd e)

let transform f e =
  let rec t l =
    match l with
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (f hd, lazy (t (Lazy.force tl))) in
  (fun () -> t ((fst e) ())),
  (snd e)

let sequence (l : 'a t list) =
  let rec s c l =
    match c with
    | Nil -> (match l with [] -> Nil | hd :: tl -> s ((fst hd) ()) tl)
    | Cons (hd, tl) -> Cons (hd, lazy (s (Lazy.force tl) l)) in
  (fun () ->
    match l with
    | [] -> Nil
    | hd :: tl -> s ((fst hd) ()) tl),
  (snd (List.hd l))

let for_each (x : int) (y : int) (g : int -> 'a t) =
  let rec fe c x y =
    match c with
    | Nil -> (if x <= y then fe ((fst (g x)) ()) (succ x) y else Nil)
    | Cons (hd, tl) -> Cons (hd, lazy (fe (Lazy.force tl) x y)) in
  (fun () ->
    if x <= y then
      fe ((fst (g x)) ()) (succ x) y
    else
      Nil),
  (snd (g x))

let magic = Obj.magic

let map1 f p e1 =
  create_state_based
    (fun () -> [| e1 |])
    (fun s -> f (State.get s 0))
    p

let map2 f p (e1, e2) =
  create_state_based
    (fun () -> [| magic e1; magic e2 |])
    (fun s -> f (magic (State.get s 0)) (magic (State.get s 1)))
    p

let map3 f p (e1, e2, e3) =
  create_state_based
    (fun () -> [| magic e1; magic e2; magic e3 |])
    (fun s -> f (magic (State.get s 0)) (magic (State.get s 1)) (magic (State.get s 2)))
    p

let map4 f p (e1, e2, e3, e4) =
  create_state_based
    (fun () -> [| magic e1; magic e2; magic e3; magic e4 |])
    (fun s -> f (magic (State.get s 0)) (magic (State.get s 1)) (magic (State.get s 2)) (magic (State.get s 3)))
    p

let map5 f p (e1, e2, e3, e4, e5) =
  create_state_based
    (fun () -> [| magic e1; magic e2; magic e3; magic e4; magic e5 |])
    (fun s -> f (magic (State.get s 0)) (magic (State.get s 1)) (magic (State.get s 2)) (magic (State.get s 3)) (magic (State.get s 4)))
    p

external zip1 : 'a t -> 'a t = "%identity"

let zip2 e1 e2 =
  create_state_based
    (fun () -> [| magic e1; magic e2 |])
    (fun s -> (magic (State.get s 0)), (magic (State.get s 1)))
    (Utils.make_string_of_tuple2 (snd e1) (snd e2))

let zip3 e1 e2 e3 =
  create_state_based
    (fun () -> [| magic e1; magic e2; magic e3 |])
    (fun s -> (magic (State.get s 0)), (magic (State.get s 1)), (magic (State.get s 2)))
    (Utils.make_string_of_tuple3 (snd e1) (snd e2) (snd e3))

let zip4 e1 e2 e3 e4 =
  create_state_based
    (fun () -> [| magic e1; magic e2; magic e3; magic e4 |])
    (fun s -> (magic (State.get s 0)), (magic (State.get s 1)), (magic (State.get s 2)), (magic (State.get s 3)))
    (Utils.make_string_of_tuple4 (snd e1) (snd e2) (snd e3) (snd e4))

let zip5 e1 e2 e3 e4 e5 =
  create_state_based
    (fun () -> [| magic e1; magic e2; magic e3; magic e4; magic e5 |])
    (fun s -> (magic (State.get s 0)), (magic (State.get s 1)), (magic (State.get s 2)), (magic (State.get s 3)), (magic (State.get s 4)))
    (Utils.make_string_of_tuple5 (snd e1) (snd e2) (snd e3) (snd e4) (snd e5))
