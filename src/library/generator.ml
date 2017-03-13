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


(* Random source *)

type random = Random.State.t

let make_random () = Random.State.make_self_init ()

let make_random_seed x = Random.State.make [| x |]

let make_random_full x = Random.State.make x


(* Generator definition *)

type 'a t = (random -> 'a) * ('a -> string)


(* 64-bit compatible int generator *)

let random_state_int =
  match Sys.word_size with
  | 32 -> Random.State.int
  | 64 -> (fun r x ->
      if x < 1073741824 then
        Random.State.int r x
      else
        Int64.to_int (Random.State.int64 r (Int64.of_int x)))
  | _ -> assert false


(* Predefined generators *)

let unit =
  (fun r -> ignore (Random.State.bool r)),
  Utils.string_of_unit

let bool =
  (fun r -> Random.State.bool r),
  string_of_bool

let make_bool w1 w2 =
  if w1 < 0 then invalid_arg "Kaputt.Generator.make_bool";
  if w2 < 0 then invalid_arg "Kaputt.Generator.make_bool";
  (fun r ->
    let w = random_state_int r (w1 + w2) in
    w < w1),
  string_of_bool

let create_int_functions id gen max neg add sub prn =
  let id' = "Kaputt.Generator.make_" ^ id in
  let std_gen =
    (fun r ->
      let s = Random.State.bool r in
      let x = gen r max in
      if s then x else neg x),
    prn in
  let pos_gen =
    (fun r -> gen r max),
    prn in
  let neg_gen =
    (fun r -> neg (gen r max)),
    prn in
  let make x y =
    if (compare x y) >= 0 then invalid_arg id';
    (fun r -> let d = sub y x in add (gen r d) x),
    prn in
  std_gen, pos_gen, neg_gen, make

let int, pos_int, neg_int, make_int =
  create_int_functions
    "int"
    random_state_int
    max_int
    (~-)
    (+)
    (-)
    string_of_int

let int32, pos_int32, neg_int32, make_int32 =
  create_int_functions
    "int32"
    Random.State.int32
    Int32.max_int
    Int32.neg
    Int32.add
    Int32.sub
    Int32.to_string

let int64, pos_int64, neg_int64, make_int64 =
  create_int_functions
    "int64"
    Random.State.int64
    Int64.max_int
    Int64.neg
    Int64.add
    Int64.sub
    Int64.to_string

let nativeint, pos_nativeint, neg_nativeint, make_nativeint =
  create_int_functions
    "nativeint"
    Random.State.nativeint
    Nativeint.max_int
    Nativeint.neg
    Nativeint.add
    Nativeint.sub
    Nativeint.to_string

let char =
  (fun r -> Char.chr (Random.State.int r 256)),
  Char.escaped

let create_digit n =
  (fun r -> Char.chr ((Random.State.int r n) + (Char.code '0'))),
  Char.escaped

let digit = create_digit 10

let digit_bin = create_digit 2

let digit_oct = create_digit 8

let digit_hex =
  (fun r ->
    let x = Random.State.int r 16 in
    if x < 10 then
      Char.chr (x + (Char.code '0'))
    else
      Char.chr ((x - 10) + (Char.code 'A'))),
  Char.escaped

let letter =
  (fun r ->
    let c = Random.State.bool r in
    let l = Random.State.int r 26 in
    let x = Char.chr (l + (Char.code 'a')) in
    if c then Char.uppercase x else x),
  Char.escaped

let alphanum =
  (fun r ->
    let x = Random.State.int r 63 in
    if x < 10 then
      Char.chr (x + (Char.code '0'))
    else if x = 10 then
      '_'
    else if (x >= 11) && (x <= 36) then
      Char.chr ((x - 11) + (Char.code 'a'))
    else
      Char.chr ((x - 37) + (Char.code 'A'))),
  Char.escaped

let string (gen_l, _) (gen_c, _) =
  (fun r ->
    let len = gen_l r in
    let res = String.create len in
    for i = 0 to pred len do
      res.[i] <- gen_c r
    done;
    res),
  Utils.string_of_string

let strings sep (gen_l, _) (gen_s, _) =
  (fun r ->
    let len = gen_l r in
    let lst = ref [] in
    for _i = 1 to len do
      lst := (gen_s r) :: !lst
    done;
    String.concat sep (List.rev !lst)),
  Utils.string_of_string

let number l =
  string l digit

let number_bin l =
  string l digit_bin

let number_oct l =
  string l digit_oct

let number_hex l =
  string l digit_hex

let word l =
  string l letter

let words l l' =
  strings " " l (word l')

let float =
  (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.float r max_float in
    if s then x else -.x),
  string_of_float

let make_float x y =
  if (compare x y) >= 0 then invalid_arg "Kaputt.Generator.make_float";
  (fun r -> let d = y -. x in (Random.State.float r d) +. x),
  string_of_float

let complex (gen_re, _) (gen_im, _) =
  (fun r ->
    let re_val = gen_re r in
    let im_val = gen_im r in
    { Complex.re = re_val; Complex.im = im_val }),
  Utils.string_of_complex


(* Generators for functions *)

type 'a outcome =
  | Value of 'a
  | Exception of exn

let total_function (gen, _) =
  (fun r ->
    let memo = Hashtbl.create 17 in
    let func x =
      try
        Hashtbl.find memo x
      with Not_found ->
        let y = gen r in
        Hashtbl.add memo x y;
        y in
    func),
  (fun _ -> String.copy "<fun>")

let partial_function (gen, _) =
  let return = function
    | Value v -> v
    | Exception e -> raise e in
  (fun r ->
    let memo = Hashtbl.create 17 in
    let func x =
      try
        return (Hashtbl.find memo x)
      with Not_found ->
        let y = gen r in
        Hashtbl.add memo x y;
        return y in
    func),
  (fun _ -> String.copy "<fun>")


(* Generators for containers *)

let array (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    Array.init len (fun _ -> gen_e r)),
  Utils.make_string_of_array prn_e

let list (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = ref [] in
    for _i = 1 to len do
      res := (gen_e r) :: !res;
    done;
    List.rev !res),
  Utils.make_string_of_list prn_e

let option (gen_k, _) (gen_e, prn_e) =
  (fun r ->
    let b = gen_k r in
    if b then
      Some (gen_e r)
    else
      None),
  Utils.make_string_of_option prn_e

let ref (gen_e, prn_e) =
  (fun r -> ref (gen_e r)),
  Utils.make_string_of_ref prn_e

let buffer (gen_e, _) =
  (fun r ->
    let buf = Buffer.create 16 in
    Buffer.add_string buf (gen_e r);
    buf),
  Utils.string_of_buffer

module type Gen = sig
  type g
  val g : g t
end

module Map (M : Map.S) (G : Gen with type g = M.key) = struct
  let gen (gen_l, _) (gen_v, prn_v) =
    let (gen_k, prn_k) = G.g in
    (fun r ->
      let len = gen_l r in
      let res = Pervasives.ref M.empty in
      let size = Pervasives.ref 0 in
      while !size < len do
        let k = gen_k r in
        if not (M.mem k !res) then begin
          let v = gen_v r in
          res := M.add k v !res;
          incr size
        end
      done;
      !res),
    (fun m ->
      let l = M.fold
          (fun k v acc -> (Printf.sprintf "%s -> %s" (prn_k k) (prn_v v)) :: acc)
          m
          [] in
      String.concat "; " (List.rev l))
end

module Set (S : Set.S) (G : Gen with type g = S.elt) = struct
  let gen (gen_l, _) =
    let (gen_e, prn_e) = G.g in
    (fun r ->
      let len = gen_l r in
      let res = Pervasives.ref S.empty in
      let size = Pervasives.ref 0 in
      while !size < len do
        let e = gen_e r in
        if not (S.mem e !res) then begin
          res := S.add e !res;
          incr size
        end
      done;
      !res),
    (fun s ->
      let l = S.fold (fun e acc -> (prn_e e) :: acc) s [] in
      String.concat "; " (List.rev l))
end

let hashtbl (gen_l, _) (gen_k, prn_k) (gen_v, prn_v) =
  (fun r ->
    let len = gen_l r in
    let res = Hashtbl.create len in
    let size = Pervasives.ref 0 in
    while !size < len do
      let k = gen_k r in
      if not (Hashtbl.mem res k) then begin
        let v = gen_v r in
        Hashtbl.add res k v;
        incr size
      end
    done;
    res),
  Utils.make_string_of_hashtbl prn_k prn_v

let queue (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = Queue.create () in
    for _i = 1 to len do
      let e = gen_e r in
      Queue.push e res
    done;
    res),
  Utils.make_string_of_queue prn_e

let stack (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = Stack.create () in
    for _i = 1 to len do
      let e = gen_e r in
      Stack.push e res
    done;
    res),
  Utils.make_string_of_stack prn_e

let weak (gen_l, _) (gen_e, prn_e) =
  (fun r ->
    let len = gen_l r in
    let res = Weak.create len in
    for i = 0 to (pred len) do
      let e = gen_e r in
      Weak.set res i e
    done;
    res),
  Utils.make_string_of_weak prn_e

module Weak (W : Weak.S) (G : Gen with type g = W.data) = struct
  let gen (gen_l, _) =
    let (gen_e, prn_e) = G.g in
    (fun r ->
      let len = gen_l r in
      let res = W.create len in
      let size = Pervasives.ref 0 in
      while !size < len do
        let e = gen_e r in
        if not (W.mem res e) then begin
          W.add res e;
          incr size
        end
      done;
      res),
    (fun w ->
      let l = W.fold (fun e acc -> (prn_e e) :: acc) w [] in
      String.concat "; " (List.rev l))
end


(* Combinators over generators *)

let sum_list = List.fold_left
    (fun acc elem ->
      if elem >= 0 then
        acc + elem
      else
        invalid_arg "negative weight")
    0

let sum_array = Array.fold_left
    (fun acc elem ->
      if elem >= 0 then
        acc + elem
      else
        invalid_arg "negative weight")
    0

let lift x s =
  let s' = String.copy s in
  (fun _ -> x),
  (fun _ -> String.copy s')

let select_list l f =
  if l = [] then invalid_arg "Kaputt.Generator.select_list";
  (fun r ->
    let len = List.length l in
    let i = random_state_int r len in
    List.nth l i),
  f

let rec get_list n = function
  | [(v, _)] -> v
  | (v, w) :: _ when n < w -> v
  | (_, w) :: tl -> get_list (n - w) tl
  | [] -> failwith "internal error"

let select_list_weighted l f =
  if l = [] then invalid_arg "Kaputt.Generator.select_list_weighted";
  let total = sum_list (List.map snd l) in
  (fun r ->
    let w = random_state_int r total in
    get_list w l),
  f

let select_array a f =
  if a = [||] then invalid_arg "Kaputt.Generator.select_array";
  (fun r ->
    let len = Array.length a in
    let i = random_state_int r len in
    a.(i)),
  f

let rec get_array n a i =
  let w = snd a.(i) in
  if (n < w) || (i = pred (Array.length a)) then
    fst a.(i)
  else
    get_array (n - w) a (succ i)

let select_array_weighted a f =
  if a = [||] then invalid_arg "Kaputt.Generator.select_array_weighted";
  let total = sum_array (Array.map snd a) in
  (fun r ->
    let w = random_state_int r total in
    get_array w a 0),
  f

let choose_list l =
  if l = [] then invalid_arg "Kaputt.Generator.choose_list";
  (fun r ->
    let len = List.length l in
    let i = random_state_int r len in
    let (gen, _) = List.nth l i in
    gen r),
  (snd (List.hd l))

let choose_list_weighted l =
  if l = [] then invalid_arg "Kaputt.Generator.choose_list_weighted";
  let total = sum_list (List.map snd l) in
  (fun r ->
    let w = random_state_int r total in
    let (gen, _) = get_list w l in
    gen r),
  (snd (fst (List.hd l)))

let choose_array a =
  if a = [||] then invalid_arg "Kaputt.Generator.choose_array";
  (fun r ->
    let len = Array.length a in
    let i = random_state_int r len in
    let (gen, _) = a.(i) in
    gen r),
  (snd a.(0))

let choose_array_weighted a =
  if a = [||] then invalid_arg "Kaputt.Generator.choose_array_weighted";
  let total = sum_array (Array.map snd a) in
  (fun r ->
    let w = random_state_int r total in
    let (gen, _) = get_array w a 0 in
    gen r),
  (snd (fst a.(0)))

let filter p (gen_g, prn_g) =
  (fun r ->
    let rec get () =
      let x = gen_g r in
      if p x then x else get () in
    get ()),
  prn_g

let transform f (gen_g, prn_g) =
  (fun r -> f (gen_g r)),
  prn_g

let map1 f p (gen_g, _) =
  (fun r -> f (gen_g r)),
  p

let map2 f p ((gen_g1, _), (gen_g2, _)) =
  (fun r ->
    let x1 = gen_g1 r in
    let x2 = gen_g2 r in
    f x1 x2),
  p

let map3 f p ((gen_g1, _), (gen_g2, _), (gen_g3, _)) =
  (fun r ->
    let x1 = gen_g1 r in
    let x2 = gen_g2 r in
    let x3 = gen_g3 r in
    f x1 x2 x3),
  p

let map4 f p ((gen_g1, _), (gen_g2, _), (gen_g3, _), (gen_g4, _)) =
  (fun r ->
    let x1 = gen_g1 r in
    let x2 = gen_g2 r in
    let x3 = gen_g3 r in
    let x4 = gen_g4 r in
    f x1 x2 x3 x4),
  p

let map5 f p ((gen_g1, _), (gen_g2, _), (gen_g3, _), (gen_g4, _), (gen_g5, _)) =
  (fun r ->
    let x1 = gen_g1 r in
    let x2 = gen_g2 r in
    let x3 = gen_g3 r in
    let x4 = gen_g4 r in
    let x5 = gen_g5 r in
    f x1 x2 x3 x4 x5),
  p

external zip1 : 'a t -> 'a t = "%identity"

let zip2 (f1, c1) (f2, c2) =
  (fun r ->
    let x1 = f1 r in
    let x2 = f2 r in
    (x1, x2)),
  (Utils.make_string_of_tuple2 c1 c2)

let zip3 (f1, c1) (f2, c2) (f3, c3) =
  (fun r ->
    let x1 = f1 r in
    let x2 = f2 r in
    let x3 = f3 r in
    (x1, x2, x3)),
  (Utils.make_string_of_tuple3 c1 c2 c3)

let zip4 (f1, c1) (f2, c2) (f3, c3) (f4, c4) =
  (fun r ->
    let x1 = f1 r in
    let x2 = f2 r in
    let x3 = f3 r in
    let x4 = f4 r in
    (x1, x2, x3, x4)),
  (Utils.make_string_of_tuple4 c1 c2 c3 c4)

let zip5 (f1, c1) (f2, c2) (f3, c3) (f4, c4) (f5, c5) =
  (fun r ->
    let x1 = f1 r in
    let x2 = f2 r in
    let x3 = f3 r in
    let x4 = f4 r in
    let x5 = f5 r in
    (x1, x2, x3, x4, x5)),
  (Utils.make_string_of_tuple5 c1 c2 c3 c4 c5)


(* Currying functions *)

let apply1 f x = f x

let apply2 f (x, y) = f x y

let apply3 f (x, y, z) = f x y z

let apply4 f (x, y, z, t) = f x y z t

let apply5 f (x, y, z, t, u) = f x y z t u

let tuple_apply1 f x = f x

let tuple_apply2 f x y = f (x, y)

let tuple_apply3 f x y z = f (x, y, z)

let tuple_apply4 f x y z t = f (x, y, z, t)

let tuple_apply5 f x y z t u = f (x, y, z, t, u)
