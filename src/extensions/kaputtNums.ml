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

let big_minus_two = Big_int.big_int_of_int ~-2
let big_minus_one = Big_int.big_int_of_int ~-1
let big_zero = Big_int.zero_big_int
let big_one = Big_int.unit_big_int
let big_two = Big_int.big_int_of_int 2

let num_minus_two = Num.Int ~-2
let num_minus_one = Num.Int ~-1
let num_zero = Num.Int ~-1
let num_one = Num.Int 1
let num_two = Num.Int 2


(* Assertion functions *)

module Assertion = struct

  let equal_big_int = Kaputt.Assertion.make_equal Big_int.eq_big_int Big_int.string_of_big_int

  let not_equal_big_int = Kaputt.Assertion.make_not_equal Big_int.eq_big_int Big_int.string_of_big_int

  let equal_num = Kaputt.Assertion.make_equal Num.eq_num Num.string_of_num

  let not_equal_num = Kaputt.Assertion.make_not_equal Num.eq_num Num.string_of_num

end


(* Generators *)

module Generator = struct

  let gen_big_int_digit, _ = Kaputt.Generator.make_int 0 10

  let big_int (gen_l, _) =
    (fun r ->
      let s = Random.State.bool r in
      let len = gen_l r in
      let res = ref Big_int.zero_big_int in
      for _i = 1 to len do
        res := Big_int.add_int_big_int
            (gen_big_int_digit r)
            (Big_int.mult_int_big_int 10 !res)
      done;
      if s then !res else Big_int.minus_big_int !res),
    Big_int.string_of_big_int

  let num (gen_a, _) (gen_b, _) =
    (fun r ->
      let a = gen_a r in
      let b = gen_b r in
      Num.div_num (Num.Big_int a) (Num.Big_int b)),
    Num.string_of_num

end


(* Enumerators *)

module Enumerator = struct

  let big_int =
    Kaputt.Enumerator.create_int_functions
      ~inf_eq:Big_int.le_big_int
      Big_int.succ_big_int
      Big_int.string_of_big_int

  let num n d =
    Kaputt.Enumerator.create_state_based
      (fun () -> [| n; d |])
      (fun s ->
        Num.div_num
          (Num.Big_int (Kaputt.Enumerator.State.get s 0))
          (Num.Big_int (Kaputt.Enumerator.State.get s 1)))
      Num.string_of_num

end


(* Specifications *)

module Specification = struct

let is_pos_big_int, is_neg_big_int,
    is_zero_big_int, is_nonzero_big_int,
    is_even_big_int, is_odd_big_int =
  Kaputt.Specification.create_int_functions
    Big_int.compare_big_int
    big_zero
    Big_int.mod_big_int
    big_two

let is_pos_num, is_neg_num,
    is_zero_num, is_nonzero_num,
    is_even_num, is_odd_num =
  Kaputt.Specification.create_int_functions
    Num.compare_num
    num_zero
    Num.mod_num
    num_two

end


(* Reducers *)

module Reducer = struct

  let big_int x =
    [ big_minus_two; big_minus_one; big_zero; big_one; big_two; Big_int.div_big_int x big_two ]

  let num x =
    [ num_minus_two; num_minus_one; num_zero; num_one; num_two; Num.div_num x num_two ]

end
