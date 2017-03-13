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

open Kaputt.Abbreviations

let () =
  Test.add_enum_test
    ~title:"empty"
    Enum.empty
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"bool"
    Enum.bool
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"int"
    (Enum.int 1 5)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"int32"
    (Enum.int32 1l 5l)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"int64"
    (Enum.int64 1L 5L)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"nativeint"
    (Enum.nativeint 1n 5n)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"char"
    (Enum.make_char 'e' 'i')
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"[aa..cc]"
    (Enum.string (Enum.make_char 'a' 'c') 2)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"float[1,1]"
    (Enum.float 1. 1. 1)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"float[1,4]"
    (Enum.float 1. 4. 4)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"bool array"
    (Enum.array Enum.bool 2)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"bool list"
    (Enum.list Enum.bool 2)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"bool option"
    (Enum.option Enum.bool)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"[1..10 | even]"
    (Enum.filter (fun x -> (x mod 2) = 0) (Enum.int 1 10))
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"[1..10] [13..15]"
    (Enum.sequence [Enum.int 1 10; Enum.int 13 15])
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"[10*i..1O*i+5 | i <- 1..2]"
    (Enum.for_each 1 2 (fun i -> Enum.int (i * 10) (i * 10 + 5)))
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"list [1; 2; 3]"
    (Enum.lift_list [ 1; 2; 3] string_of_int)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"array [| 4; 5; 6 |]"
    (Enum.lift_array [| 4; 5; 6 |] string_of_int)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"string \"azerty\""
    (Enum.lift_string "azerty")
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"characters from 'data' file"
    (Enum.file_chars "data")
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"bytes from 'data' file"
    (Enum.file_bytes "data")
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"lines from 'data' file"
    (Enum.file_lines "data")
    (fun _ -> ())
    [Spec.always ==> Spec.never]

type t = { x : int; y : string; }
let string_of_t { x; y } = Printf.sprintf "%d is %S" x y
let () =
  Test.add_enum_test
    ~title:"t values from 'data2' file"
    (Enum.file_values "data2" string_of_t)
    (fun _ -> ())
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"Bigarray"
    (KaputtBigarray.Enumerator.bigarray
       Bigarray.int32
       Bigarray.c_layout
       [| 3 |]
       (Enum.int32 0l 2l))
    (fun x -> x)
    [Spec.always ==> Spec.never]

let () =
  Test.add_enum_test
    ~title:"Nums"
    (KaputtNums.Enumerator.big_int
       Big_int.zero_big_int
       (Big_int.big_int_of_int 8))
    (fun x -> x)
    [Spec.always ==> Spec.never]


let () =
  Test.launch_tests ~output:(Test.Text_output (open_out "result")) ();
  let diff = Shell.run (Shell.diff ~options:["-q"] "reference" "result") in
  if diff <> 0 then begin
    print_endline "failed";
    exit 1
  end else begin
    ignore (Shell.run (Shell.rm ~options:["-f"] ["result"]));
    exit 0
  end
