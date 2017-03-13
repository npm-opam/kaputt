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

let seed = 17

let () =
  Test.add_random_test
    ~title:"int * float"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.zip2 (Gen.make_int 1 1000) (Gen.transform (fun x -> mod_float x 1000.) Gen.float))
    (fun (x, y) -> (float x) +. y)
    [Spec.always ==> Spec.never]

let () =
  Test.add_random_test
    ~title:"int[1..10] * float[1..2["
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.zip2 (Gen.make_int 1 11) (Gen.make_float 1. 2.))
    (fun (x, y) -> (float x) +. y)
    [Spec.always ==> Spec.never]

let () =
  Test.add_random_test
    ~title:"bool * digit * letter * word"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.zip4 Gen.bool Gen.digit Gen.letter (Gen.word (Gen.make_int 1 5)))
    (fun _ -> 0)
    [Spec.always ==> Spec.never]

let () =
  Test.add_random_test
    ~title:"string option"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.option Gen.bool (Gen.word (Gen.make_int 1 9)))
    (fun _ -> 0)
    [Spec.always ==> Spec.never]

let () =
  Test.add_random_test
    ~title:"(string * number) list"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.list (Gen.make_int 0 4)
       (Gen.zip2 (Gen.word (Gen.make_int 1 9)) (Gen.number (Gen.make_int 2 4))))
    (fun _ -> 0)
    [Spec.always ==> Spec.never]

module IntMap = Map.Make (struct type t = int let compare = Pervasives.compare end)
module G = Gen.Map (IntMap) (struct type g = int let g = Gen.make_int 1 11 end)

let () =
  Test.add_random_test
    ~title:"(int[1..10] -> float[1..2[) map"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (G.gen (Gen.make_int 0 5) (Gen.make_float 1. 2.))
    (fun _ -> 0)
    [Spec.always ==> Spec.never]

let () =
  Test.add_random_test
    ~title:"int list (with reduction)"
    ~nb_runs:3
    ~reducer:Red.list
    ~random_src:(Gen.make_random_seed seed)
    (Gen.list (Gen.make_int 8 24) (Gen.make_int 0 16))
    (fun x -> x)
    [(fun x -> x <> []) ==> (List.mem 0)]

let () =
  Test.add_random_test
    ~title:"Bigarray"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (KaputtBigarray.Generator.bigarray
       Bigarray.int32
       Bigarray.c_layout
       (Gen.array (Gen.make_int 1 2) (Gen.make_int 2 16))
       (Gen.make_int32 0l 256l))
    (fun x -> x)
    [Spec.always ==> Spec.never]

let () =
  Test.add_random_test
    ~title:"Nums"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (KaputtNums.Generator.big_int
       (Gen.make_int 1 5))
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
