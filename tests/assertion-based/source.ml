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

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
let id x = x

module Prn = struct type t = string let to_string = id end
module AssertMap = Assert.Map (StringMap) (Prn)
module AssertSet = Assert.Set (StringSet) (Prn)

let ht1 = Hashtbl.create 17
let ht2 = Hashtbl.create 17
let () =
  Hashtbl.add ht1 "k" "abc";
  Hashtbl.add ht1 "k" "def";
  Hashtbl.add ht2 "k" "abc"

let m1 = StringMap.add "k1" "abc" (StringMap.add "k2" "def" StringMap.empty)
let m2 = StringMap.add "k1" "abc" (StringMap.add "k2" "ghi" StringMap.empty)

let s1 = StringSet.add "abc" (StringSet.add "def" StringSet.empty)
let s2 = StringSet.add "abc" StringSet.empty

let q1 = Queue.create ()
let q2 = Queue.create ()
let () =
  Queue.push "abc" q1;
  Queue.push "def" q1;
  Queue.push "abc" q2

let st1 = Stack.create ()
let st2 = Stack.create ()
let () =
  Stack.push "abc" st1;
  Stack.push "def" st1;
  Stack.push "abc" st2

let w1 = Weak.create 2
let w2 = Weak.create 2
let () =
  Weak.set w1 0 (Some "abc");
  Weak.set w1 1 (Some "def");
  Weak.set w2 0 (Some "abc")


let () =
  Test.add_simple_test
    ~title:"function builders"
    (fun () ->
      Assert.make_equal_array (=) id [| "abc"; "def" |] [| "abc"; "def" |];
      Assert.make_not_equal_array (=) id [| "abc"; "def" |] [| "abc"; "ghi" |];
      Assert.make_equal_list (=) id [ "abc"; "def" ] [ "abc"; "def" ];
      Assert.make_not_equal_list (=) id [ "abc"; "def" ] [ "abc"; "ghi" ];
      Assert.make_equal_hashtbl (=) id id ht1 ht1;
      Assert.make_not_equal_hashtbl (=) id id ht1 ht2;
      AssertMap.make_equal (=) id m1 m1;
      AssertMap.make_not_equal (=) id m1 m2;
      AssertSet.equal s1 s1;
      AssertSet.not_equal s1 s2;
      Assert.make_equal_queue (=) id q1 q1;
      Assert.make_not_equal_queue (=) id q1 q2;
      Assert.make_equal_stack (=) id st1 st1;
      Assert.make_not_equal_stack (=) id st1 st2;
      Assert.make_equal_weak (=) id w1 w1;
      Assert.make_not_equal_weak (=) id w1 w2)

let () =
  Test.add_simple_test
    ~title:"specialized functions"
    (fun () ->
      Assert.equal_bool true true;
      Assert.not_equal_bool false true;
      Assert.equal_int 3 3;
      Assert.not_equal_int 3 5;
      Assert.equal_int32 3l 3l;
      Assert.not_equal_int32 3l 5l;
      Assert.equal_int64 3L 3L;
      Assert.not_equal_int64 3L 5L;
      Assert.equal_nativeint 3n 3n;
      Assert.not_equal_nativeint 3n 5n;
      Assert.equal_char 'e' 'e';
      Assert.not_equal_char 'e' 'f';
      Assert.equal_string "azerty" "azerty";
      Assert.not_equal_string "azerty" "qwerty";
      Assert.equal_float 1. 1.;
      Assert.not_equal_float 1. 2.)

let () =
  Test.add_simple_test
    ~title:"equal_bool"
    (fun () -> Assert.equal_bool true false)

let () =
  Test.add_simple_test
    ~title:"not_equal_bool"
    (fun () -> Assert.not_equal_bool false false)

let () =
  Test.add_simple_test
    ~title:"equal_int"
    (fun () -> Assert.equal_int 3 7)

let () =
  Test.add_simple_test
    ~title:"not_equal_int"
    (fun () -> Assert.not_equal_int 3 3)

let () =
  Test.add_simple_test
    ~title:"equal_int32"
    (fun () -> Assert.equal_int32 3l 7l)

let () =
  Test.add_simple_test
    ~title:"not_equal_int32"
    (fun () -> Assert.not_equal_int32 3l 3l)

let () =
  Test.add_simple_test
    ~title:"equal_int64"
    (fun () -> Assert.equal_int64 3L 7L)

let () =
  Test.add_simple_test
    ~title:"not_equal_int64"
    (fun () -> Assert.not_equal_int64 3L 3L)

let () =
  Test.add_simple_test
    ~title:"equal_nativeint"
    (fun () -> Assert.equal_nativeint 3n 7n)

let () =
  Test.add_simple_test
    ~title:"not_equal_nativeint"
    (fun () -> Assert.not_equal_nativeint 3n 3n)

let () =
  Test.add_simple_test
    ~title:"equal_char"
    (fun () -> Assert.equal_char 'a' 'z')

let () =
  Test.add_simple_test
    ~title:"not_equal_char"
    (fun () -> Assert.not_equal_char 'e' 'e')

let () =
  Test.add_simple_test
    ~title:"equal_string"
    (fun () -> Assert.equal_string "azerty" "qwerty")

let () =
  Test.add_simple_test
    ~title:"not_equal_string"
    (fun () -> Assert.not_equal_string "azerty" "azerty")

let () =
  Test.add_simple_test
    ~title:"equal_float"
    (fun () -> Assert.equal_float 1. 2.)

let () =
  Test.add_simple_test
    ~title:"not_equal_float"
    (fun () -> Assert.not_equal_float 1. 1.)

let () =
  Test.add_simple_test
    ~title:"miscellaneous"
    (fun () ->
      Assert.is_true true;
      Assert.is_false false;
      Assert.is_some (Some 3);
      Assert.is_none None;
      Assert.raises (fun () -> failwith "msg");
      Assert.no_raise ignore;
      Assert.make_raises
        (function Not_found -> true | _ -> false)
        Printexc.to_string
        (fun () -> raise Not_found))

let () =
  Test.add_simple_test
    ~title:"is_true"
    (fun () -> Assert.is_true false)

let () =
  Test.add_simple_test
    ~title:"is_false"
    (fun () -> Assert.is_false true)

let () =
  Test.add_simple_test
    ~title:"is_some"
    (fun () -> Assert.is_some None)

let () =
  Test.add_simple_test
    ~title:"is_none"
    (fun () -> Assert.is_none (Some ""))

let () =
  Test.add_simple_test
    ~title:"raises"
    (fun () -> Assert.raises (fun () -> ()))

let () =
  Test.add_simple_test
    ~title:"no_raise"
    (fun () -> Assert.no_raise (fun () -> failwith "msg"))

let () =
  Test.add_simple_test
    ~title:"make_raises"
    (fun () ->
      Assert.make_raises
        (function Not_found -> false | _ -> true)
        Printexc.to_string
        (fun () -> raise Not_found))

let () =
  Test.add_simple_test
    ~title:"Bigarray"
    (fun () ->
      let a = Bigarray.Genarray.create Bigarray.int32 Bigarray.c_layout [| 3 |] in
      KaputtBigarray.Assertion.make_equal_bigarray (=) Int32.to_string a a)

let () =
  Test.add_simple_test
    ~title:"Nums"
    (fun () ->
      let x = Big_int.big_int_of_int 8 in
      let y = Big_int.big_int_of_int 16 in
      KaputtNums.Assertion.equal_big_int x x;
      KaputtNums.Assertion.not_equal_big_int x y)

let () =
  List.iter
    (fun (impl, name) ->
      Test.add_simple_test
        ~title:(Printf.sprintf "Mock (%s)" name)
        (fun () ->
          let eq_int_list = Assert.make_equal_list (=) string_of_int in
          let f = impl in
          let i = [0; 1; 2; 0] in
          let o = List.map (Mock.func f) i in
          let o' = [1; 2; 3; 1] in
          eq_int_list o' o;
          eq_int_list i (Mock.calls f);
          Assert.equal_int 4 (Mock.total f)))
    [Mock.from_mapping [0, 1; 1, 2; 2, 3], "mapping";
     Mock.from_sequence [0, 1; 1, 2; 2, 3; 0, 1], "sequence";
     Mock.from_function succ, "function"]


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
