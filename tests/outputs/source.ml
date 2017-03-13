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

let odd x = (x mod 2) = 1
let even x = (x mod 2) = 0
let pos x = x >= 0
let small_pos x = (x >= 0) && (x <= 100)
let rec fact = function
| 0 -> 1
| n -> n * (fact (pred n))
let fact' x = Printf.printf " --> %d\n" x; fact x
let no_print _ = ""
let size = (fun s -> if (String.length s) < 6 then "short" else "long")

let seed = 17

let () =
  Test.add_random_test
    ~title:"succ test"
    ~nb_runs:100
    ~random_src:(Gen.make_random_seed seed)
    Gen.int
    succ
    [odd ==> even]

let () =
  Test.add_random_test
    ~nb_runs:10
    ~random_src:(Gen.make_random_seed seed)
    (Gen.make_int 0 10)
    fact
    [(fun x -> x >= 0) ==> even]

let () =
  Test.add_random_test
    ~title:"sum of odds"
    ~nb_runs:200
    ~random_src:(Gen.make_random_seed seed)
    (Gen.zip2 Gen.int Gen.int)
    (Gen.apply2 (+))
    [(Spec.zip2 odd odd) ==> even]

let () =
  Test.add_random_test
    ~title:"strings"
    ~nb_runs:2
    ~classifier:size
    ~random_src:(Gen.make_random_seed seed)
    (Gen.word (Gen.make_int 2 16))
    (fun x -> x)
    [(fun _ -> true) ==> (fun x -> x = "")]

let () =
  Test.add_random_test
    ~title:"lists"
    ~nb_runs:2
    ~random_src:(Gen.make_random_seed seed)
    (Gen.list (Gen.make_int 2 8) (Gen.make_int 1 7))
    (fun x -> x)
    [Spec.always ==> (fun x -> x = [])]

let outputs = [
  Test.Text_output (open_out "result.txt") ;
  Test.Html_output (open_out "result.html") ;
  Test.Xml_output (open_out "result.xml") ;
  Test.Xml_junit_output (open_out "resultjunit.xml") ;
  Test.Csv_output ((open_out "result.csv"), "|")
]

let () =
  List.iter
    (fun o -> Test.launch_tests ~output:o ~clear:false ())
    outputs

let () =
  let extensions = [ ".csv"; ".html"; ".txt"; ".xml"; "junit.xml" ] in
  let diffs =
    List.map
      (fun e -> Shell.diff ~options:["-q"] ("reference" ^ e) ("result" ^ e))
      extensions in
  if Shell.run_list diffs <> 0 then begin
    print_endline "failed";
    exit 1
  end else begin
    let rm = Shell.rm ~options:["-f"] (List.map (fun e -> "result" ^ e) extensions) in
    ignore (Shell.run rm);
    exit 0
  end
