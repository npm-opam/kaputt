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


(* Type definitions *)

type result =
  | Passed
  | Failed of Assertion.failure
  | Uncaught of exn * string
  | Report of int * int * int * (string list) * ((string * int) list)
  | Exit_code of int

type 'a classifier = 'a -> string

type t = string * (unit -> result)

type output_mode =
  | Text_output of out_channel
  | Html_output of out_channel
  | Xml_output of out_channel
  | Xml_junit_output of out_channel
  | Csv_output of out_channel * string


(* Tests to be run *)

let tests = ref []

let add_test x =
  tests := x :: !tests


(* Generation of titles *)

let counter = ref 0

let get_title () =
  incr counter;
  "untitled no " ^ (string_of_int !counter)


(* Assertion-based tests *)

let return x = fun () -> x

let make_assert_test ?(title=get_title ()) set_up f tear_down =
  title,
  (fun () ->
    try
      tear_down (f (set_up ()));
      Passed
    with
    | Assertion.Failed f -> Failed f
    | e -> Uncaught (e, Printexc.get_backtrace ()))

let return_unit = return ()

let make_simple_test ?(title=get_title ()) f =
  make_assert_test ~title:title return_unit f ignore

let add_assert_test ?(title=get_title ()) set_up f tear_down =
  add_test (make_assert_test ~title:title set_up f tear_down)

let add_simple_test ?(title=get_title ()) f =
  add_test (make_simple_test ~title:title f)


(* Generator-based tests *)

let default_classifier _ = ""

let default_reducer _ = []

let default_smaller _ _ = true

let dummy_spec =
  let dummy_pre _ = false in
  let dummy_post _ = false in
  let open Specification in
  { precond = dummy_pre; postcond = dummy_post; }

let rec extract x = function
  | hd :: tl ->
      if hd.Specification.precond x then
        hd
      else
        extract x tl
  | [] -> dummy_spec

let rec reduce_candidates n red spec x f =
  let candidates = red x in
  let candidates =
    List.filter
      (fun c ->
        (spec.Specification.precond c)
          && not (spec.Specification.postcond (c, f c)))
      candidates in
  if n > 0 then
    List.flatten
      (List.map
         (fun c -> reduce_candidates (pred n) red spec c f)
         candidates)
  else
    candidates
  
let reduce smaller n red spec x f =
  if (n > 0) && (red != default_reducer) then begin
    match reduce_candidates n red spec x f with
    | hd :: tl ->
        let min =
          List.fold_left
            (fun acc elem ->
              if smaller elem acc then
                elem
              else
                acc)
            hd
            tl in
        Some min
    | [] -> None
  end else
    None

let make_random_test_with_wrapper
    ~title
    ~nb_runs
    ~nb_tries
    ~classifier
    ~reducer
    ~reduce_depth
    ~reduce_smaller
    ~random_src
    (gen, prn) f spec
    wrap =
  if nb_runs <= 0 then invalid_arg "Kaputt.Test.make_random_test";
  if nb_tries <= 0 then invalid_arg "Kaputt.Test.make_random_test";
  if reduce_depth < 0 then invalid_arg "Kaputt.Test.make_random_test";
  title,
  (fun () ->
    let valid = ref 0 in
    let uncaught = ref 0 in
    let actual_runs = ref 0 in
    let counterexamples = ref [] in
    let categories = Hashtbl.create 16 in
    for _i = 1 to nb_runs do
      let x = ref (gen random_src) in
      let pre_post = ref (extract !x spec) in
      let tries = ref nb_tries in
      while (!pre_post == dummy_spec) && (!tries > 0) do
        let tmp = gen random_src in
        x := tmp;
        pre_post := extract tmp spec;
        decr tries
      done;
      if !pre_post != dummy_spec then begin
        incr actual_runs;
        try
          let y = wrap f !x in
          let cat = classifier !x in
          let curr = try Hashtbl.find categories cat with _ -> 0 in
          Hashtbl.replace categories cat (succ curr);
          if (!pre_post).Specification.postcond (!x, y) then
            incr valid
          else
            let x' = prn !x in
            let reduced = reduce reduce_smaller reduce_depth reducer !pre_post !x (wrap f) in
            let x' = match reduced with
            | Some r -> x' ^ " reduced to " ^ (prn r)
            | None -> x' in
            if not (List.mem x' !counterexamples) then
              counterexamples := x' :: !counterexamples
        with _ -> incr uncaught
      end
    done;
    let categories' = Hashtbl.fold (fun k v acc -> (k, v) :: acc) categories [] in
    Report (!valid, !actual_runs, !uncaught, (List.rev !counterexamples), categories'))

let no_wrap f x =
  f x

let wrap_partial f x =
  try
    Specification.Result (f x)
  with e -> Specification.Exception e

let make_random_test
    ?(title=get_title ())
    ?(nb_runs=100)
    ?(nb_tries=10*nb_runs)
    ?(classifier=default_classifier)
    ?(reducer=default_reducer)
    ?(reduce_depth=4)
    ?(reduce_smaller=default_smaller)
    ?(random_src=Generator.make_random ())
    (gen, prn) f spec =
  make_random_test_with_wrapper
    ~title:title
    ~nb_runs:nb_runs
    ~nb_tries:nb_tries
    ~classifier:classifier
    ~reducer:reducer
    ~reduce_depth:reduce_depth
    ~reduce_smaller:reduce_smaller
    ~random_src:random_src
    (gen, prn) f spec
    no_wrap

let add_random_test
    ?(title=get_title ())
    ?(nb_runs=100)
    ?(nb_tries=10*nb_runs)
    ?(classifier=default_classifier)
    ?(reducer=default_reducer)
    ?(reduce_depth=4)
    ?(reduce_smaller=default_smaller)
    ?(random_src=Generator.make_random ())
    (gen, prn) f spec =
  add_test (make_random_test
              ~title:title
              ~nb_runs:nb_runs
              ~nb_tries:nb_tries
              ~classifier:classifier
              ~reducer:reducer
              ~reduce_depth:reduce_depth
              ~reduce_smaller:reduce_smaller
              ~random_src:random_src
              (gen, prn) f spec)


(* Generator-based tests (partial functions) *)

let make_partial_random_test
    ?(title=get_title ())
    ?(nb_runs=100)
    ?(nb_tries=10*nb_runs)
    ?(classifier=default_classifier)
    ?(reducer=default_reducer)
    ?(reduce_depth=4)
    ?(reduce_smaller=default_smaller)
    ?(random_src=Generator.make_random ())
    (gen, prn) f spec =
  make_random_test_with_wrapper
    ~title:title
    ~nb_runs:nb_runs
    ~nb_tries:nb_tries
    ~classifier:classifier
    ~reducer:reducer
    ~reduce_depth:reduce_depth
    ~reduce_smaller:reduce_smaller
    ~random_src:random_src
    (gen, prn) f spec
    wrap_partial

let add_partial_random_test
    ?(title=get_title ())
    ?(nb_runs=100)
    ?(nb_tries=10*nb_runs)
    ?(classifier=default_classifier)
    ?(reducer=default_reducer)
    ?(reduce_depth=4)
    ?(reduce_smaller=default_smaller)
    ?(random_src=Generator.make_random ())
    (gen, prn) f spec =
  add_test (make_partial_random_test
              ~title:title
              ~nb_runs:nb_runs
              ~nb_tries:nb_tries
              ~classifier:classifier
              ~reducer:reducer
              ~reduce_depth:reduce_depth
              ~reduce_smaller:reduce_smaller
              ~random_src:random_src
              (gen, prn) f spec)


(* Enumerator-based tests *)

let dummy_post _ = false

let make_enum_test_with_wrapper ~title enum f spec wrap =
  title,
  (fun () ->
    let valid = ref 0 in
    let nb = ref 0 in
    let uncaught = ref 0 in
    let counterexamples = ref [] in
    let prn = snd enum in
    Enumerator.iter
      (fun x ->
        let post = (extract x spec).Specification.postcond in
        if post == dummy_post then begin
          let x' = prn x in
          if not (List.mem x' !counterexamples) then
            counterexamples := x' :: !counterexamples
        end else begin
          try
            let y = wrap f x in
            if post (x, y) then
              incr valid
            else
              let x' = prn x in
              if not (List.mem x' !counterexamples) then
                counterexamples := x' :: !counterexamples
          with _ -> incr uncaught
        end;
        incr nb)
      enum;
    Report (!valid, !nb, !uncaught, (List.rev !counterexamples), []))

let make_enum_test ?(title=get_title ()) enum f spec =
  make_enum_test_with_wrapper ~title enum f spec no_wrap

let add_enum_test ?(title=get_title ()) enum f spec =
  add_test (make_enum_test ~title:title enum f spec)


(* Enumerator-based tests (partial functions) *)

let make_partial_enum_test ?(title=get_title ()) enum f spec =
  make_enum_test_with_wrapper ~title enum f spec wrap_partial

let add_partial_enum_test ?(title=get_title ()) enum f spec =
  add_test (make_partial_enum_test ~title:title enum f spec)


(* Shell-based tests *)

let make_shell_test ?(title=get_title ()) l =
  title,
  (fun () ->
    Exit_code (try Shell.run_list l with _ -> min_int))

let add_shell_test ?(title=get_title ()) l =
  add_test (make_shell_test ~title:title l)


(* Test runners *)

let exec_test (_, func) =
  func ()

let exec_tests = List.map exec_test

let escape s =
  let buff = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string buff "&lt;"
      | '>' -> Buffer.add_string buff "&gt;"
      | '\"' -> Buffer.add_string buff "&quot;"
      | '&' -> Buffer.add_string buff "&amp;"
      | '\t' -> for _i = 1 to 4 do Buffer.add_string buff "&nbsp;" done
      | ch -> Buffer.add_char buff ch)
    s;
  Buffer.contents buff

let safe_close out =
  if (out != stdout) && (out != stderr) then close_out_noerr out

let make_output kind =
  match kind with
  | Text_output out ->
      object
        method header _ _ _ _ = ()
        method footer = ()
        method result name res =
          match res with
          | Passed ->
              Printf.fprintf out "Test '%s' ... passed\n" name
          | Failed { Assertion.expected_value ; actual_value ; message = "" } ->
              if expected_value <> actual_value then
                Printf.fprintf out "Test '%s' ... failed\n  expected `%s` but received `%s`\n"
                  name
                  expected_value
                  actual_value
              else
                Printf.fprintf out "Test '%s' ... failed\n  expected anything excluding `%s` but received `%s`\n"
                  name
                  expected_value
                  actual_value
          | Failed { Assertion.expected_value ; actual_value ; message } ->
              if expected_value <> actual_value then
                Printf.fprintf out "Test '%s' ... failed\n  %s (expected `%s` but received `%s`)\n"
                  name
                  message
                  expected_value
                  actual_value
              else
                Printf.fprintf out "Test '%s' ... failed\n  %s (expected anything excluding `%s` but received `%s`)\n"
                  name
                  message
                  expected_value
                  actual_value
          | Uncaught (e, bt) ->
              Printf.fprintf out "Test '%s' ... raised an exception\n  %s\n%s\n" name (Printexc.to_string e) bt
          | Report (valid, total, uncaught, counterexamples, categories) ->
              Printf.fprintf out "Test '%s' ... %d/%d case%s passed%s\n"
                name
                valid
                total
                (if valid > 1 then "s" else "")
                (match uncaught with
                | 0 -> ""
                | 1 -> " (1 uncaught exception)"
                | n -> " (" ^ (string_of_int n) ^ " uncaught exceptions)");
              if counterexamples <> [] then
                Printf.fprintf out "  counterexample%s: %s\n"
                  (if (List.length counterexamples) > 1 then "s" else "")
                  (String.concat ", " counterexamples);
              if (List.length categories) > 1 then begin
                Printf.fprintf out "  categories:\n";
                List.iter
                  (fun (c, n) ->
                    Printf.fprintf out "    %s -> %d occurrence%s\n" c n (if n > 1 then "s" else ""))
                  categories
              end
          | Exit_code c ->
              Printf.fprintf out "Test '%s' ... returned code %d\n" name c
        method close = safe_close out
      end
  | Html_output out ->
      let output_lines =
        List.iter
          (fun x ->
            output_string out x;
            output_char out '\n') in
      object
        method header _ _ _ _ =
          output_lines
            [ "<html>";
              "<head>";
              "<title>Kaputt report</title>";
              "<style type=\"text/css\">";
              "table.sample {";
              "  border-width: 1px;";
              "  border-style: solid;";
              "  border-color: black;";
              "  border-collapse: collapse;";
              "  background-color: white;";
              "}";
              "table.sample th {";
              "  border-width: 1px;";
              "  padding: 3px;";
              "  border-style: solid;";
              "  border-color: black;";
              "  background-color: white;";
              "}";
              "table.sample td {";
              "  border-width: 1px;";
              "  padding: 3px;";
              "  border-style: solid;";
              "  border-color: black;";
              "  background-color: white;";
              "}";
              "</style>";
              "</head>";
              "<body>";
              "<p align=\"center\">";
              "<table class=\"sample\" width=\"85%\">";
              ("<tr>" ^
               "<th width=\"25%\">Test kind</th>" ^
               "<th width=\"25%\">Test name</th>" ^
               "<th>Test outcome</th>" ^
               "</tr>") ]
        method footer =
          output_lines
            [  "</table>";
               "<p style=\"font-size: smaller; text-align: center;\">Generated by <a href=\"http://kaputt.x9c.fr\">Kaputt</a> " ^ Version.value ^ "</p>";
              "</body>";
              "</html>" ]
        method result name res =
          let output_strings = List.iter (output_string out) in
          match res with
          | Passed ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Assertion-based</td>";
                  "<td align=\"center\">"; name; "</td>";
                  "<td>passed</td>";
                  "</tr>\n" ]
          | Failed { Assertion.expected_value ; actual_value ; message } ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Assertion-based</td>";
                  "<td align=\"center\">"; name; "</td>" ];
              flush out;
              Printf.fprintf out "<td>failed - %s '%s' but received '%s'%s"
                (if expected_value <> actual_value then "expected" else "expected anything excluding")
                (escape expected_value)
                (escape actual_value)
                (if message = "" then "" else ("<br/>" ^ (escape message)));
              flush out;
              output_strings [ "</td></tr>\n" ]
          | Uncaught (e, bt) ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Assertion-based</td>";
                  "<td align=\"center\">"; name; "</td>" ];
              flush out;
              Printf.fprintf out "<td>raised an exception: %s%s"
                (escape (Printexc.to_string e))
                (if bt = "" then "" else ("<br/>" ^ (escape bt)));
              flush out;
              output_strings [ "</td></tr>\n" ]
          | Report (valid, total, uncaught, counterexamples, categories) ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Random-based</td>";
                  "<td align=\"center\">"; name; "</td>";
                  "<td>" ];
              flush out;
              Printf.fprintf out "%d/%d case%s passed%s\n"
                valid
                total
                (if valid > 1 then "s" else "")
                (match uncaught with
                | 0 -> ""
                | 1 -> " (1 uncaught exception)"
                | n -> " (" ^ (string_of_int n) ^ " uncaught exceptions)");
              if counterexamples <> [] then
                Printf.fprintf out "<br/>&nbsp;&nbsp;counterexample%s: %s\n"
                  (if (List.length counterexamples) > 1 then "s" else "")
                  (String.concat ", " counterexamples);
              if (List.length categories) > 1 then begin
                output_strings ["<br/>&nbsp;&nbsp;categories:"];
                flush out;
                List.iter
                  (fun (c, n) ->
                    Printf.fprintf out "<br/>&nbsp;&nbsp;&nbsp;&nbsp;%s -> %d occurrence%s\n" c n (if n > 1 then "s" else ""))
                  categories
              end;
              flush out;
              output_strings [ "</td></tr>\n" ]
          | Exit_code c ->
              output_strings
                [ "<tr style=\"font-family: monospace;\">";
                  "<td align=\"center\">Shell-based</td>";
                  "<td align=\"center\">"; name; "</td>";
                  "<td>return code: " ^ (string_of_int c) ^ "</td>";
                  "</tr>\n" ]
        method close = safe_close out
      end
  | Xml_output out ->
      object
        method header _ _ _ _ =
          output_string out "<kaputt-report>\n"
        method footer =
          output_string out "</kaputt-report>\n"
        method result name res =
          match res with
          | Passed ->
              Printf.fprintf out "  <passed-test name=\"%s\"/>\n" (escape name)
          | Failed { Assertion.expected_value ; actual_value ; message = "" } ->
              Printf.fprintf out "  <failed-test name=\"%s\" %s=\"%s\" actual=\"%s\"/>\n"
                (escape name)
                (if expected_value <> actual_value then "expected" else "not-expected")
                (escape expected_value)
                (escape actual_value)
          | Failed { Assertion.expected_value ; actual_value ; message } ->
              Printf.fprintf out "  <failed-test name=\"%s\" %s=\"%s\" actual=\"%s\" message=\"%s\"/>\n"
                (escape name)
                (if expected_value <> actual_value then "expected" else "not-expected")
                (escape expected_value)
                (escape actual_value)
                (escape message)
          | Uncaught (e, bt) ->
              Printf.fprintf out "  <uncaught-exception name=\"%s\" exception=\"%s\">\n"
                (escape name)
                (escape (Printexc.to_string e));
              output_string out (escape bt);
              output_string out "  </uncaught-exception>"
          | Report (valid, total, uncaught, counterexamples, categories) ->
              let tag = if categories = [] then "enum-test" else "random-test" in
              Printf.fprintf out "  <%s name=\"%s\" valid=\"%d\" total=\"%d\" uncaught=\"%d\">\n"
                tag
                (escape name)
                valid
                total
                uncaught;
              if counterexamples <> [] then begin
                output_string out "    <counterexamples>\n";
                List.iter
                  (fun x -> Printf.fprintf out "      <counterexample value=\"%s\"/>\n" (escape x))
                  counterexamples;
                output_string out "    </counterexamples>\n"
              end;
              if (List.length categories) > 1 then begin
                output_string out "    <categories>\n";
                List.iter
                  (fun (c, n) -> Printf.fprintf out "      <category name=\"%s\" total=\"%d\"/>\n" (escape c) n)
                  categories;
                output_string out "    </categories>\n"
              end;
              Printf.fprintf out "  </%s>\n" tag;
          | Exit_code c ->
              Printf.fprintf out "  <shell-test name=\"%s\" exit-code=\"%d\"/>\n" (escape name) c
        method close = safe_close out
      end
  | Xml_junit_output out ->
      object
        method header _ failed uncaught total =
          Printf.fprintf
            out
            "<testsuite name=\"Kaputt Report\" tests=\"%d\" errors=\"%d\" failures=\"%d\">\n"
            total
            failed
            uncaught;
          output_string out "  <properties>\n";
          Printf.fprintf out "    <property name=\"KAPUTT_VERSION\" version=\"%s\"/>\n" Version.value;
          output_string out "  </properties>\n"
        method footer =
          output_string out "</testsuite>\n"
        method result name res =
          Printf.fprintf out "  <testcase name=\"%s\"" (escape name);
          (match res with
          | Passed ->
              output_string out "/>\n"
          | Failed { Assertion.expected_value ; actual_value ; message } ->
              output_string out ">\n";
              Printf.fprintf
                out
                "    <failure type=\"%s '%s' but received '%s'\" message=\"%s\"/>\n"
                (if expected_value <> actual_value then "expected" else "expected anything excluding")
                (escape expected_value)
                (escape actual_value)
                (escape message);
              output_string out "  </testcase>\n"
          | Uncaught (e, bt) ->
              output_string out ">\n";
              Printf.fprintf
                out
                "    <failure type=\"uncaught exception\" message=\"%s\">\n"
                (escape (Printexc.to_string e));
              output_string out bt;
              output_string out "    </failure>\n";
              output_string out "  </testcase>\n"
          | Report (valid, total, uncaught, counterexamples, _) ->
              if valid = total then
                output_string out "/>\n"
              else begin
                output_string out ">\n";
                Printf.fprintf
                  out
                  "    <failure type=\"random test\" message=\"%d counterexample%s found, %d uncaught exception%s\">\n"
                  (List.length counterexamples)
                  (if (List.length counterexamples) > 1 then "s" else "")
                  uncaught
                  (if uncaught > 1 then "s" else "");
                List.iter
                  (Printf.fprintf out "      %s\n")
                  counterexamples;
                output_string out "    </failure>\n";
                output_string out "  </testcase>\n"
              end
          | Exit_code c ->
              output_string out ">\n";
              Printf.fprintf
                out
                "    <failure type=\"exit code\" message=\"%d\"/>\n"
                c;
              output_string out "  </testcase>\n")
        method close = safe_close out
      end
  | Csv_output (out, sep) ->
      object
        method header _ _ _ _ = ()
        method footer = ()
        method result name res =
          let output_strings = List.iter (output_string out) in
          match res with
          | Passed ->
              output_strings ["passed-test"; sep; name; "\n"]
          | Failed { Assertion.expected_value ; actual_value ; message } ->
              output_strings [ "failed-test"; sep;
                               name; sep;
                               expected_value; sep;
                               actual_value; "\n" ];
              if message <> "" then  output_strings [sep; message]
          | Uncaught (e, _) ->
              output_strings ["uncaught-exception"; sep; name; sep; (Printexc.to_string e); "\n"]
          | Report (valid, total, uncaught, counterexamples, categories) ->
              let tag = if categories = [] then "enum-test" else "random-test" in
              output_strings [ (tag ^ " (stats)"); sep;
                               name; sep;
                               (string_of_int valid); sep;
                               (string_of_int total); sep;
                               (string_of_int uncaught); "\n" ];
              if counterexamples <> [] then
                output_strings [ (tag ^ " (counterexamples)"); sep;
                                 name; sep;
                                 (String.concat sep counterexamples); "\n" ];
              if (List.length categories) > 1 then begin
                output_strings [ (tag ^ " (categories)"); sep; name];
                List.iter
                  (fun (c, n) ->
                    output_strings [sep; c; (string_of_int n)])
                  categories;
                output_string out "\n"
              end
          | Exit_code c ->
              output_strings ["shell-test"; sep; name; sep; (string_of_int c); "\n"]
        method close = safe_close out
      end

let run_tests ?(output=(Text_output stdout)) l =
  let out = make_output output in
  let passed = ref 0 in
  let failed = ref 0 in
  let uncaught = ref 0 in
  let total = ref 0 in
  let l' =
    List.map
      (fun (n, f) ->
        let res = f () in
        (match res with
        | Passed ->
            incr passed;
            incr total
        | Failed _ ->
            incr failed;
            incr total
        | Uncaught _ ->
            incr uncaught;
            incr total
        | Report (pass, tot, unc, _, _) ->
            passed := !passed + pass;
            failed := !failed + (tot - pass -unc);
            uncaught := !uncaught + unc;
            total := !total + tot
        | Exit_code c ->
            incr (if c = 0 then passed else failed);
            incr total);
        (n, res))
      l in
  out#header !passed !failed !uncaught !total;
  List.iter (fun (n, r) -> out#result n (r)) l';
  out#footer;
  out#close

let run_test ?(output=(Text_output stdout)) x =
  run_tests ~output:output [x]

let launch_tests ?(output=(Text_output stdout)) ?(clear=true) () =
  run_tests ~output:output (List.rev !tests);
  if clear then tests := []

let check
    ?(title=get_title ())
    ?(nb_runs=100)
    ?(nb_tries=10*nb_runs)
    ?(classifier=default_classifier)
    ?(random_src=Generator.make_random ())
    generator f spec =
  run_test
    (make_random_test
       ~title:title
       ~nb_runs:nb_runs
       ~nb_tries:nb_tries
       ~classifier:classifier
       ~random_src:random_src
       generator
       f
       spec)

let check_partial
    ?(title=get_title ())
    ?(nb_runs=100)
    ?(nb_tries=10*nb_runs)
    ?(classifier=default_classifier)
    ?(random_src=Generator.make_random ())
    generator f spec =
  run_test
    (make_partial_random_test
       ~title:title
       ~nb_runs:nb_runs
       ~nb_tries:nb_tries
       ~classifier:classifier
       ~random_src:random_src
       generator
       f
       spec)
