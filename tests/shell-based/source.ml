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

let _ =
  let cmd =
    Shell.redirect_output
      (Shell.pipe
         (Shell.cat ["data"])
         (Shell.sed "s/line/LINE/g"))
      "data2" in
  Shell.run cmd

let () =
  let lines = Shell.read_lines "data2" in
  let lines = List.tl lines in
  Shell.write_lines lines "data3"

let _ =
  ignore (Shell.run (Shell.touch ["result"]));
  Shell.run_list
    (List.map
       (fun s -> Shell.redirect_append (Shell.cat [s]) "result")
       [ "data2"; "data3" ])

let _ =
  let (!!) = Shell.coerce in
  Shell.run_list
    [Shell.sleep 1;
     !! (Shell.touch ["."]);
     Shell.exit 0]
    

let () =
  let diff = Shell.run (Shell.diff ~options:["-q"] "reference" "result") in
  if diff <> 0 then begin
    print_endline "failed";
    exit 1
  end else begin
    ignore (Shell.run (Shell.rm ~options:["-f"] ["data2"; "data3"; "result"]));
    exit 0
  end
