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

let call args =
  let quote s =
    if (s <> "") && (s.[0] = '-') then
      Filename.quote s
    else
      s in
  let len = Array.length args in
  let buff = Buffer.create 128 in
  for i = 0 to pred len do
    Buffer.add_string buff (quote args.(i));
    Buffer.add_char buff ' ';
  done;
  let code = Sys.command (Buffer.contents buff) in
  ignore (exit code)

let copy from_chan to_chan =
  try
    while true do
      output_string to_chan (input_line from_chan);
      output_char to_chan '\n';
    done
  with _ -> ()

let () =
  let len = Array.length Sys.argv in
  if len > 3 then
    let activated = match Sys.argv.(1) with
    | "on" -> true
    | "off" -> false
    | s ->
        Printf.eprintf "Error: invalid parameter %S (should be either \"on\" or \"off\")\n" s;
        exit 1 in
    let test_file = Sys.argv.(len - 1) ^ "t" in
    let args = Array.sub Sys.argv 2 (len - 2) in
    if activated && (Sys.file_exists test_file) then begin
      let temp_name, temp_chan = Filename.open_temp_file "kaputt" ".ml" in
      let source_chan = open_in args.(len - 3) in
      let test_chan = open_in test_file in
      copy source_chan temp_chan;
      let directive = Printf.sprintf "# 1 %S\n" test_file in
      output_string temp_chan directive;
      copy test_chan temp_chan;
      close_in_noerr source_chan;
      close_in_noerr test_chan;
      close_out_noerr temp_chan;
      args.(len - 3) <- temp_name;
      call args
    end else begin
      call args
    end
  else begin
    Printf.eprintf "Error: invalid command-line\n";
    exit 1
  end
