(* Convert .jsonl file to SQLite database file *)

let jsonl_file = "session.jsonl"
let db_file = "session.db"

type request = { url : string; encoding : string; data : string }
[@@deriving yojson]

(* Return a Seq of lines from the jsonl file *)
let file_to_lines () =
  let ic = open_in jsonl_file in
  ic
  |> Seq.unfold (fun ic ->
         try Some (input_line ic, ic)
         with End_of_file ->
           close_in ic;
           None)

(* Parse each line into request record *)
let line_to_request (line : string) =
  let json = Yojson.Safe.from_string line in
  let request = request_of_yojson json in
  {
    request with
    data =
      (if request.encoding = "base64" then Base64.decode_exn request.data
      else request.data);
  }

let () =
  print_endline "Inside incomplete db program";
  if Sys.file_exists db_file then Sys.remove db_file;
  file_to_lines () |> Seq.map line_to_request
  |> Seq.iter (fun request -> print_endline request.url)
