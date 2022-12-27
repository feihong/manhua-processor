(* Convert .jsonl file to SQLite database file *)

type request = { url : string; encoding : string option; data : string }
[@@deriving yojson]

let jsonl_file = "session.jsonl"

(* Return a Seq of lines from the jsonl file *)
let file_to_lines () =
  let ic = open_in jsonl_file in
  Seq.unfold
    (fun () ->
      try Some (input_line ic, ())
      with End_of_file ->
        close_in ic;
        None)
    ()

(* Parse each line into request record *)
let line_to_request (line : string) =
  let json = Yojson.Safe.from_string line in
  let request = request_of_yojson json in
  {
    request with
    data =
      (if request.encoding = Some "base64" then Base64.decode_exn request.data
      else request.data);
  }

let () =
  let db_file = Shared.db_file in
  if Sys.file_exists db_file then Sys.remove db_file;
  let db = Sqlite3.db_open db_file in
  let _code = Sqlite3.exec db "CREATE TABLE dump (url text, data blob)" in
  file_to_lines () |> Seq.map line_to_request
  |> Seq.iter (fun { url; data; _ } ->
         (* print_endline url; *)
         let statement =
           Sqlite3.prepare db "INSERT INTO dump VALUES (:url, :data)"
         in
         let _code =
           Sqlite3.bind_names statement
             [ (":url", TEXT url); (":data", BLOB data) ]
         in
         match Sqlite3.step statement with
         | DONE -> ()
         | code ->
             Printf.printf "Failed to insert row, code: %s"
               (Sqlite3.Rc.to_string code));
  let _success = Sqlite3.db_close db in
  ()
