(* Convert SQLite database file to a bunch of .cbz files *)

type episode = {
  id : string;
  title : string;
  short_title : string;
  link : string option; [@yojson.option]
}
[@@deriving yojson]

type comic = { id : string; title : string; ep_list : episode list }
[@@deriving yojson]

let get_data (json : Yojson.Safe.t) =
  match json with `Assoc lst -> List.assoc_opt "data" lst | _ -> None

let get_comics (db : Sqlite3.db) =
  let statement =
    Sqlite3.prepare db "select data from dump where url like '%ComicDetail%'"
  in
  Seq.unfold
    (fun () ->
      match Sqlite3.step statement with
      | OK ->
          Sqlite3.column_blob statement 0
          |> Yojson.Safe.from_string |> get_data
          |> Option.map (fun data -> (data |> comic_of_yojson, ()))
      | _ -> None)
    ()

let () =
  print_endline "Inside incomplete cbz program";
  let db = Sqlite3.db_open Shared.db_file in
  get_comics db |> Seq.iter (fun comic -> print_endline comic.title);
  let _success = Sqlite3.db_close db in
  ()
