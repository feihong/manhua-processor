(* Convert SQLite database file to a bunch of .cbz files *)

type input_episode = { path : string; title : string; short_title : string }
[@@deriving yojson]

type comic = { id : string; title : string; ep_list : input_episode list }
[@@deriving yojson]

type episode = { id : string; title : string; link : string }

let get_data (json : Yojson.Safe.t) =
  match json with `Assoc lst -> List.assoc_opt "data" lst | _ -> None

let get_comics_table (db : Sqlite3.db) =
  let statement =
    Sqlite3.prepare db "select data from dump where url like '%ComicDetail%'"
  in
  let table = Hashtbl.create 10 in
  let _code, () =
    Sqlite3.fold statement
      ~f:(fun () array ->
        match array with
        | [| BLOB data |] ->
            data |> Yojson.Safe.from_string |> get_data
            |> Option.map comic_of_yojson
            |> Option.iter (fun (comic : comic) ->
                   Hashtbl.add table comic.id comic)
        | _ -> ())
      ~init:()
  in
  table

let () =
  print_endline "Inside incomplete cbz program";
  let db = Sqlite3.db_open Shared.db_file in
  get_comics_table db |> Hashtbl.to_seq_values
  |> Seq.iter (fun (comic : comic) -> print_endline comic.title);
  let _success = Sqlite3.db_close db in
  ()
