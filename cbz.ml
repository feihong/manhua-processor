(* Convert SQLite database file to a bunch of .cbz files *)

type input_episode = {
  id : int;
  path : string;
  title : string;
  short_title : string;
}
[@@deriving yojson]

type comic = { id : int; title : string; ep_list : input_episode list }
[@@deriving yojson]

type episode = { id : int; title : string; link : string }

let get_data (s : string) =
  match Yojson.Safe.from_string s with
  | `Assoc lst -> List.assoc_opt "data" lst
  | _ -> None

let make_comics_table (db : Sqlite3.db) =
  let statement =
    Sqlite3.prepare db "select data from dump where url like '%ComicDetail%'"
  in
  let table = Hashtbl.create 10 in
  let _code, () =
    Sqlite3.fold statement
      ~f:(fun () array ->
        match array with
        | [| BLOB data |] ->
            data |> get_data |> Option.map comic_of_yojson
            |> Option.iter (fun (comic : comic) ->
                   Hashtbl.add table comic.id comic)
        | _ -> ())
      ~init:()
  in
  table

let make_episode (comics_table : (int, comic) Hashtbl.t)
    (input_episode : input_episode) =
  let re = Str.regexp {|/bfs/manga/\([0-9]+\)/\([0-9]+\)|} in
  let path = input_episode.path in
  match Str.string_match re path 0 with
  | false -> None
  | true ->
      let comic_id = Str.matched_group 1 path |> int_of_string in
      let episode_id = Str.matched_group 2 path |> int_of_string in
      let comic = Hashtbl.find comics_table comic_id in
      let episode =
        comic.ep_list
        |> List.find (fun (ep : input_episode) -> ep.id = episode_id)
      in
      Some
        {
          id = episode_id;
          link =
            Printf.sprintf "https://manga.bilibili.com/mc%i/%i" comic_id
              episode_id;
          title =
            [ comic.title; episode.short_title; episode.title ]
            |> List.filter_map (fun s ->
                   match String.trim s with "" -> None | s -> Some s)
            |> String.concat " ";
        }

let make_episodes_table (db : Sqlite3.db)
    (comics_table : (int, comic) Hashtbl.t) =
  let table = Hashtbl.create 10 in
  let statement =
    Sqlite3.prepare db "select data from dump where url like '%GetImageIndex%'"
  in
  let string_to_episode s =
    s |> get_data
    |> Option.map input_episode_of_yojson
    |> Option.map (make_episode comics_table)
    |> Option.join
  in
  let _code, () =
    Sqlite3.fold statement
      ~f:(fun () array ->
        match array with
        | [| BLOB s |] ->
            s |> string_to_episode
            |> Option.iter (fun (episode : episode) ->
                   Hashtbl.add table episode.id episode)
        | _ -> ())
      ~init:()
  in
  table

let () =
  print_endline "Inside incomplete cbz program";
  let db = Sqlite3.db_open Shared.db_file in
  let comics_table = make_comics_table db in
  let episodes = make_episodes_table db comics_table |> Hashtbl.to_seq_values in
  episodes |> Seq.iter (fun (ep : episode) -> print_endline ep.title);
  let _success = Sqlite3.db_close db in
  ()
