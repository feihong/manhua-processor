(* Convert SQLite database file to a bunch of .cbz files *)

type image = { path : string } [@@deriving yojson]

type input_episode = {
  id : int;
  path : string;
  title : string;
  short_title : string;
  images : image list;
}
[@@deriving yojson]

type comic = { id : int; title : string; ep_list : input_episode list }
[@@deriving yojson]

type episode = { id : int; title : string; link : string; images : string list }

let query_to_json_seq db query =
  let statement = Sqlite3.prepare db query in
  Seq.unfold
    (fun () ->
      match Sqlite3.step statement with
      | OK -> (
          match Sqlite3.column_blob statement 0 |> Yojson.Safe.from_string with
          | `Assoc lst -> Some (List.assoc "data" lst, ())
          | _ -> None)
      | _ -> None)
    ()

let make_comics_table (db : Sqlite3.db) =
  query_to_json_seq db "select data from dump where url like '%ComicDetail%'"
  |> Seq.map (fun json ->
         let comic = comic_of_yojson json in
         (comic.id, comic))
  |> Hashtbl.of_seq

let make_episode ~(comics_table : (int, comic) Hashtbl.t)
    (input_episode : input_episode) =
  let re = Str.regexp {|/bfs/manga/\([0-9]+\)/\([0-9]+\)|} in
  let path = input_episode.path in
  (* If path doesn't match, then it probably hasn't been bought *)
  match Str.string_match re path 0 with
  | false -> None
  | true ->
      let comic_id = Str.matched_group 1 path |> int_of_string in
      let episode_id = Str.matched_group 2 path |> int_of_string in
      let comic = Hashtbl.find comics_table comic_id in
      let episode : input_episode =
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
          images = episode.images |> List.map (fun (im : image) -> im.path);
        }

let make_episodes_table (db : Sqlite3.db)
    (comics_table : (int, comic) Hashtbl.t) =
  query_to_json_seq db "select data from dump where url like '%GetImageIndex%'"
  |> Seq.filter_map (fun json ->
         json |> input_episode_of_yojson |> make_episode ~comics_table
         |> Option.map (fun episode -> (episode.id, episode)))
  |> Hashtbl.of_seq

let get_episode_images db (episode : episode) =
  let index = ref 0 in
  episode.images |> List.to_seq
  |> Seq.filter_map @@ fun path ->
     incr index;
     let sql =
       Printf.sprintf "select data from dump where url like '%s' LIMIT 1" path
     in
     let statement = Sqlite3.prepare db sql in
     match Sqlite3.step statement with
     | OK -> Some (Sqlite3.column_blob statement 0)
     | _ ->
         Printf.printf "No data found for image %i of %s" !index episode.title;
         None

let make_cbz_file db (episode : episode) =
  let image_count = ref 0 in
  let comment = Printf.sprintf "%s\n%s" episode.title episode.link in
  let filename1 = episode.title ^ ".cbz" in
  let zf = Zip.open_out ~comment filename1 in
  get_episode_images db episode
  |> Seq.iter (fun image ->
         incr image_count;
         let filename = Printf.sprintf "%03i.jpg" !image_count in
         Zip.add_entry image zf ~level:0 filename);
  Zip.close_out zf;

  (* Rename file to say how images are inside it *)
  Sys.rename filename1 (Printf.sprintf "%s (%i).cbz" episode.title !image_count);

  let expected_count = List.length episode.images in
  if !image_count <> expected_count then
    Printf.printf "Found %i images for %s, but expected %i" !image_count
      episode.title expected_count

let () =
  print_endline "Inside incomplete cbz program";
  let db = Sqlite3.db_open Shared.db_file in
  make_episodes_table db (make_comics_table db)
  |> Hashtbl.to_seq_values
  |> Seq.iter (make_cbz_file db);
  let _success = Sqlite3.db_close db in
  ()
