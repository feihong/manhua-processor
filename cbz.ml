(* Convert SQLite database file to a bunch of .cbz files *)
open Printf

type episode = { id : int; title : string; short_title : string }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type comic = { id : int; title : string; ep_list : episode list }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type image = { path : string } [@@deriving yojson] [@@yojson.allow_extra_fields]

type imageIndex = { path : string; images : image list }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type cbz = { title : string; link : string; images : string list }

let query_to_json_seq db query =
  let statement = Sqlite3.prepare db query in
  Seq.unfold
    (fun () ->
      match Sqlite3.step statement with
      | ROW -> (
          match Sqlite3.column_blob statement 0 |> Yojson.Safe.from_string with
          | `Assoc lst -> Some (List.assoc "data" lst, ())
          | _ -> None)
      | DONE -> None
      | code ->
          printf {|Unexpected return code for query "%s": %s\n|} query
            (code |> Sqlite3.Rc.to_string);
          None)
    ()

let _dump_json json =
  let oc = open_out "temp.json" in
  output_string oc (json |> Yojson.Safe.to_string);
  close_out oc

let make_comics_table (db : Sqlite3.db) =
  query_to_json_seq db "select data from dump where url like '%ComicDetail%'"
  |> Seq.map (fun json ->
         let comic = comic_of_yojson json in
         printf "Comic: %s\n" comic.title;
         (comic.id, comic))
  |> Hashtbl.of_seq

let make_cbz ~(comics_table : (int, comic) Hashtbl.t) (imageIndex : imageIndex)
    =
  let re = Str.regexp {|/bfs/manga/\([0-9]+\)/\([0-9]+\)|} in
  let path = imageIndex.path in
  (* If path doesn't match, then it probably hasn't been bought *)
  match Str.string_match re path 0 with
  | false ->
      printf "The path %s did not match regexp" path;
      None
  | true ->
      let comic_id = Str.matched_group 1 path |> int_of_string in
      let episode_id = Str.matched_group 2 path |> int_of_string in
      let comic = Hashtbl.find comics_table comic_id in
      let episode : episode =
        comic.ep_list |> List.find (fun (ep : episode) -> ep.id = episode_id)
      in
      Some
        {
          link =
            sprintf "https://manga.bilibili.com/mc%i/%i" comic_id episode_id;
          title =
            [ comic.title; episode.short_title; episode.title ]
            |> List.filter_map (fun s ->
                   match String.trim s with "" -> None | s -> Some s)
            |> String.concat " ";
          images = imageIndex.images |> List.map (fun (im : image) -> im.path);
        }

let make_imageIndex_table (db : Sqlite3.db) =
  query_to_json_seq db "select data from dump where url like '%GetImageIndex%'"
  |> Seq.map (fun json ->
         let imageIndex = imageIndex_of_yojson json in
         print_endline ("Image index: " ^ imageIndex.path);
         (imageIndex.path, imageIndex))
  |> Hashtbl.of_seq

let get_episode_images db (cbz : cbz) =
  let index = ref 0 in
  cbz.images |> List.to_seq
  |> Seq.filter_map @@ fun path ->
     incr index;
     let sql =
       sprintf "select data from dump where url like '%%%s%%' LIMIT 1" path
     in
     let statement = Sqlite3.prepare db sql in
     match Sqlite3.step statement with
     | ROW -> Some (Sqlite3.column_blob statement 0)
     | code ->
         printf "No data found for image %i of %s, code: %s\n" !index cbz.title
           (code |> Sqlite3.Rc.to_string);
         None

let make_cbz_file db (cbz : cbz) =
  let image_count = ref 0 in
  let comment = sprintf "%s\n%s" cbz.title cbz.link in
  let expected_count = List.length cbz.images in
  let init_filename = sprintf "%s (%i).cbz" cbz.title expected_count in
  let zf = Zip.open_out ~comment init_filename in
  get_episode_images db cbz
  |> Seq.iter (fun image ->
         incr image_count;
         let image_filename = sprintf "%03i.jpg" !image_count in
         (* printf "Image size: %i\n" (String.length image); *)
         Zip.add_entry image zf ~level:0 image_filename);
  Zip.close_out zf;

  let final_filename =
    if !image_count <> expected_count then (
      printf "Found %i images for %s, but expected %i\n" !image_count cbz.title
        expected_count;
      let final_filename = sprintf "%s (%i).cbz" cbz.title !image_count in
      (* Rename file to say how many actual images are inside it *)
      Sys.rename init_filename final_filename;
      final_filename)
    else init_filename
  in
  print_endline ("Generated " ^ final_filename)

let () =
  let db = Sqlite3.db_open Shared.db_file in
  let comics_table = make_comics_table db in
  let imageIndexes = make_imageIndex_table db |> Hashtbl.to_seq_values in
  let cbzs = imageIndexes |> Seq.filter_map (make_cbz ~comics_table) in
  cbzs |> Seq.iter (make_cbz_file db);
  let _success = Sqlite3.db_close db in
  ()
