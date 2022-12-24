(* Generate database from session export *)

let file_to_seq filename =
  let ic = open_in filename in
  ic |> Seq.unfold (fun ic ->
      try Some (input_line ic, ic)
      with End_of_file ->
        close_in ic;
        None) 

let () = print_endline "Inside incomplete db program"
