open Yojson;;

let read_json filename =
  let json = Yojson.Basic.from_file filename in

  let open Yojson.Basic.Util in
  
  json |> member "data" |> to_list |> List.length 
