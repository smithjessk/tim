open Core

module HostsMap = struct
  let invalid_ip = "127.0.0.1"

  type t = string String.Map.t

  let create_stdin () =
    let map =
      In_channel.(input_lines stdin)
      |> List.fold ~init:String.Map.empty ~f:(fun map line ->
             match String.split line ~on:' ' with
             | ip :: domains ->
                 List.fold domains ~init:map ~f:(fun map domain ->
                     Map.set map ~key:domain ~data:ip )
             | [] ->
                 Printf.eprintf "Invalid line encountered: %s\n" line ;
                 Printf.eprintf "Skipping\n" ;
                 map )
    in
    map

  let block t host = Map.set t ~key:host ~data:invalid_ip

  let unblock t host = Map.remove t host

  let to_string t =
    Map.fold t ~init:[] ~f:(fun ~key:domain ~data:ip l ->
        sprintf "%s %s" ip domain :: l )
    |> String.concat ~sep:"\n"
end

let () =
  Command.run
    (Command.group ~summary:"Block or unblock domains"
       ( [("block", HostsMap.block); ("unblock", HostsMap.unblock)]
       |> List.map ~f:(fun (name, f) ->
              let domain =
                Command.Param.anon
                  (Command.Anons.( %: ) "domain" Command.Arg_type.Export.string)
              in
              let cmd =
                Command.basic
                  ~summary:(sprintf "%s domains" name)
                  (Command.Param.map domain ~f:(fun domain () ->
                       let open HostsMap in
                       (create_stdin () |> f) domain
                       |> to_string |> print_endline ))
              in
              (name, cmd) ) ))
