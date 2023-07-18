open! Base

let tap it ~f = IterLabels.map it ~f:(fun x -> f x ; x)

let tapi it ~f = IterLabels.mapi it ~f:(fun i x -> f i x ; x)

let to_map_exn it ~m =
  IterLabels.fold it ~init:(Map.empty m) ~f:(fun map (key, data) ->
      Map.add_exn map ~key ~data )

let of_map map =
  IterLabels.from_iter (fun k ->
      Map.iteri map ~f:(fun ~key ~data -> k (key, data)) )

let to_set it ~m =
  IterLabels.fold it ~init:(Set.empty m) ~f:(fun s el -> Set.add s el)

let of_set s = IterLabels.from_iter (fun k -> Set.iter s ~f:(fun el -> k el))

let filter_opt it = IterLabels.filter_map it ~f:Fn.id

let split it ~on = IterLabels.map it ~f:(String.split ~on)

let split' it ~on =
  IterLabels.map it ~f:(fun s -> String.split s ~on |> Array.of_list)

let split_strip it ~on =
  IterLabels.map it ~f:(fun s ->
      String.split s ~on |> List.map ~f:String.strip )

let split_strip' it ~on =
  IterLabels.map it ~f:(fun s ->
      s |> String.split ~on |> Array.of_list_map ~f:String.strip )
