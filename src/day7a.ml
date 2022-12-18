type file = File of int | Directory of dir
and dir = Node of dir option * (string, file) Hashtbl.t

let peek_line : string option ref = ref None

let read_line_opt () =
  match !peek_line with
  | Some line ->
      let rv = line in
      peek_line := None;
      Some rv
  | None -> ( try Some (read_line ()) with End_of_file -> None)

let explode s = List.init (String.length s) (String.get s)
let root = Node (None, Hashtbl.create 67)

let rec ls node =
  let (Node (_, files)) = node in
  match read_line_opt () with
  | None -> ()
  | Some l when l.[0] == '$' -> peek_line := Some l
  | Some l ->
      (if String.starts_with l ~prefix:"dir" then
       let dir = String.sub l 4 (String.length l - 4) in
       Hashtbl.add files dir (Directory (Node (Some node, Hashtbl.create 67)))
      else
        let size, file =
          match String.split_on_char ' ' l with
          | [ size; file ] -> (size, file)
          | _ -> failwith ""
        in
        let size = int_of_string size in
        Hashtbl.add files file (File size));
      ls node

let cd node dir =
  let (Node (parent, files)) = node in
  match dir with
  | "/" -> root
  | ".." -> (
      match parent with Some parent -> parent | _ -> failwith "Bad dir")
  | _ -> (
      match Hashtbl.find_opt files dir with
      | Some (Directory node) -> node
      | Some _ -> failwith "Can't cd into file"
      | None ->
          let new_dir = Node (Some node, Hashtbl.create 67) in
          Hashtbl.add files dir (Directory new_dir);
          new_dir)

let parse () =
  let rec next node =
    match read_line_opt () with
    | Some line ->
        if String.starts_with line ~prefix:"$ ls" then (
          ls node;
          next node)
        else if String.starts_with line ~prefix:"$ cd " then
          next (cd node (String.sub line 5 (String.length line - 5)))
        else failwith (String.concat "" [ "Invalid line"; line ])
    | None -> ()
  in
  next root

let amount = ref 0

let rec count (Node (_, files)) =
  let f _ file size =
    match file with
    | File size' -> size + size'
    | Directory node -> size + count node
  in
  let size = Hashtbl.fold f files 0 in
  if size <= 100000 then (
    amount := !amount + size;
    size)
  else size

let () =
  parse ();
  let _ = count root in
  print_int !amount
