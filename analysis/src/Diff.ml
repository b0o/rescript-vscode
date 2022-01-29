(* Adapted from https://github.com/ocaml/ocaml-lsp/blob/edf6695f9be3d2b920903e6fd36d06692bb63e55/ocaml-lsp-server/src/diff.ml *)
(* Copyright 2018-2022 the ocaml-lsp contributors, ISC License. *)

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

module Diff = struct
  (* based on *)
  (* https://github.com/paulgb/simplediff/blob/031dc772ca6795cfdfed27384a6b79e772213233/python/simplediff/__init__.py *)

  type item = string
  type t = Deleted of item array | Added of item array | Equal of item array

  let longest_subsequence old_lines new_lines =
    let _, old_index_map =
      Array.fold_left
        (fun (i, m) line ->
          ( i + 1,
            StringMap.update line
              (function None -> Some [i] | Some xs -> Some (i :: xs))
              m ))
        (0, StringMap.empty) old_lines
    in
    let overlap = ref IntMap.empty in

    let sub_start_old = ref 0 in
    let sub_start_new = ref 0 in
    let sub_length = ref 0 in

    Array.iteri
      (fun inew v ->
        let overlap' = ref IntMap.empty in
        let old_indices =
          StringMap.find_opt v old_index_map |> Option.value ~default:[]
        in
        List.iter
          (fun iold ->
            let o =
              1
              + (IntMap.find_opt (iold - 1) !overlap |> Option.value ~default:0)
            in
            overlap' := IntMap.add iold o !overlap';

            if o > !sub_length then (
              sub_length := o;
              sub_start_old := iold - o + 1;
              sub_start_new := inew - o + 1))
          old_indices;

        overlap := !overlap')
      new_lines;

    (!sub_start_new, !sub_start_old, !sub_length)

  let get_diff old_lines new_lines =
    let rec get_diff' old_lines new_lines =
      match (old_lines, new_lines) with
      | [||], [||] -> []
      | old_lines, [||] -> [Deleted old_lines]
      | [||], new_lines -> [Added new_lines]
      | _, _ ->
        let sub_start_new, sub_start_old, sub_length =
          longest_subsequence old_lines new_lines
        in
        if sub_length = 0 then [Deleted old_lines; Added new_lines]
        else
          let old_lines_presubseq = Array.sub old_lines 0 sub_start_old in
          let new_lines_presubseq = Array.sub new_lines 0 sub_start_new in
          let old_lines_postsubseq =
            let start_index = sub_start_old + sub_length in
            let len = Array.length old_lines - start_index in
            Array.sub old_lines start_index len
          in
          let new_lines_postsubseq =
            let start_index = sub_start_new + sub_length in
            let len = Array.length new_lines - start_index in
            Array.sub new_lines start_index len
          in
          let unchanged_lines = Array.sub new_lines sub_start_new sub_length in
          List.concat
            [
              get_diff' old_lines_presubseq new_lines_presubseq;
              [Equal unchanged_lines];
              get_diff' old_lines_postsubseq new_lines_postsubseq;
            ]
    in
    get_diff' (Array.of_list old_lines) (Array.of_list new_lines)
end

type edit =
  | Insert of string array
  | Replace of string array * string array
  | Delete of string array

let text_edit ~line_sep ~line edit =
  let deleted_lines, added_lines =
    match edit with
    | Insert adds -> (None, Some adds)
    | Replace (dels, adds) -> (Some dels, Some adds)
    | Delete dels -> (Some dels, None)
  in
  let start = Protocol.{character = 0; line} in
  let end_ =
    Protocol.
      {
        character = 0;
        line =
          (match deleted_lines with
          | None -> line
          | Some dels -> line + Array.length dels);
      }
  in
  let range = Protocol.{start; end_} in
  let newText =
    match added_lines with
    | None -> ""
    | Some adds -> (adds |> Array.to_list |> String.concat line_sep) ^ line_sep
  in
  Protocol.{newText; range}

let str_findi =
  let rec loop s len ~f i =
    if i >= len then None
    else if f (String.unsafe_get s i) then Some i
    else loop s len ~f (i + 1)
  in
  fun ?from s ~f ->
    let len = String.length s in
    let from =
      match from with
      | None -> 0
      | Some i -> if i > len - 1 then failwith "findi: invalid from" else i
    in
    loop s len ~f from

let edit ~from:orig ~to_:formatted : Protocol.textEdit list =
  let orig_lines = String.split_on_char '\n' orig in
  let formatted_lines = String.split_on_char '\n' formatted in
  (* TODO: better way of knowing this ? *)
  let line_sep =
    match str_findi ~f:(fun c -> c = '\r') formatted with
    | Some _ -> "\n\r"
    | None -> "\n"
  in
  let line, prev_deleted_lines, edits_rev =
    let diff_list = Diff.get_diff orig_lines formatted_lines in
    List.fold_left
      (fun (line, prev_deleted_lines, edits_rev) edit ->
        match (edit : Diff.t) with
        | Deleted deleted_lines ->
          (line, Array.append prev_deleted_lines deleted_lines, edits_rev)
        | Added added_lines ->
          let edit =
            text_edit ~line_sep ~line
              (if Array.length prev_deleted_lines > 0 then
               Replace (prev_deleted_lines, added_lines)
              else Insert added_lines)
          in
          let line = line + Array.length prev_deleted_lines in
          (line, [||], edit :: edits_rev)
        | Equal equal_lines ->
          let edits_rev =
            if Array.length prev_deleted_lines > 0 then
              text_edit ~line_sep ~line (Delete prev_deleted_lines) :: edits_rev
            else edits_rev
          in
          let line =
            line + Array.length prev_deleted_lines + Array.length equal_lines
          in
          (line, [||], edits_rev))
      (0, [||], [])
      diff_list
  in
  let edits_rev =
    if Array.length prev_deleted_lines > 0 then
      text_edit ~line_sep ~line (Delete prev_deleted_lines) :: edits_rev
    else edits_rev
  in
  List.rev edits_rev
