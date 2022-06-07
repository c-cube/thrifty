open Ast

type t = { headers: Header.t list; def_scc_l: Definition.t list list }

let iter_const_value (v : Const_value.t) f : unit =
  let rec aux (v : Const_value.t) =
    f v;
    match v with
    | Bool _ | Int _ | Double _ | String _ -> ()
    | List l -> List.iter aux l
    | Map l ->
      List.iter
        (fun (k, v) ->
          aux k;
          aux v)
        l
    | Named _ -> ()
  in
  aux v

let names_in_const_value (v : Const_value.t) : identifier Iter.t =
  iter_const_value v
  |> Iter.filter_map (function
       | Const_value.Named name -> Some name
       | _ -> None)

let iter_ty (ty : Type.t) f : unit =
  let rec aux ty =
    f ty;
    match ty.Type.view with
    | Type.List ty | Type.Set ty -> aux ty
    | Type.Map (a, b) ->
      aux a;
      aux b
    | Type.Base _ | Type.Named _ -> ()
  in
  aux ty

let names_in_ty (ty : Type.t) : identifier Iter.t =
  iter_ty ty
  |> Iter.filter_map (function
       | Type.{ view = Named i; _ } -> Some i
       | _ -> None)

let names_in_field (f : Field.t) =
  Iter.(
    append (names_in_ty f.ty)
      (of_option f.default |> flat_map names_in_const_value))

let names_in_fun_ty (ty : Function_type.t) =
  match ty with
  | Function_type.Void -> Iter.empty
  | Function_type.Ty ty -> names_in_ty ty

let names_in_fun (f : Function.t) : _ Iter.t =
  Iter.(
    of_list
      [
        names_in_fun_ty f.ty;
        of_list f.args |> flat_map names_in_field;
        of_option f.throws |> flat_map of_list |> flat_map names_in_field;
      ]
    |> flatten)

let names_in_def (d : Definition.t) : identifier Iter.t =
  match d.view with
  | Definition.Const { ty; value } ->
    Iter.append (names_in_ty ty) (names_in_const_value value)
  | Definition.TypeDef { ty } -> names_in_ty ty
  | Definition.Enum _ -> Iter.empty
  | Definition.Union { fields }
  | Definition.Exception { fields }
  | Definition.Struct { fields } ->
    Iter.of_list fields |> Iter.flat_map names_in_field
  | Definition.Service { funs } ->
    Iter.of_list funs |> Iter.flat_map names_in_fun

module Scc = struct
  (* code reused from containers to compute strongly connected components *)

  type name = string

  type node = {
    mutable min_id: int; (* min ID of the vertex' scc *)
    id: int; (* ID of the vertex *)
    mutable on_stack: bool;
    mutable vertex: Definition.t;
  }

  type graph = { nodes: (string, Definition.t) Hashtbl.t }

  let mk_cell (d : Definition.t) n =
    { min_id = n; id = n; on_stack = false; vertex = d }

  (* pop elements of [stack] until we reach node with given [id] *)
  let rec pop_down_to ~id acc stack =
    assert (not (Stack.is_empty stack));
    let cell = Stack.pop stack in
    cell.on_stack <- false;
    if cell.id = id then (
      assert (cell.id = cell.min_id);
      cell.vertex :: acc (* return SCC *)
    ) else
      pop_down_to ~id (cell.vertex :: acc) stack

  let iter_out_edges (graph : graph) (d : Definition.t) : Definition.t Iter.t =
    names_in_def d
    |> Iter.map (fun n ->
           try Hashtbl.find graph.nodes n
           with Not_found ->
             failwith (Printf.sprintf "identifier %s not found" n))

  let top (l : Definition.t list) : _ list list =
    (* turn [l] into a map for easy access *)
    let graph =
      {
        nodes =
          List.mapi (fun i d -> d.Definition.name, d) l
          |> List.to_seq |> Hashtbl.of_seq;
      }
    in
    let tbl : (name, node) Hashtbl.t = Hashtbl.create 32 in
    let scc_l : _ list list ref = ref [] in
    ((* stack of nodes being explored, for the DFS *)
     let to_explore = Stack.create () in
     (* stack for Tarjan's algorithm itself *)
     let stack = Stack.create () in
     (* unique ID *)
     let n = ref 0 in
     (* exploration *)
     List.iter
       (fun v ->
         Stack.push (`Enter v) to_explore;
         while not (Stack.is_empty to_explore) do
           match Stack.pop to_explore with
           | `Enter d ->
             if not (Hashtbl.mem tbl d.Definition.name) then (
               (* remember unique ID for [v] *)
               let id = !n in
               incr n;
               let cell = mk_cell d id in
               cell.on_stack <- true;
               Hashtbl.add tbl d.name cell;
               Stack.push cell stack;
               Stack.push (`Exit (d, cell)) to_explore;
               (* explore children *)
               iter_out_edges graph d (fun d' ->
                   Stack.push (`Enter d') to_explore)
             )
           | `Exit (d, cell) ->
             (* update [min_id] *)
             assert cell.on_stack;
             iter_out_edges graph d (fun d' ->
                 (* must not fail, [dest] already explored *)
                 let dest_cell = Hashtbl.find tbl d'.name in
                 (* same SCC? yes if [dest] points to [cell.v] *)
                 if dest_cell.on_stack then
                   cell.min_id <- min cell.min_id dest_cell.min_id);
             (* pop from stack if SCC found *)
             if cell.id = cell.min_id then (
               let scc = pop_down_to ~id:cell.id [] stack in
               scc_l := scc :: !scc_l
             )
         done)
       l;
     assert (Stack.is_empty stack);
     ());
    List.rev !scc_l
end

let top (f : File.t) : t =
  let def_scc_l = Scc.top f.defs in
  { headers = f.headers; def_scc_l }
