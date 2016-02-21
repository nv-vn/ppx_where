open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

exception PpxWhereException of string

let omap f = function
  | None -> None
  | Some x -> Some (f x)

(* Add:
   - Records
   - Polymorphic variants
   - __ -> _ or `Any -> _ or any -> _ ?
   - Array literals
   - Lazy literals
   - || -> Extra | clauses ? *)

let rec pattern_of_expr ~loc = function
  | Pexp_ident {txt = Lident id} -> Pat.var ~loc {txt = id; loc}
  | Pexp_constant const -> Pat.constant const
  | Pexp_tuple vals -> Pat.tuple ~loc (List.map (fun {pexp_desc} -> pattern_of_expr ~loc pexp_desc) vals)
  | Pexp_construct (constructor, args) -> Pat.construct ~loc constructor (omap (fun {pexp_desc} -> pattern_of_expr ~loc pexp_desc) args)
  | _ -> Pat.any ~loc ()

(* Parsing: `f 1 2 where f a b = a + b`:
   Pexp_apply "=":
     Pexp_apply "f":
       Pexp_constant 1
       Pexp_constant 2
       Pexp_ident "where"
       Pexp_ident "a"
       Pexp_ident "b"
     Pexp_apply "+":
       Pexp_ident "a"
       Pexp_ident "b" *)

let expand_where ~loc = function
  | [_, {pexp_desc = Pexp_apply ({pexp_desc = f}, context)}; _, body] -> begin
      let context' = f::(List.map (fun (_, {pexp_desc}) -> pexp_desc) context) in
      let rec split_at_where ?(after = false) ?(pre=[]) ?(post=[]) = function
        | [] -> (List.rev pre, List.rev post) (* We're cons-ing backwards here *)
        | e::es when after -> split_at_where ~after ~pre ~post:(e::post) es
        | (Pexp_ident {txt = Lident "where"})::es -> split_at_where ~after:true ~pre ~post es
        | e::es -> split_at_where ~pre:(e::pre) ~post es in
      let (pre_where, post_where) = split_at_where context' in
      if pre_where = [] || post_where = [] then None
      else begin
        let name = pattern_of_expr ~loc (List.hd post_where)
        and matches = List.map (pattern_of_expr ~loc) (List.tl post_where) in
        let new_context = match pre_where with
          | [] -> raise (PpxWhereException "Keyword `where` used in invalid context")
          | [f] -> Exp.mk f
          | f::xs -> Exp.apply (Exp.mk f) (List.map (fun e -> ("", Exp.mk e)) xs) in
        Some (Exp.let_ Nonrecursive [Vb.mk name body] new_context)
      end
    end
  | _ -> None (* Maybe we should indicate some error in this case *)

let where_mapper argv =
  {default_mapper with
   expr = begin fun mapper expr ->
     match expr with
     | {pexp_desc; pexp_loc; pexp_attributes} -> begin
         match pexp_desc with
         | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "="}}, args) -> begin
             match expand_where ~loc:pexp_loc args with
             | Some expanded -> expanded
             | None -> expr
           end
         | _ -> expr
       end
     | _ -> expr
   end
  }

let _ = register "ppx_where" where_mapper
