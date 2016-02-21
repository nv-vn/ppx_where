open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

exception PpxWhereException of string

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

let expand_where = function
  | [_, {pexp_desc = Pexp_apply ({pexp_desc = f}, context)}; _, {pexp_desc = body}] -> begin
      let context' = f::(List.map (fun (_, {pexp_desc}) -> pexp_desc) context) in
      let rec split_at_where ?(after = false) ?(pre=[]) ?(post=[]) = function
        | [] -> (List.rev pre, List.rev post) (* We're cons-ing backwards here *)
        | e::es when after -> split_at_where ~after ~pre ~post:(e::post) es
        | (Pexp_ident {txt = Lident "where"})::es -> split_at_where ~after:true ~pre ~post es
        | e::es -> split_at_where ~pre:(e::pre) ~post es in
      let (pre_where, post_where) = split_at_where context' in
      if pre_where = [] || post_where = [] then None
      else begin
        let new_context = match pre_where with
          | [] -> raise (PpxWhereException "Keyword `where` used in invalid context")
          | [f] -> Exp.mk f
          | f::xs -> Exp.apply (Exp.mk f) (List.map (fun e -> ("", Exp.mk e)) xs) in
        Some new_context
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
             match expand_where args with
             | Some expanded -> expanded
             | None -> expr
           end
         | _ -> expr
       end
     | _ -> expr
   end
  }

let _ = register "ppx_where" where_mapper
