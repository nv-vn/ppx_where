# ppx_where
Haskell-style `where` clauses as a PPX syntax extension

## Examples:

```ocaml
let () = Array.iter print_endline args
  where args = Sys.argv
```
gets translated into
```ocaml
let args = Sys.argv in
let () = Array.iter print_endline args
```
