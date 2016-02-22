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

#### With pattern parameters:
```ocaml
second (1, 2)
  where second (x, y) = y
```
becomes
```ocaml
let second = fun (x, y) -> y in
second (1, 2)
```

## Limitations:

* No nested/recursive where clauses
* Only one where clause per expression
* Can't use `_` or `|` patterns in arguments
* No support for default args or labels (yet)
* No support for record matching, polymorphic variants, lazy matches, or array literals (yet)
