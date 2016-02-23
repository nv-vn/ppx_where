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

## Special Syntax:

* The equivalent to `_` is `any`
* Instead of `|` use `or`

## Limitations:

* No nested/recursive where clauses
* Only one where clause per expression
* No support for default args or labels (yet)
* No support for lazy matches or array literals (yet)
* No intended support for constraints, unpacks, extensions, exceptions, intervals, type matches, or aliases for the time being
