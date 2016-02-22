(* Test without args *)
let () = print "Hello, world"
    where print = print_endline

(* Test with args *)
let () = print_half 2
    where print_half x = print_endline (string_of_int (x / 2))

(* Test defining an operator and multiple arguments *)
let () = print_endline ("world!" ^^^ "Hello, ")
    where (^^^) y x = x ^ y

(* Test constants/literals as args *)
let () = first_number 1
    where first_number 1 = ()

(* Test constructors as args *)
let () = print_endline (string_of_int (second [1; 2; 3]))
    where second (x::y::xs) = y

(* Test tuples *)
let () = print (show 5)
    where (print, show) = (print_endline, string_of_int)

(* Test recursive where clauses
let () = print_num 15
    where print_num = print_endline <<< string_of_int
           where (<<<) f g x = f (g x) *)
