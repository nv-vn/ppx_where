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

(* Test default case *)
let () = print_endline (always 15)
    where always any = "This is always true"

(* Test polymorphic variants and `or` clauses *)
let () = print_endline (a_or_b `B)
    where a_or_b (`A or `B) = "It's either `A or `B"

(* Test record matching *)
type stuff = {
  first : int;
  second : string
}

let () = print_endline (string_of_int (first_field {first = 1; second = "2"}))
    where first_field {first} = first

(* Test array matching *)
let () = print_endline (string_of_int (third [| 1; 2; 3; 4; 5 |]))
    where third [| any; any; third; any; any |] = third

(* Test lazy matching *)
let () = print_endline (force' (lazy "Hello"))
    where force' (lazy msg) = msg

(* Test recursive where clauses
let () = print_num 15
    where print_num = print_endline <<< string_of_int
           where (<<<) f g x = f (g x) *)
