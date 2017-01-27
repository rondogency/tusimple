
(* code samples from lecture 1, with a few extra notes *)

(* type #use "lec01.ml";; at the toplevel *)




(* variables don't vary.  They are more like definitions **********************)

let example1 =
  let x = 0 in
  let f y = x + y in
  let x = 3 in
  f 5

(* you should see "val example1 : int = 5" *)


(* sum squares of numbers from 1 to n *****************************************)

let rec sumsquares n =
  if n <= 0 then 0
  else n*n + sumsquares (n - 1)
 
let example2 = sumsquares 5


(* sum cubes of numbers from 1 to n *******************************************)

let rec sumcubes n =
  if n <= 0 then 0
  else n*n*n + sumcubes (n - 1)

let example3 = sumcubes 5

let rec sumof f n =
  if n <= 0 then 0
  else (f n) + sumof f (n - 1)

let square x = x * x
let cube     = (fun x -> x * x * x)

let sumsquares x = sumof square x
let sumcubes   x = sumof cube x

let sumcubes = sumof cube

let example4 = sumcubes 5

(* reverse ********************************************************************)

let rec reverse l = match l with
  | []     -> []
  | hd::tl -> (reverse tl) @ [hd]

let example5 = reverse [1;2;3;4]

(* a better version of reverse (remember rule #1?): *)

let reverse = List.rev

(* quicksort ******************************************************************)

let rec qsort l = match l with
  | []        -> []
  | mid::rest ->
    let left, right = List.partition ((<) mid) rest in
    (qsort left) @ [mid] @ (qsort right)

let example_list = [17; 35; 32; 8; -14]
let example6     = qsort example_list

(* a better version: (rule #1) ************************************************)

let rec qsort greater_than l = match l with
  | [] -> []
  | mid::rest ->
    let left, right = List.partition (greater_than mid) rest in
    (qsort greater_than left) @ [mid] @ (qsort greater_than right)

let qsort_increasing = qsort (>)
let qsort_decreasing = qsort (<)
let qsort_abs_sin    = qsort (fun x y -> abs_float (sin x) < abs_float (sin y))

let example7 = qsort_increasing example_list
let example8 = qsort_decreasing example_list

(* type error: can't take sin of an int *)
(* let example9 = qsort_abs_sin example_list *)
let example9 = qsort_abs_sin (List.map float_of_int example_list)

(* a still better version: (rule #1) ******************************************)

let qsort = List.sort

