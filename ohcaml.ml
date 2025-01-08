# 
let rec quicksort lst =  
        match lst with
        | [] -> [] 
        | pivot :: rest ->
              let rec partition left right = function
                      [] -> (left, right)
                     | x :: xs ->
                        if x <= pivot then
                            partition (x :: left) right xs
                        else
                            partition left (x :: right) xs
              in 
              let left, right = partition [] [] rest in 
              quicksort left
              @ [pivot]
              @ quicksort right;;

#
let quicksort_pop lst =
        match quicksort lst with
        | [] -> None
        | x :: xs ->
            Some (x :: xs);;

(* let quicksort lst = List.sort compare lst;; *)

let lst = [7; 3; 2; 4];;

let result = quicksort_pop lst;;

match result with
  | None -> print_endline "List is empty!"
  | Some (smallest, rest) ->
      Printf.printf "Smallest: %d\n" smallest;
      Printf.printf "Rest: [%s]\n"
        (String.concat "; " (List.map string_of_int rest));;


