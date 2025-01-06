

;;
let rec quicksort lst = 
  match lst with
  | [] -> []
  | pivot :: rest ->
    let rec partition left right = function
      | [] -> (left, right)
      | x :: xs ->
        if x <= pivot then
          partition (x :: left) right xs
        else
          partition left (x :: right) xs
    in
    let left, right = partition [] [] rest in
    quicksort left @ [pivot] @ quicksort right;;