let rec sort lst =
    match lst with
    | [] -> []  (* Base case: An empty list is already sorted. *)
    | x :: xs -> (* Split the list into the head (x) and tail (xs). *)
        let rec insert y sorted = (* Define a helper function for insertion. *)
            match sorted with
            | [] -> [y] (* If the sorted list is empty, return a new list containing y. *)
            | z :: zs -> (* Compare y with the head (z) of the sorted list. *)
                if y <= z then y :: sorted (* Place y before z if y is smaller or equal. *)
                else z :: insert y zs  (* Otherwise, keep z and continue inserting y into the tail (zs). *)
        in
        insert x (sort xs) (* Insert the head (x) into the sorted version of the tail (xs). *)
