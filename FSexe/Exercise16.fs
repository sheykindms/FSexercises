//42.3. Напишите функцию allSubsets, получающую целочисленные параметры n и k, и выдающую множество всех подмножеств множества {1, 2, ..., n}, в которых ровно k элементов (0 < k < n)
let rec allSubsets n k =
    let rec powerSet xs =
        match xs with
        | [] -> [ [] ]
        | head :: tail -> List.fold (fun head1 tail1 -> (head :: tail1) :: tail1 :: head1) [] (powerSet tail)

    Set.filter (fun xs -> Set.count xs = k) (Set.ofList (List.map Set.ofList (powerSet [ 1 .. n ])))
   