// 39.1
let rec rmodd =
    function
    | [] -> []
    | [ x ] -> []
    | x :: xx :: xxs -> [ xx ] @ rmodd xxs

// 39.2
let rec del_even =
    function
    | [] -> []
    | x :: xs when (x % 2 <> 0) -> [ x ] @ del_even xs
    | x :: xs -> del_even xs

// 39.3
let rec multiplicity n ns =
    match n, ns with
    | n, [] -> 0
    | n, x :: xs when n = x -> 1 + multiplicity n xs
    | n, x :: xs when n <> x -> multiplicity n xs

// 39.4
let rec split =
    let rec even =
        function
        | [ x ] -> [ x ]
        | x :: (_ :: xs) -> x :: even xs
        | _ -> []

    let rec odd =
        function
        | [ _ ] -> []
        | _ :: (x :: xs) -> x :: odd xs
        | _ -> []

    fun xs -> (even xs, odd xs)

// 39.5
let rec zip (xxs, yys) =
    match xxs, yys with
    | [], [] -> []
    | x :: xs, y :: ys when (xxs.Length = yys.Length) -> [ (x, y) ] @ zip (xs, ys)
    | _ when (xxs.Length <> yys.Length) -> failwith "error"
