// 49.5.1 Определите последовательность чётных положительных чисел.
let even_seq = Seq.initInfinite (fun x -> (x + 1) * 2)

// 49.5.2 Определите последовательность факториалов неотрицательных целых чисел 1,1,2,6,...,n!
let fac_seq =
    let rec fact n =
        match n with
        | 0 -> 1
        | 1 -> 1
        | _ -> n * fact (n - 1)

    Seq.initInfinite fact

// 49.5.3 Определите последовательность 0, -1, 1, -2, 2, -3, 3, ...
let seq_seq =
    let rec mirrored n =
        if n = 0 then 0
        elif n % 2 = 0 then (n / 2)
        else n / 2 - n

    Seq.initInfinite mirrored
