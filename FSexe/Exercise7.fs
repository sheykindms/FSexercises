// 20.3.1
let vat n x = x + x * n / 100.0

// 20.3.2
let unvat n x = x * 100.0 / (100.0 + n)

// 20.3.3
let rec min f =
    let rec loop = function
        | n when f n = 0 -> n
        | n when f n <> 0 -> loop (n + 1)
    loop 0

//printf $"{vat 10 500}\n"

//printf $"{unvat 10 550}"

//printf $"{min (fun x -> x - 999999999)}"
