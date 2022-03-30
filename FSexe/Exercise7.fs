// 20.3.1
let vat (n:int) (x:float) = x + x * (float)n / (float)100

// 20.3.2
let unvat (n:int) (x:float) = x * (float)100 / (float)(100 + n)

// 20.3.3
let rec min f =
    let rec loop = function
        | n when f n = 0 -> n
        | n when f n <> 0 -> loop (n + 1)
    loop 0

//printf $"{vat 10 500}\n"

//printf $"{unvat 10 550}"

//printf $"{min (fun x -> x - 999999999)}"
