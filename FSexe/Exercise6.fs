// 17.1
let rec pow =
    function
    | (str, 1) -> str
    | (str, index) -> str + pow (str, index - 1)

// 17.2
let rec isIthChar =
    function
    | (_, index, _) when index < 0 -> false
    | (str, _, _) when String.length str <= 0 -> false
    | (str, index, c) -> (string str).[index] = c


// 17.3
let rec occFromIth (str, index, c) =
    let rec loop =
        function
        | (str, index, _, count) when String.length str <= index -> count
        | (str, index, c, count) when (string str).[index] = c -> loop (str, index + 1, c, count + 1)
        | (str, index, c, count) -> loop (str, index + 1, c, count)

    loop (str, index, c, 0)


//let a = pow ("hey", 2)
//printf $"%s{a}"
//
//let b = isIthChar ("cbath", 2, 'a')
//printf $"%b{b}"
//
//let c = occFromIth ("aaath", 0, 'a')
//printf $"%d{c}"
