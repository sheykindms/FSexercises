// 16.1
let notDivisible =
    function
    | (divider, number) -> number % divider = 0

// 16.2
let prime n =
    let rec check i =
        i > n / 2 || (n % i <> 0 && check (i + 1))

    check 2
