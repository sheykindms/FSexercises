// 23.4.1

let rec balance (a, b, c) =
    if c > 11 then
        balance (a, b + 1, c - 12)
    elif b > 19 then
        balance (a + 1, b - 20, c)
    elif c < 0 && b > 0 then
        balance (a, b - 1, c + 12)
    elif c < 0 && a > 0 then
        balance (a - 1, b, c + 12 * 20)
    elif b < 0 && a > 0 then
        balance (a - 1, b + 20, c)
    else
        (a, b, c)

let rec (.+.) (a, b, c) (x, y, z) = balance (a + x, b + y, c + z)

let rec (.-.) (a, b, c) (x, y, z) = balance (a - x, b - y, c - z)

// 23.4.2
let (.+) (a, b) (c, d) = (a + c, b + d)
let (.-) (a, b) (c, d) = (.+) (a, b) (-c, -d)
let (.*) (a, b) (c, d) = (a * c - b * d, b * c + a * d)

let (./) (a, b) (c, d) =
    (.*) (a, b) (c / (c * c + d * d), -d / (c * c + d * d))

//printf $"{(1, 0, 128) .+. (32, 23, 5)}\n"
//printf $"{(32, 23, 5) .-. (1, 0, 128)}\n"
//
//printf $"{(.+) (2, 3) (8, 7)}"
//printf $"{(.-) (10, 12) (5, 6)}"
//printf $"{(.*) (2, 3) (10, 100)}"
//printf $"{(./) (100, 15) (10, 5)}"
