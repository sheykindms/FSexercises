// 47.4.1 Напишите функцию факториала f: int -> int, не используя рекурсию, с помощью императивных возможностей.
let f n =
    if n < 2 then
        1
    else
        let mutable index = 1
        let mutable result = 1

        while index <= n do
            result <- result * index
            index <- index + 1

        result

// 47.4.2 Напишите функцию fibo: int -> int, где fibo(n) вычисляет n-е число Фибоначчи (n >= 0), не используя рекурсию, с помощью императивных возможностей.
let fibo n =
    if (n = 0) then
        0
    elif (n = 1) then
        1
    else
        let mutable a = 0
        let mutable b = 1
        let mutable index = 2
        let mutable result = 0

        while index <= n do
            result <- a + b
            a <- b
            b <- result
            index <- index + 1
        result