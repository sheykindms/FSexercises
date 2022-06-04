// 47.4.1 Напишите функцию факториала f: int -> int, не используя рекурсию, с помощью императивных возможностей.
let f n =
    if n < 2 then
        1
    else
        let mutable index = ref 1
        let mutable result = ref 1

        while index.Value <= n do
            result.Value <- result.Value * index.Value
            index.Value <- index.Value + 1

        result.Value

// 47.4.2 Напишите функцию fibo: int -> int, где fibo(n) вычисляет n-е число Фибоначчи (n >= 0), не используя рекурсию, с помощью императивных возможностей.
let fibo n =
    if (n = 0) then
        0
    elif (n = 1) then
        1
    else
        let mutable elemN2 = ref 0
        let mutable elemN1 = ref 1
        let mutable index = ref 2
        let mutable result = ref 0

        while index.Value <= n do
            result.Value <- elemN2.Value + elemN1.Value
            elemN2.Value <- elemN1.Value
            elemN1.Value <- result.Value
            index.Value <- index.Value + 1

        result.Value