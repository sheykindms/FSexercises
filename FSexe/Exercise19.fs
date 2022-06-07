// 48.4.1 Напишите две версии функции вычисления n-го числа Фибоначчи Fn.
let rec fibo1 n n1 n2 =
    if (n = 0) then 0
    elif n < 2 then 1
    else fibo1 (n - 1) (n1 + n2) n1

// 48.4.2 Напишите две версии функции вычисления n-го числа Фибоначчи Fn.
let rec fibo2 n c =
    if (n = 0) then
        c 0
    elif (n = 1) then
        c 1
    else
        (fibo2 (n - 1) c) + (fibo2 (n - 2) c)

// 48.4.3 Напишите функцию генерации списка с хвостовой рекурсией
let rec bigList n k =
    let rec f acc list =
        if acc = 0 then
            k list
        else
            f (acc - 1) (1 :: k list)

    f n []
