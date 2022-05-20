// 41.4.1. Напишите функцию list_filter, которая реализует стандартную функцию List.filter, с помощью List.foldBack.
let list_filter f xs = List.foldBack (fun head tail -> if f head then head::tail else tail) xs []

// 41.4.2. Напишите функцию sum(p, xs), где p -- предикат int -> bool, и xs -- список целых.
let sum (p, xs) = List.fold (+) 0 (List.filter p xs)

// 41.4.3. Напишите функцию revrev, которая получает на вход список списков, и перевёртывает как порядок вложенных списков, так и порядок элементов внутри каждого вложенного списка.
let revrev = fun lst -> List.fold (fun head tail -> (List.rev tail)::head) [] lst