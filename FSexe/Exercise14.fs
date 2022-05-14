// 40.1 Напишите функцию sum(p, xs), где p -- предикат int -> bool, и xs -- список целых. Функция возвращает сумму тех элементов xs, для которых предикат истинен.
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | [ x ] when p x -> x
    | head :: tail when (p head) -> head + sum (p, tail)
    | _ :: tail -> 0 + sum (p, tail)
    | _ -> 0

//40.2. Список [x1; x2; ...; xn] называется слабо восходящим, если его элементы удовлетворяют требованию
//x1 <= x2 <= ... <= xn
// 40.2.1. Напишите функцию count: int list * int -> int, которая подсчитывает количество вхождений числа в список.
let rec count (xs, n) =
    match xs, n with
    | [], _ -> 0
    | head :: tail, n when head < n -> count (tail, n)
    | head :: tail, n when head = n -> 1 + count (tail, n)
    | head :: _, n when head > n -> 0
    | _ -> 0

// 40.2.2 Напишите функцию insert: int list * int -> int list, которая добавляет новый элемент в список.
let rec insert (xs, n) =
    match xs, n with
    | head :: tail, n when head <= n -> [ head ] @ insert (tail, n)
    | head :: tail, n when head > n -> [ n ] @ head :: tail
    | [], n -> [ n ]
    | _, _ -> []

//40.2.3. Напишите функцию intersect: int list * int list -> int list, которая находит общие элементы в обоих списках, включая повторяющиеся.
let rec intersect (xs1, xs2) =
    match (xs1, xs2) with
    | head1 :: tail1, head2 :: tail2 when head1 = head2 -> head1 :: intersect (tail1, tail2)
    | head1 :: _, head2 :: tail2 when head1 > head2 -> intersect (xs1, tail2)
    | head1 :: tail1, head2 :: _ when head1 < head2 -> intersect (tail1, xs2)
    | [ x ], [ y ] when x = y -> [ x ]
    | _ -> []

// 40.2.4. Напишите функцию plus: int list * int list -> int list, которая формирует список, объединяющий все элементы входных списков, включая повторяющиеся.
let rec plus (xs1, xs2) =
    match (xs1, xs2) with
    | head1 :: tail1, head2 :: _ when head1 < head2 -> [ head1 ] @ plus (tail1, xs2)
    | head1 :: _, head2 :: tail2 when head1 > head2 -> [ head2 ] @ plus (xs1, tail2)
    | head1 :: tail1, head2 :: tail2 when head1 = head2 -> [ head1 ] @ [ head2 ] @ plus (tail1, tail2)
    | xs1, [] -> xs1
    | [], xs2 -> xs2
    | [ x ], [ y ] when x = y -> [ x ] @ [ y ]
    | [ x ], [ y ] when x > y -> [ y ] @ [ x ]
    | [ x ], [ y ] when x < y -> [ x ] @ [ y ]
    | _ -> []


// 40.2.5. Напишите функцию minus: int list * int list -> int list, которая возвращает список, содержащий элементы первого списка за исключением элементов второго списка (элементы, одинаковые по значению, считаются разными).
let rec minus (xs1, xs2) =
    match (xs1, xs2) with
    | head1 :: tail1, head2 :: _ when head1 < head2 -> [ head1 ] @ minus (tail1, xs2)
    | head1 :: _, head2 :: tail2 when head1 > head2 -> minus (xs1, tail2)
    | head1 :: tail1, head2 :: tail2 when head1 = head2 -> minus (tail1, tail2)
    | xs1, [] -> xs1
    | [], _ -> []
    | [ x ], [ y ] when x = y -> []
    | [ x ], [ y ] when x <> y -> [ x ]
    | _ -> []


//40.3. Делаем сортировку.
// 40.3.1. Напишите функцию smallest: int list -> int option, которая возвращает наименьший элемент непустого списка.
let rec loop (xs, min) =
        match xs with
        | head::tail when head > min  -> loop(tail,min)
        | head::tail when head <= min  -> loop(tail,head)
        | [  ] -> Some min
        | _ -> Some min

let rec smallest = fun xs ->
    let head :: tail = xs
    loop(tail,head)

//40.3.2. Напишите функцию delete: int * int list -> int list, которая удаляет из списка первое вхождение заданного элемента (если он имеется).
let rec delete (n, xs) =
    match xs with
    | [] -> []
    | head :: tail when head <> n -> [head] @ delete(n,tail)
    | head :: tail when head = n -> tail
    | _ -> []

// 40.3.3. Напишите функцию сортировки с использованием предыдущих функций, которая сортирует входной список так, что на выходе получается слабо восходящий список.
let rec sort =
    fun xs ->
        let rec iteratorSort (list: 'b list) result =
            if (list.Length = 0) then
                result
            else
                let min = smallest list
                let tail = delete (min.Value, list)
                iteratorSort tail (result @ [ min.Value ])

        iteratorSort xs []

// 40.4. Напишите функцию revrev, которая получает на вход список списков, и перевёртывает как порядок вложенных списков, так и порядок элементов внутри каждого вложенного списка.
let rec revrev =
    fun (xs: list<list<int>>) ->
        match xs with
        | [] -> []
        | [ x ] -> [ List.rev x ]
        | head :: tail -> revrev tail @ [ List.rev head ]
        