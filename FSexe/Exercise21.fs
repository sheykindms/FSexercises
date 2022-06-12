// 50.2.1 Определите последовательности из пп. 49.5.2 и 49.5.4 с помощью выражений последовательностей.
let fac_seq =
    let rec fact n acc =
        seq {
            if n <= 1 then yield 1 else yield n
            yield! fact (n + 1) (acc * (n + 1))
        }

    fact 0 1

// 50.2.2 Определите последовательности из пп. 49.5.2 и 49.5.4 с помощью выражений последовательностей.
let seq_seq =
    let rec build n =
        seq {
            if n % 2 = 0 then
                yield (n / 2)
            else
                yield (-(n + 1) / 2)

            yield! build (n + 1)
        }

    build 0
    