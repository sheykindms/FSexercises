// 17.1
let rec pow =
    function
    | (str, 1) -> str
    | (str, index) -> str + pow (str, index - 1)

// 17.2
let isIthChar (str: string) (index: int) (c: char) = str.Chars(index) = c


// 17.3
let rec occFromIth (getStr: string) (c: char) =
    let rec loop i count =
        if i < getStr.Length then
            if getStr.[i] = c then
                loop (i + 1) (count + 1)
            else
                loop (i + 1) count
        else
            count

    loop 0 0
