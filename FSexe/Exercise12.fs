// 34.1
let rec upto =
    function
    | 1 -> [ 1 ]
    | n -> upto (n - 1) @ [ n ]

// 34.2
let rec dnto =
    function
    | 1 -> [ 1 ]
    | n -> n :: dnto (n - 1)


// 34.3
let evenn n =
    [ for i in 0 .. n do
          if i % 2 = 0 then yield i ]
    
//printf $"{evenn 17}"