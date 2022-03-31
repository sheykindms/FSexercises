let curry f (x: int) = fun (y: int) -> f (x, y): int

let uncurry (f: int) (x: int) (y: int) =
    function
    | (x: int), (y: int) -> f: int
