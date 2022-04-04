type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) (x:TimeOfDay) (y:TimeOfDay) =
    if x.f = "PM" && x.f <> y.f then x > y
    elif y.f = "PM" && x.f <> y.f then y > x
    elif x.hours * 60 + x.minutes > y.hours * 60 + y.minutes then x > y
    else y > x


//let greaterTime = {hours = 11; minutes = 40; f = "AM"}
//let lesserTime = {hours = 10; minutes = 59; f = "AM"}
//
//printf $"{greaterTime .>. lesserTime}"