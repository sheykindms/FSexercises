type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) (x:TimeOfDay) (y:TimeOfDay) =
    if x.f = "PM" && x.f <> y.f then true
    elif y.f = "PM" && x.f <> y.f then false
    elif x.hours * 60 + x.minutes > y.hours * 60 + y.minutes then true
    else false


//let greaterTime = {hours = 11; minutes = 59; f = "AM"}
//let lesserTime = {hours = 00; minutes = 00; f = "AM"}
//
//printf $"{greaterTime .>. lesserTime}"