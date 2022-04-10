type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) (x:TimeOfDay) (y:TimeOfDay) =
  match x.f,y.f with
  | PM, AM -> true
  | AM, PM -> false
  | _ -> x > y
  
//let a = {hours = 11; minutes = 22; f = AM}
//let b = {hours = 11; minutes = 21; f = PM}
//
//printf $"{ a .>. b}"
//printf $"{ b .>. a}"