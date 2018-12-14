type set = int -> bool 

let contains (s:set, x:int) = s x

let singletonSet (x: int) : set = fun y -> y = x

let union (s1: set, s2:set) : set = fun x -> s1 x || s2 x
let intersect (s1: set, s2:set) : set = fun x -> s1 x && s2 x
let diff (s1: set, s2:set) : set = fun x -> s1 x && not (s2 x)
let filter (s1: set, pred: int -> bool) : set = intersect(s1,pred)


let set1 = fun x -> x % 2 = 1
contains(set1 , 5)
contains(set1, 2)

let singleton2 = singletonSet(1)
let singleton1 = singletonSet(0)
contains(singleton1,2)

let set2 = union(set1, singleton1)
contains(set2, 2)

let set3 = intersect (singleton2, set1)
contains(set3, 2)
contains(set3, 1)

let set4 = diff (set1, singleton2)
contains(set4, 1)
contains(set4, 3)

let set5 = filter(set1, fun x-> x<6)
contains(set5, 7)
contains(set5, 5)