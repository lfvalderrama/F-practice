let sum (f: int -> int) (a:int) (b:int) =
    let rec loop (a:int) (acc :int) : int =
        if ( a > b) then acc 
        else loop (a+1) (acc + f a)
    loop a 0


let rec fact(x: int) : int = if (x = 1) then 1 else x * fact (x-1)
    

let sumInts = sum id
let sumCubes = sum (fun x -> x*x*x)
let sumFactorials = sum fact

sumInts 1 5
sumCubes 1 3
sumFactorials 1 5

let mult (f: int -> int) (a:int) (b:int) =
    let rec loop (a:int) (acc :int) : int =
        if ( a > b) then acc 
        else loop (a+1) (acc * f a)
    loop a 1


let fact2(x: int) : int = mult id 1 x

fact 5 

let general (f: int -> int) (a:int) (b:int) (op: int -> int -> int) (zero: int)=
    let rec loop (a:int) (acc :int) : int =
        if ( a > b) then acc 
        else loop (a + 1) (op acc (f a))
    loop a zero

let fact3(x: int) : int = general id 1 x (*) 1 
let sumCubes2 (a:int) (b:int) : int = general (fun x-> x*x*x ) a b (+) 0 

sumCubes2 1 3

fact3 5


type rational( x: int , y: int) =
    let rec gcd (a: int, b: int) : int = if b=0 then a else gcd(b, a%b);
    let g = gcd(x,y)
    member __.numer = x / g
    member __.denom = y / g

    member __.less(other: rational) = __.numer * other.denom < other.numer * __.denom

    member __.max(other: rational) = if __.less(other) then other else __

    member this.add(other: rational) =
        rational (this.numer * other.denom + other.numer * this.denom,
                  this.denom * other.denom)
    
    member this.sub(other: rational) = this.add(other.neg)
    
    member __.neg = rational (-__.numer, __.denom)
           
    override __.ToString() = __.numer.ToString() + "/" + __.denom.ToString()

    new(x: int) = rational(x,1)

let rat1 = rational (1, 3)
let rat2 = rational (5, 7)
let rat3 = rational (3, 2)
let num = rational(5)

rat1.add(rat2)
rat1.neg
rat1.sub(rat2).sub(rat3)
rat3.add(rat3)
rat1.less(rat2)
rat1.max(rat2)
