open System

let isPrime (n: int): bool =
    [1..n] |> List.filter (fun x -> n%x = 0) |> List.length |> (>=) 2

let pairs (n: int): List<int*int> =
    [1..n-1] |> List.collect (fun i ->  [1..i-1] |> List.filter (fun k -> isPrime (i+k)) |> List.map (fun y -> (i, y))) 

let pairs2 (n: int): List<int*int>=
    seq {
        for i in [1 .. n-1] do 
        for j in [1..i-1] do 
        if (isPrime(i+j)) then yield (i,j)
    } |> Seq.toList

pairs 7
pairs2 7

(* ------------------------------------------------------------------------------------------------------------------------ *)

let isSafe(col: int, queens: List<int>): bool =
    let row = queens.Length
    let queensWithRow =  List.zip [row-1 .. -1 .. 0] queens
    queensWithRow |> List.forall (fun (r,c) -> col <> c && abs (col-c) <> (row - r))


let queens(n:int): Set<List<int>> = 
    let rec placeQueens(k: int): Set<List<int>> =
        if k = 0 then set [[]] else
            seq{
                for queens in placeQueens (k-1) do
                for col in [0 .. n-1] do
                if isSafe(col,queens) then yield col :: queens
            } |> Set.ofSeq
    placeQueens n

let show(queens: List<int>) =
    let t = Array.create queens.Length "*"
    "\n" + (queens |> List.map (fun col -> let t = Array.create queens.Length "* " 
                                           Array.fill t col 1 "Q " 
                                           t |> String.concat "") |> String.concat "\n")


queens 5 |> Seq.map show

(* ------------------------------------------------------------------------------------------------------------------------ *)

let romanNumbers = Map.empty.Add("I", 1).Add("V",5).Add("X",10)
let someI = romanNumbers.TryFind "I"
someI.Value


let i = ["apple"; "pear"; "orange"; "pinnaple"]
List.sort i
i |> List.sortBy (fun f -> f.Length) 
i |> List.groupBy (fun f -> f.Chars(0))

(* ------------------------------------------------------------------------------------------------------------------------ *)

type Poly (terms: Map<int, double>) =
    member __.terms = terms
    static member (+) (t1: Poly, t2: Poly) = 
        Map.fold (fun acc key value -> match Map.tryFind key acc with
                                       | Some x -> Map.add key (value + x) acc
                                       | None ->  Map.add key value acc) t1.terms t2.terms
        |> Poly

    override __.ToString() = seq{
                                for (exp, coef) in Map.toList __.terms |> List.sort do
                                yield coef.ToString() + "x^" + exp.ToString()
                                } |> String.concat " + "

let p1 = Poly (Map.empty.Add(1, 2.0).Add(3, 4.0).Add(5, 6.2))
let p2 = Poly (Map.empty.Add(0, 3.0).Add(3,7.0))

p1 + p2

(* ------------------------------------------------------------------------------------------------------------------------ *)

let nmem = Map([('2', "ABC"); ('3', "DEF"); ('4', "GHI"); ('5', "JKL"); ('6', "MNO"); ('7', "PQRS"); ('8', "TUV"); ('9', "WXYZ");])

//let aux = Map.map (fun _ v -> Seq.toList v) nmem
//let charCode = Map.map (fun k v -> Map(seq{for x in v do yield (x, k)}) ) aux 

let charCode = seq {for (k, value) in Map.toList nmem do
                        for v in Seq.toList value do
                            yield (v, k)
                    } |> Map

let wordCode (word: string) = 
    word.ToUpper() |>  Seq.toList |> List.map (fun l -> match charCode.TryFind l with
                                                           | Some x -> x
                                                           | None ->  ' ') |> System.String.Concat

wordCode "Java"

//let wordsForNum