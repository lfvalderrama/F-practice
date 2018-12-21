
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

