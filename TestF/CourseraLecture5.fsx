let rec removeAt (n: int) (xs: List<'a>) : List<'a> =
    match xs with   
        | [] -> []
        | head::tail ->  if n = 0 then tail else head :: removeAt (n-1) tail 

removeAt 1 ['a';'b';'c';'d']

(* Al parecer la inferencia de tipos no me deja hacer esto manual, no con lo que se al menos, una lsita de lsitas, se peude con List.concat.

let rec flatten xs: List<'a> =
        match xs with            
            | [] -> []            
            | (x: List<'a>) :: xs -> flatten x @ flatten xs            
            | (x: 'a) :: xs -> x :: flatten xs.Tail
         
         

flatten [[1;2;3];[1;2];[1];[]]  *)

let rec pack (xs: List<'a>): List<List<'a>> =
    match xs with 
        | [] -> []
        | x::_ -> let start = xs |> List.takeWhile (fun y -> y = x)
                  start :: pack (snd ( xs |> List.splitAt start.Length ))

let encode (xs: List<'a>): List<'a*int> =
    xs |> pack |> List.map (fun l -> (l.Head, l.Length))

let lenghtFun (xs: List<'a>) : int =
    List.fold (fun acc _ -> 1+acc) 0 xs

encode ['a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a']
encode [1; 1; 1; 2; 2; 2; 3; 3; 1] 

lenghtFun [1..2..100]

List.fold (fun acc x -> x+acc) 0 [1;2]
List.fold (fun acc x -> x*acc) 1 [1..100]