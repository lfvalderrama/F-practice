let toCharList (s:string) : List<char>=
    s.ToCharArray() |> Array.toList


type CodeTree =
    | Fork of left: CodeTree * right: CodeTree * chars: List<char> * weight: int
    | Leaf of char: char * weight: int

let weight (tree: CodeTree) : int =
    match tree with 
        | Fork (_,_,_,w) -> w
        | Leaf (_, w) -> w

let chars(tree: CodeTree) : List<char> =
    match tree with 
        | Fork (_,_,cl,_) -> cl
        | Leaf (c, _) -> c::[]

let makeCodeTree (left: CodeTree) (right: CodeTree) =
    Fork(left,right, chars left @ chars right, weight left + weight right)

let createCodeTree(chars: List<char>) : CodeTree = 
    
    let rec times (chars: List<char>) : List<char * int> =   
        match chars with
            | [] -> []
            | _ -> (chars.Head, chars |> List.filter (fun x -> x = chars.Head)  |> List.length ) :: 
                   (chars |> List.filter (fun x -> x <> chars.Head)|> times)
    
    let makeOrderedLeafList (freqs: List<char * int>) =
        List.sortBy (fun (_, i) -> i ) freqs |> List.map (fun (c,i) -> Leaf(c,i) )
       
    let singleton (trees: List<CodeTree>) : bool =
        if List.length trees = 1 then true else false

    let combine (trees: List<CodeTree>) : List<CodeTree> =
        makeCodeTree trees.Head trees.Tail.Head :: trees.Tail.Tail |> List.sortBy (fun x -> weight x)

    let rec until (trees: List<CodeTree>) : CodeTree =
        if singleton trees then trees.Head else until (combine trees)

    chars |> times |> makeOrderedLeafList |> until




let sampleTree = makeCodeTree (makeCodeTree ( Leaf ('x', 1) ) (Leaf ('e', 1))) (Leaf ('t', 2))
let wordArray = toCharList "asdasdiahsodinaosidnasiod"
let tree1 = createCodeTree wordArray