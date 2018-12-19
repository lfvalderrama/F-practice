type CodeTree =
    | Fork of left: CodeTree * right: CodeTree * chars: List<char> * weight: int
    | Leaf of char: char * weight: int

type bit = int

type CodeTable = List<(char * List<bit>)>

let toCharList (s:string) : List<char>=
    s.ToCharArray() |> Array.toList

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


let encode (tree: CodeTree) (text: List<char>) : List<bit> =
    let treeContains (tree: CodeTree) (c: char) : bool =
        match tree with
            | Fork (_,_,lc,_) -> List.contains c lc
            | Leaf (ch,_) -> ch = c

    let rec loop (treeInternal: CodeTree) (text: List<char>) (acc: List<bit> ) : List<bit> =
        match text with 
            | [] -> acc
            | _ -> match treeInternal with
                    | Leaf _ -> loop tree text.Tail acc
                    | Fork (l, r, _, _)  -> if treeContains l text.Head then 
                                               let acc = acc @ [0]
                                               loop l text acc 
                                            else 
                                               let acc = acc @ [1]
                                               loop r text acc

    loop tree text []


let mergeCodeTables(a: CodeTable) (b: CodeTable) : CodeTable = a@b

let rec convert(tree: CodeTree) : CodeTable =
    let rec loop (treeInternal: CodeTree) : CodeTable =
        match treeInternal with 
            | Leaf (c,_) -> [c, encode tree [c]]
            | Fork (l,r,_,_) -> mergeCodeTables (loop l) (loop r) 
    
    loop tree 


let quickEncode (tree: CodeTree) (text: List<char>): List<bit> =
    let rec codeBits (table: CodeTable)(c: char): List<bit> =
        if List.length table = 0 then [] else
        match table.Head with 
            | (ca, lb) -> if ca = c then lb else codeBits table.Tail c
    
    let rec loop (table: CodeTable) (text: List<char>) (acc: List<bit> ) : List<bit> =
        match text with 
            | [] -> acc
            | head :: tail -> let acc = acc @ codeBits table head 
                              loop table tail acc 

    loop (convert tree) text []

let decode (tree: CodeTree)(bits: List<bit>) : List<char> = 
    let rec loop (internalTree: CodeTree)(bits: List<bit>)(acc: List<char>) : List<char> =
            match internalTree with
                | Leaf (c,_) when List.length bits = 0 ->  acc @ [c]
                | Leaf (c,_) when List.length bits > 0 -> let acc = acc @ [c]
                                                          loop tree bits acc
                | Fork (l,r,_,_) when List.length bits > 0 -> if bits.Head = 0 then loop l bits.Tail acc else loop r bits.Tail acc
                | _ -> acc  
    loop tree bits []


let sampleTree = makeCodeTree (makeCodeTree ( Leaf ('x', 1) ) (Leaf ('e', 1))) (Leaf ('t', 2))
let wordArray = toCharList "asdasdiahsodinaosidnasiod"
let tree1 = createCodeTree wordArray
let wordArray2 = toCharList "hasdasdas"
encode tree1 wordArray2
let encoded = quickEncode tree1 wordArray2
decode tree1 encoded