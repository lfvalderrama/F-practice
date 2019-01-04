open System
open System.IO

let readFile path =
    let text = File.ReadAllText(path)
    text.Split('\n') 

let result = readFile "C:\Users\lvalderrama\Documents\TestF\F-practice\TestF\linuxwords.txt"

(* -------------------------------------------------------------------------------------------- *)

type Word = String
type Sentence = List<Word>
type Ocurrences = List<(Char*int)>

let dictionary : List<Word> =  List.ofArray result

let wordOcurrences (word: Word) : Ocurrences = Seq.toList word |> List.groupBy (fun c -> c) |> List.map (fun (k, v) -> (k,v.Length)) |> List.sort 

let sentenceOcurrences (s: Sentence) : Ocurrences = s |> String.concat "" |> wordOcurrences

let dictionaryByOcurrencesLazy : Lazy<Map<Ocurrences, List<Word>>> = lazy (
                                                                seq { for word in dictionary do yield ((wordOcurrences word), word)} 
                                                                |> Seq.toList 
                                                                |> List.groupBy (fun (list, _) -> list) 
                                                                |> List.map (fun (k, v) -> (k, List.fold (fun acc (_, w) -> w :: acc) List.empty v)) 
                                                                |> Map
                                                            )

let dictionaryByOcurrences = dictionaryByOcurrencesLazy.Force()

let wordAnagrams(word: Word) : List<Word> = 
    match dictionaryByOcurrences.TryFind(wordOcurrences word) with
    | Some x -> x
    | None -> List.empty

let rec combinations (ocurrences: Ocurrences) : List<Ocurrences> =
    match ocurrences with 
    | [] -> [[]]
    | head::tail -> let occs = seq {for value in combinations tail do 
                                        for j in 1..snd head do 
                                            yield (fst head, j) :: value
                                    } |> Seq.toList
                    occs @ combinations tail

let substract (x: Ocurrences) (y: Ocurrences) : Ocurrences =
    List.map (fun (cx, ix) -> match List.tryFind (fun (cy, _) -> cy = cx) y with
                                                    | Some (_, iy) -> (cx, ix - iy)
                                                    | None -> (cx, ix)
                                                 ) x |> List.filter (fun (_,i) -> i > 0)

let wordsInDict (occ: Ocurrences): Sentence =
    match dictionaryByOcurrences.TryFind occ with
                                        | Some lword -> lword
                                        | None -> []

let sentenceAnagrams(sentence: Sentence): List<Sentence> =
    let rec findWords(occ: Ocurrences): List<Sentence> = 
        match occ with 
            | [] -> []
            | _ -> seq {for comb in combinations occ do
                            for word in wordsInDict comb do 
                                for anagram in findWords(substract occ comb) do 
                                    yield word :: anagram  
                                yield word :: []
                        } |> Seq.toList  


    findWords (sentenceOcurrences sentence)
(*
wordAnagrams "playerasd"
sentenceOcurrences ["Hello"; "my"; "name"; "is"]
(combinations [('a', 2); ('b', 2)]).Length

let x = [('a', 1); ('d', 1); ('l', 4); ('r', 1)]
let y = [('r', 1)]

substract x y*)

sentenceAnagrams ["linux"]


