//General stuff like discriminated unions and pattern matching

type Suit =
    | Spades
    | Hearts
    | Diamonds
    | Clubes

type Rank =
    | King
    | Queen
    | Jack
    | Ace
    | Value of int

type Card = Card of Rank * Suit

let FullDeck = 
    [
        for suit in [Spades; Hearts; Diamonds; Clubes] do
            for value in [2 .. 10] do
                yield Card (Value(value), suit)
            for face in [King; Queen; Jack; Ace] do 
                yield Card (face, suit)
    ]

FullDeck |> Seq.toList |> Seq.length

let CardValue card = 
    let (Card(rank, _)) = card
    match rank with
    | King | Queen | Jack -> 10
    | Ace                 -> 11
    | Value x             -> x

let cardValueDict =
    dict [
        for card in FullDeck do
            yield card, CardValue card
    ]

cardValueDict.[Card(King, Spades)]

let hand = [Card(King, Hearts); Card(Value 3, Spades)]

let HandTotal hand =
    hand
    |> Seq.map (fun card -> CardValue card)
    |> Seq.sum

HandTotal hand

type HandType = 
    |Busted
    |BlackJack
    |Hand of int

let HandType hand =
    let total = HandTotal hand
    if total > 21 then Busted
    else if total = 21 then BlackJack
    else Hand total


let judgeHand hand =
    let total = HandTotal hand
    match total with
    | n when n > 21 -> Busted
    | 21            -> BlackJack
    | n             -> Hand n

HandType hand
judgeHand hand

let hand2 = [Card(King, Hearts); Card(Ace, Spades)]

HandType hand2
judgeHand hand2

let hand3 = [Card(Ace, Hearts); Card(Ace, Spades)]

HandType hand3
judgeHand hand3



//------------------------------------------------------------------------------------------------------

let numbers = [1..100]
let OddNumber x = x%2 = 1
let square x = x*x

List.filter OddNumber numbers

List.map square (List.filter OddNumber numbers)

numbers |> List.filter OddNumber |> List.map square


type Point = int * int
type Shape =
  | Rectangle of Point * Point
  | Ellipse of Point * Point
  | Composed of Shape * Shape

let figure = Rectangle ((0,0),(2,2))
let figure2 = Ellipse ((0,1),(3,5))
let figure3 = Composed (figure,figure2)
let figure4 = Composed (figure3,figure3)


open System
open System.Numerics

let figuresTest figure =
    match figure with
        | Rectangle (pt1, pt2) -> String.Format("Rectangle From {0} To {1}",pt1,pt2)
        | Ellipse (pt1, pt2) -> String.Format("Ellipse with boundingBox from {0} To {1}",pt1,pt2)
        | Composed (Composed(_, _), Composed(_,_))  -> String.Format("Composed figure of composed figures")
        | Composed (fig1, fig2) -> String.Format("Composed figure by {0} and {1}",fig1,fig2)

figuresTest figure4


let pop stack =
    match stack with
        | [] -> None
        | head::tail -> Some(head,tail)

let mystack = [1 .. 2 .. 10]
let mystack2 = []

let (value,newstack) = (pop mystack).Value
let newstack2 = 5::newstack
let (value2,newstack3) = (pop newstack2).Value



let GenerateTuples (n:int) =
    [for i in 1..n do for j in 1..n do if i <> j then yield (i,j)]

GenerateTuples 5

(*
type IntSet() =
    abstract member incl: int -> IntSet
    abstract member contains: int -> bool

type NonEmpty (elem: int, left: IntSet, right: IntSet)  =
    inherit IntSet()
    override __.incl (x: int): IntSet = 
        if x < elem then NonEmpty (elem, (left incl x), right) 
        elif x > elem then NonEmpty (elem, left, (right incl x))
        else __

    override __.contains (x: int): bool =
        if x < elem then left contains x

type Empty =
    interface IntSet with    
        member __.contains (x: int): bool = 
        member __.incl (x: int): IntSet = NonEmpty x Empty Empty

*)



let sqrt(x: double) = 
    let isGoodEnough (guess:double) =
        abs(guess * guess - x ) / x < 0.00001

    let improve (guess: double)  =
        (guess + x / guess)/2.0

    let rec sqrtIter (guess: double) : double =
        if isGoodEnough guess then guess else improve guess |> sqrtIter 

    sqrtIter 1.0 
    
sqrt(2.0)
sqrt(4.0)
sqrt(1e-6)
sqrt(1e60)

let rec fact (n:int) =
    let rec loop (n:int) (acc: bigint) =
        if n = 1 then acc else loop (n-1) (acc* bigint n)
    loop n (bigint 1)

fact 60

type Expr =
    |Number of int
    |Sum of Expr * Expr
    |Prod of Expr * Expr
    |Var of string

(*let rec eval(e: Expr): int = 
    match e with 
        | Number n -> n
        | Sum (e1,e2) -> eval e1 + eval e2
        | Prod (e1,e2) -> eval e1 * eval e2
        | 
        *)

let rec show (e:Expr) : string =
    let parentize (e: Expr) : string =
        match e with 
            | Var v -> v
            | Number n -> n.ToString()
            | _ -> "(" + show e + ")"

    match e with    
        | Number n -> n.ToString()
        | Sum (e1, e2) -> show e1 + " + "+ show e2
        | Prod (e1,e2) -> parentize e1 + " * " + parentize e2
        | Var v -> v


 
let sum1 = Prod(Sum(Number 2, Var("x")), Var("y"))
//eval sum1
show sum1

let rec times (chars: List<char>) =        
        (chars |> List.filter (fun x -> x = chars.Head)  |> List.length )

times ['a';'a';'b']


let split3 s = List.fold (fun (xs, ys) e -> (e::ys, xs))  ([], []) s
let l = [1..10]
split3 l

List.collect (fun x -> x) [[2;3;2;5];[3]]
List.concat [[2;3;2;5];[3]]

let pairs = [(2, 1); (3, 2); (4, 1); (4, 3); (5, 2); (6, 1); (6, 5)]

seq {
    for p in pairs do  yield snd p
 }