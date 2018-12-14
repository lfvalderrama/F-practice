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