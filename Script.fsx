// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

//#load "Library1.fs"
//open ProjectEuler

// Define your library scripting code here

type Suit = Hearts | Clubs | Spades | Diamonds
type FaceCardType = Jack = 11 | Queen = 12 | King = 13 | Ace = 14
type HandType = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush

type Card = 
    | NumberCard of (int * Suit)
    | FaceCard of (FaceCardType * Suit)

type Hand = (HandType * Card)

let Problem54 =

    let isFaceCard x =
        match x with
        | FaceCard (y, z) -> true
        | NumberCard (y, z) -> false

    let cardSuit x = 
        match x with
        | NumberCard (n, s) -> s
        | FaceCard (n, s) -> s

    let cardValue x = 
        match x with
        | FaceCard (n, s) -> (int)n
        | NumberCard (n, s) -> n

    let isFlush xs = xs |> List.distinctBy cardSuit |> List.length = 1

    let isStraight xs = 
        let sorted = xs |> List.map cardValue |> List.sort
        List.last sorted - List.head sorted = List.length xs - 1

    let valueIsNotRepeated xs = xs |> List.distinctBy cardValue |> List.length = (xs |> List.length)

    let isRoyalFlush xs =
        let isTen x = match x with | NumberCard (n, s) -> n = 10 | _ -> false
        let isRoyal x = isFaceCard x || isTen x
        xs |> isFlush && xs |> List.forall isRoyal && xs |> valueIsNotRepeated

    let isStraightFlush xs = xs |> isStraight && xs |> isFlush

    let groupedMultiples xs = xs |> List.groupBy (fun (x:Card) -> x |> cardValue) |> List.sortByDescending (fun (x, y) -> List.length y)

    let isMultipleCard xs n = xs |> groupedMultiples |> List.head |> snd |> List.length = n

    let hasNElements n xs = xs |> List.length = n
    let isPairNTimes n xs = xs |> List.groupBy cardValue |> List.where (fun (_, x) -> x |> hasNElements 2) |> hasNElements n
    let isPair xs = xs |> isPairNTimes 1
    let isTwoPair xs = xs |> isPairNTimes 2
    let isThreeOfAKind xs = isMultipleCard xs 3
    let isFourOfAKind xs = isMultipleCard xs 4

    let isFullHouse xs = xs |> isPair && xs |> isThreeOfAKind

    let handSolvers' = [
        (RoyalFlush, isRoyalFlush)
        (StraightFlush, isStraightFlush)
        (FourOfAKind, isFourOfAKind)
        (Straight, isStraight)
        (Flush, isFlush)
        (ThreeOfAKind, isThreeOfAKind)
        (TwoPair, isTwoPair)
        (Pair, isPair) ]

    let handSolvers = handSolvers' |> List.sortByDescending fst
    
    let royalFlush = [NumberCard (10, Clubs); FaceCard (FaceCardType.Jack, Clubs); FaceCard (FaceCardType.Queen, Clubs); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]
    let straightFlush = [NumberCard (9, Clubs); NumberCard (10, Clubs); FaceCard (FaceCardType.Jack, Clubs); FaceCard (FaceCardType.Queen, Clubs); FaceCard (FaceCardType.King, Clubs)]
    let straight = [NumberCard (10, Hearts); FaceCard (FaceCardType.Jack, Clubs); FaceCard (FaceCardType.Queen, Clubs); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]
    let flush = [NumberCard (9, Clubs); FaceCard (FaceCardType.Jack, Clubs); FaceCard (FaceCardType.Queen, Clubs); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]
    let threeOfAKind = [NumberCard (9, Clubs); NumberCard (9, Spades); NumberCard (9, Hearts); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]
    let twoPairs = [NumberCard (9, Clubs); NumberCard (9, Spades); FaceCard (FaceCardType.King, Hearts); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]
    let pair = [NumberCard (9, Clubs); NumberCard (9, Spades); FaceCard (FaceCardType.Jack, Hearts); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]
    let highCard = [NumberCard (9, Clubs); NumberCard (1, Spades); FaceCard (FaceCardType.Jack, Hearts); FaceCard (FaceCardType.King, Clubs); FaceCard (FaceCardType.Ace, Clubs)]

    let hands = [ highCard; pair; twoPairs; threeOfAKind; straight; flush; straightFlush; royalFlush ]

    let readLines filePath = System.IO.File.ReadLines(filePath)

    let lines = readLines "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p054_poker.txt"
    lines

    //let result = hands |> List.map (fun x -> handSolvers |> List.filter (fun (_, f) -> f x) |> List.tryHead)
    //result

