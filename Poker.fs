﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

//#load "Library1.fs"
//open ProjectEuler

// Define your library scripting code here

type Suit = Hearts | Clubs | Spades | Diamonds
type FaceCardType = Jack = 11 | Queen = 12 | King = 13 | Ace = 14
type HandType = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush

type CardValue = 
    | NumberCard of int
    | FaceCard of FaceCardType

type Card = CardValue * Suit

type Hand = HandType * CardValue
type Result = Win | Lose | Draw

let Problem54 =
    let matchCard x : CardValue = 
        match x with 
        | 'J' -> FaceCard FaceCardType.Jack
        | 'Q' -> FaceCard FaceCardType.Queen
        | 'K' -> FaceCard FaceCardType.King
        | 'A' -> FaceCard FaceCardType.Ace
        | 'T' -> NumberCard 10
        | c -> (NumberCard(System.Int32.Parse(c.ToString())))

    let matchSuit x = 
        match x with
        | 'C' -> Some Clubs
        | 'D' -> Some Diamonds
        | 'H' -> Some Hearts
        | 'S' -> Some Spades
        | _ -> None

    let isFaceCard x =
        match x with
        | (FaceCard _, s) -> true
        | (NumberCard _, s) -> false

    let cardValue x = 
        match x with
        | (FaceCard n, s) -> (int)n
        | (NumberCard n, s) -> n

    let tryGetHand sorted matches handType =
        let matches = sorted |> List.distinctBy snd |> List.length = 1
        match matches with
        | true -> Some (handType, List.last sorted |> fst)
        | _ -> None

    let isFlush xs =
        let sorted = xs |> List.sortBy cardValue
        let matches = sorted |> List.distinctBy snd |> List.length = 1
        tryGetHand sorted matches Flush

    let isStraight xs : Hand option = 
        let sorted = xs |> List.sortBy cardValue
        let matches = (List.last sorted |> cardValue) - (List.head sorted |> cardValue) = List.length xs - 1
        tryGetHand sorted matches Straight
        
    let valueIsNotRepeated xs = xs |> List.distinctBy cardValue |> List.length = (xs |> List.length)

    let isRoyalFlush xs : Hand option =
        let isTen x = match x with | (NumberCard n, _) -> n = 10 | _ -> false
        let isRoyal x = isFaceCard x || isTen x
        let matches = xs |> isFlush |> Option.isSome && xs |> List.forall isRoyal && xs |> valueIsNotRepeated
        match matches with 
        | true -> Some (RoyalFlush, FaceCard FaceCardType.Ace)
        | _ -> None

    let isStraightFlush xs = 
        let straight = xs |> isStraight
        let flush = xs |> isFlush
        match (straight, flush) with
        | (Some s, Some f) -> straight
        | _ -> None

    let groupedMultiples xs = xs |> List.groupBy (fun (x:Card) -> x) |> List.sortByDescending (fun (x, y) -> List.length y)

    let isMultipleCard xs n = 
        let mostMultiples = xs |> groupedMultiples |> List.head
        match mostMultiples |> snd |> List.length = n with
        | true -> Some (mostMultiples |> fst)
        | _ -> None

    let hasNElements n xs = xs |> List.length = n
    let isPairNTimes n xs = 
        let pairs = xs |> List.groupBy cardValue |> List.where (fun (_, x) -> x |> hasNElements 2)
        match pairs with
        | l when l |> List.length = n -> Some (l |> List.sortBy fst |> List.last |> snd |> List.head |> fst)
        | _ -> None

    let isPair xs = match xs |> isPairNTimes 1 with Some x -> Some (Pair, x) | _ -> None
    let isTwoPair xs = match xs |> isPairNTimes 2 with Some x -> Some (TwoPair, x) | _ -> None
    let isThreeOfAKind xs = match isMultipleCard xs 3 with Some x -> Some (ThreeOfAKind, fst x) | _ -> None
    let isFourOfAKind xs = match isMultipleCard xs 4 with Some x -> Some (FourOfAKind, fst x) | _ -> None

    let isFullHouse xs = 
        let pair = xs |> isPair
        let threeOfAKind = xs |> isThreeOfAKind
        match (pair, threeOfAKind) with
        | (Some p, Some t) -> threeOfAKind
        | _ -> None

    let highestCard xs = 
        //printfn "Highest"
        Some (HighCard, xs |> List.sortBy cardValue |> List.last |> fst)

    let handSolvers' = [
        (RoyalFlush, isRoyalFlush)
        (StraightFlush, isStraightFlush)
        (FourOfAKind, isFourOfAKind)
        (FullHouse, isFullHouse)
        (Flush, isFlush)
        (Straight, isStraight)
        (ThreeOfAKind, isThreeOfAKind)
        (TwoPair, isTwoPair)
        (Pair, isPair)
        (HighCard, highestCard) ]

    let handSolvers = handSolvers' |> List.sortByDescending fst    
   
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let lines = readLines "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p054_poker.txt"
    let codeToCard (x:string) : Card option = 
        let cardValue = x.[0] |> matchCard
        let suit = x.[1] |> matchSuit
        match suit with
        | Some s -> Some (cardValue, s)
        | _ -> None

    let extractTwoHands n (x:string) = 
        let xs = x.Split ' '
        (xs |> Seq.take n |> Seq.map codeToCard, xs |> Seq.skip n |> Seq.take n |> Seq.map codeToCard)
    
    let extractTwoHandsOf5 (x:string) = extractTwoHands 5 x        

    let isValid xs = xs |> Seq.forall (fun x -> Option.isSome x)
    let onlyValids xs = xs |> Seq.map Option.get
    let onlyValidCards (xs, ys) = if xs |> isValid && ys |> isValid then Some (onlyValids xs, onlyValids ys) else None
    let targetLines = lines |> Seq.skip 60 |> Seq.take 1
    let hands = targetLines |> Seq.map extractTwoHandsOf5 |> Seq.map onlyValidCards |> Seq.map Option.get
 
    let playerOneHands =
        let findHand (xs:Card list) = handSolvers |> List.map (fun (_, f) -> f xs) |> List.filter Option.isSome |> List.head
        let playerOne (x, y) = findHand x
        let playerTwo (x, y) = findHand y

        let getResult (x:Hand option) (y:Hand option) = 
            match (x, y) with 
            | (None, None) -> Draw
            | (None, _) -> Lose
            | (_, None) -> Win
            | (Some (h, c), Some(yh, yc)) when h = yh -> match c with | c when c = yc -> Draw
                                                                      | c when c > yc -> Win
                                                                      | c when c < yc -> Lose
                                                                      | _ -> Lose
            | (Some (h, c), Some(yh, yc)) when h > yh -> Win
            | (Some (h, c), Some(yh, yc)) when h < yh -> Lose
            | _ -> Lose

        let getResultWithHand (x, y) =
            let playerOneHand = x |> List.ofSeq |> findHand
            let playerTwoHand = y |> List.ofSeq |> findHand
            (playerOneHand, playerTwoHand, getResult playerOneHand playerTwoHand)
            
//        let test = "3D AD 3C 3S 4C QC AS 5D TH 8C"
//        printfn "%A"  (test |> extractTwoHandsOf5 |> fst |> List.ofSeq |> List.map Option.get |> findHand)

        hands |> Seq.map getResultWithHand //|> Seq.filter (fun x -> x = Win)

    let lineList = lines |> Seq.toList
    playerOneHands |> Seq.zip lineList |> Seq.iter (printfn "%A")

printfn "%A" Problem54

//    let royalFlush = [(NumberCard 10, Clubs); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
//    let straightFlush = [(NumberCard 9, Clubs); (NumberCard 10, Clubs); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs)]
//    let straight = [(NumberCard 10, Hearts); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
//    let flush = [(NumberCard 9, Clubs); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
//    let threeOfAKind = [(NumberCard 9, Clubs); (NumberCard 9, Spades); (NumberCard 9, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
//    let twoPairs = [(NumberCard 9, Clubs); (NumberCard 9, Spades); (FaceCard FaceCardType.King, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
//    let pair = [(NumberCard 9, Clubs); (NumberCard 9, Spades); (FaceCard FaceCardType.Jack, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
//    let highCard = [(NumberCard 9, Clubs); (NumberCard 1, Spades); (FaceCard FaceCardType.Jack, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]

