// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
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

type Card = (CardValue * Suit)

type Hand = (HandType * Card)
type Result = Win | Lose | Draw

let Problem54 =
    let matchCard x : CardValue = 
        match x with 
        | 'J' -> FaceCard FaceCardType.Jack
        | 'Q' -> FaceCard FaceCardType.Queen
        | 'K' -> FaceCard FaceCardType.King
        | 'A' -> FaceCard FaceCardType.Ace
        | c -> (NumberCard(System.Int32.Parse(c.ToString())))

    let matchSuit x = 
        match x with
        | 'C' -> Clubs
        | 'D' -> Diamonds
        | 'H' -> Hearts
        | 'S' -> Spades

    let isFaceCard x =
        match x with
        | (FaceCard _, s) -> true
        | (NumberCard _, s) -> false

    let cardValue x = 
        match x with
        | (FaceCard n, s) -> (int)n
        | (NumberCard n, s) -> n

    let isFlush xs = xs |> List.distinctBy snd |> List.length = 1

    let isStraight xs = 
        let sorted = xs |> List.map cardValue |> List.sort
        List.last sorted - List.head sorted = List.length xs - 1

    let valueIsNotRepeated xs = xs |> List.distinctBy cardValue |> List.length = (xs |> List.length)

    let isRoyalFlush xs =
        let isTen x = match x with | (NumberCard n, _) -> n = 10 | _ -> false
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
    
    let royalFlush = [(NumberCard 10, Clubs); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    let straightFlush = [(NumberCard 9, Clubs); (NumberCard 10, Clubs); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs)]
    let straight = [(NumberCard 10, Hearts); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    let flush = [(NumberCard 9, Clubs); (FaceCard FaceCardType.Jack, Clubs); (FaceCard FaceCardType.Queen, Clubs); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    let threeOfAKind = [(NumberCard 9, Clubs); (NumberCard 9, Spades); (NumberCard 9, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    let twoPairs = [(NumberCard 9, Clubs); (NumberCard 9, Spades); (FaceCard FaceCardType.King, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    let pair = [(NumberCard 9, Clubs); (NumberCard 9, Spades); (FaceCard FaceCardType.Jack, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    let highCard = [(NumberCard 9, Clubs); (NumberCard 1, Spades); (FaceCard FaceCardType.Jack, Hearts); (FaceCard FaceCardType.King, Clubs); (FaceCard FaceCardType.Ace, Clubs)]
    
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let lines = readLines "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p054_poker.txt"
    let codeToCard (x:string) : Card = (x.[0] |> matchCard, x.[1] |> matchSuit)
    let twoHands (x:string) = 
        let xs = x.Split ' '
        (xs |> Seq.take 5 |> Seq.map codeToCard, xs |> Seq.skip 5 |> Seq.take 5 |> Seq.map codeToCard)

    let hands = lines |> Seq.map twoHands

    let playerOneHands =
        // TODO: Return Hand (capture best card)
        let findHand (x:Card list) = handSolvers |> List.filter (fun (_, f) -> f x) |> List.tryHead |> Option.bind fst
        let playerOne (x, y) = findHand x
        let playerTwo (x, y) = findHand y

        let getResult (x:Hand) (yh, yc) = 
            match x with
            | (h, c) when h = yh -> match c with | c when c = yc -> Draw
                                                 | c when c > yc -> Win
                                                 | c when c < yc -> Lose
            | (h, c) when h > yh -> Win
            | (h, c) when h < yh -> Lose

        hands |> Seq.map (fun (x, y) -> getResult (x |> List.ofSeq |> findHand) (y |> List.ofSeq |> findHand))

    playerOneHands

    //let hands = [ highCard; pair; twoPairs; threeOfAKind; straight; flush; straightFlush; royalFlush ]
    //let result = hands |> List.map (fun x -> handSolvers |> List.filter (fun (_, f) -> f x) |> List.tryHead)
    //result

