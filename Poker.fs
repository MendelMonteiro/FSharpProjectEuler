module Poker

type Suit = Hearts | Clubs | Spades | Diamonds
type FaceCard = Jack = 11 | Queen = 12 | King = 13 | Ace = 14
type HandType = HighCard | Pair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush

type Card = 
    | NumberCard of (int * Suit)
    | FaceCard of (FaceCard * Suit)

//type Hand = (HandType * Card)

let Problem54 =

    //let b = HighCard > Pair
    
    let c = NumberCard (1, Hearts)

    c

