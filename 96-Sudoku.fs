namespace ProjectEuler

open System.IO

module Sudoku =

    [<Literal>] 
    let NonetSize = 3

    [<Literal>]
    let BoardSize = 3

    /// Converts a string into a list of characters.
    let explode (s:string) =
        [for c in s -> c]

    /// Converts a list of characters into a string.
    let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()
    
    type Cell = {row:int; col:int; value:int}

    type Nonet = {cells : Cell list}

    type CellInNonet = {cell : Cell; nonet : Nonet}

    type NonetInBoard = {nonet : Nonet; left : Nonet option ; top : Nonet option; right : Nonet option; down : Nonet option}

    type Line = CellInNonet list

    type Board = {name : string; nonets : Nonet list; horizontalLines : Line list; verticalLines : Line list}

    let parseFile rawLines =
        let parseNonetFromRaw horizontalPos nonetLines =
            let parseNonetLine r chars = 
                chars 
                |> List.mapi (fun c x -> { row = r; col = c; value = System.Int32.Parse(x.ToString())})

            // 3 lines with other nonets in it
            let vals = nonetLines 
                       |> List.take NonetSize
                       |> List.mapi (fun i x -> (explode x |> List.skip (horizontalPos * NonetSize) |> List.take NonetSize |> parseNonetLine i))                       
                       |> List.concat            

            { cells = vals }

        let parseBoard (name :: nonetLines : string list) =
            let nonet1 = nonetLines |> parseNonetFromRaw 0
            let nonet2 = nonetLines |> parseNonetFromRaw 1
            let nonet3 = nonetLines |> parseNonetFromRaw 2
            
            let board = {name = name; nonets = [nonet1; nonet2; nonet3]; horizontalLines = []; verticalLines = []}
            board

        rawLines 
        |> List.chunkBySize (1 + NonetSize * BoardSize)
        |> List.take 1
        |> List.map parseBoard

    let answer = 
        let filePath = "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p096_sudoku.txt"
        let rawLines = System.IO.File.ReadLines(filePath)

        let board = parseFile (rawLines |> List.ofSeq) |> List.head
        board.nonets |> List.iter (printfn "%A")
        