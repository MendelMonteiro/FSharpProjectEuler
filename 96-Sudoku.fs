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

    let rec transpose matrix =
        match matrix with
        | (_::_)::_ -> List.map List.head matrix :: transpose(List.map List.tail matrix)
        | _ -> []

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
            let getHorizontalLinesForRow nonetRow =
                nonetRow |> List.map (List.chunkBySize NonetSize) |> transpose |> List.map List.concat

            let getHorizontalLines xs =
                xs
                |> List.map (fun x -> x.cells |> List.map (fun y -> {cell = y; nonet = x}))
                |> List.chunkBySize NonetSize
                |> getHorizontalLinesForRow
                |> List.concat

            let nonets = [0..2] |> List.map (fun x -> nonetLines |> parseNonetFromRaw x)

            let board = {name = name; nonets = nonets; horizontalLines = (nonets |> getHorizontalLines); verticalLines = []}
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
        