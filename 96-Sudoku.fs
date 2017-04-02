namespace ProjectEuler

open System.IO
open Shared

module Sudoku = 
    [<Literal>]
    let NonetSize = 3
    
    [<Literal>]
    let BoardSize = 3
    
    /// Converts a string into a list of characters.
    let explode (s : string) = 
        [ for c in s -> c ]
    
    /// Converts a list of characters into a string.
    let implode (xs : char list) = 
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

    type CellValue =
        | ValidValue of int
        | NoValue

    let printCellValue x = 
        match x with 
        | ValidValue v -> v.ToString()
        | NoValue -> "_"

    let isValidValue x = 
        match x with 
        | ValidValue v -> true
        | NoValue -> false

    [<StructuredFormatDisplay("({col}, {row}) => {Display}")>]
    type Cell = 
        { row : int
          col : int
          value : CellValue }
        
        member m.Display = printCellValue m.value
    
    [<StructuredFormatDisplay("{cells}")>]
    type Nonet = 
        { cells : Cell list
          rows : Cell list list
          cols : Cell list list }
    
    [<StructuredFormatDisplay("{cell}")>]
    type CellInNonet = 
        { cell : Cell
          nonet : Nonet }
    
    [<StructuredFormatDisplay("{nonet}")>]
    type NonetInBoard = 
        { nonet : Nonet
          left : Nonet option
          top : Nonet option
          right : Nonet option
          down : Nonet option }
    
    type Line = CellInNonet list

    type CellWithCandidates =
        { cell : Cell
          candidates : int list }
    
    [<StructuredFormatDisplay("{name} {nonets}")>]
    type Board = 
        { name : string
          nonets : Nonet list
          horizontalLines : Line list
          verticalLines : Line list }
    
    let parseNonetFromRaw nonetLines horizontalPos =
        let parseNonetValue x =
            match x with
            | 0 -> NoValue
            | _ -> ValidValue x
     
        let parseNonetLine r chars = 
            chars |> List.mapi (fun c x -> 
                         { row = r
                           col = c
                           value = System.Int32.Parse(x.ToString()) |> parseNonetValue  })
        
        // 3 lines with other nonets in it
        let vals = 
            nonetLines
            |> List.take NonetSize
            |> List.mapi (fun i x -> 
                   (explode x
                    |> List.skip (horizontalPos * NonetSize)
                    |> List.take NonetSize
                    |> parseNonetLine i))
            |> List.concat
        
        let r = vals |> List.chunkBySize NonetSize
        { cells = vals; rows = r; cols = r |> transpose }
    
    let attachCellsToNonets f xs = 
        xs |> List.map (fun nonet -> 
                  nonet
                  |> f
                  |> List.map (fun line -> line |> List.map (fun cell -> { cell = cell; nonet = nonet })))
    
    let parseFile rawLines = 
        let parseBoard (name :: rawNonetLines : string list) = 
            let getHorizontalLines xs = 
                xs
                |> attachCellsToNonets (fun nonet -> nonet.rows)
                |> List.chunkBySize (BoardSize)
                |> List.collect (transpose)
                |> List.map (fun xs -> xs |> List.concat)
            
            let getVerticalLines xs = 
                xs
                |> attachCellsToNonets (fun nonet -> nonet.cols)
                |> List.concat
                |> List.chunkBySize (NonetSize)
                |> List.chunkBySize (BoardSize)
                |> transpose
                |> List.collect transpose
                |> List.map List.concat
            
            //printfn "Board name %A" name
            //printfn "Raw lines %A" rawNonetLines
            let nonets = 
                rawNonetLines
                |> List.chunkBySize NonetSize
                |> List.collect (fun chunk -> [ 0..(NonetSize - 1) ] |> List.map (parseNonetFromRaw chunk))
            
            printfn "Nonets %A" nonets.Length
            let board = 
                { name = name
                  nonets = nonets
                  horizontalLines = (nonets |> getHorizontalLines)
                  verticalLines = (nonets |> getVerticalLines) }
            board

        rawLines   
        |> List.chunkBySize (1 + NonetSize * BoardSize)
        |> List.take 1
        |> List.map parseBoard
        

    let fillInCandidates board =
        let getCandidates (cells : Line) =
            let candidates = cells 
                             |> List.choose (fun x -> match x.cell.value with 
                                                      | ValidValue v -> Some v
                                                      | NoValue -> None)
            [1..9] |> List.except candidates

        let getAllCandidates (row : Line) rowIndex col colIndex =
            let cell = (List.item colIndex row).cell
            let candidates = match cell.value with
                             | NoValue -> (getCandidates row) |> List.except (getCandidates col)
                             | ValidValue v -> []

            let cellWithCandidate = { cell = cell
                                      candidates = candidates }
            cellWithCandidate
        
        let findCandidates rows cols = 
            rows
            |> List.mapi (fun i row -> cols |> List.mapi (fun j col -> getAllCandidates row i col j))
        
        let assignDefiniteCandidates candidates =
            let assign cell =
                match cell.candidates with
                | [x] -> { cell = { row = cell.cell.row; col = cell.cell.col; value = ValidValue x }; candidates = [] }
                | _ -> cell

            candidates 
            |> List.map (fun row -> row |> List.map (fun cell -> assign cell))

        let hasAssignable (c : CellWithCandidates list list) = 
            let onlyOneCandidate = c |> List.concat |> List.filter (fun cell -> cell.candidates.Length = 1)
            onlyOneCandidate.Length > 1
        
        let candidates = findCandidates board.horizontalLines board.verticalLines

        let assignable = hasAssignable candidates
        printfn "Has assignable: %A" assignable
        
        let assigned = candidates |> assignDefiniteCandidates
        assigned

    let answer = 
        let filePath = "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p096_sudoku.txt"
        let rawLines = System.IO.File.ReadLines(filePath)
        let board = parseFile (rawLines |> List.ofSeq) |> List.head
        printfn "Horizontal lines"
        board.horizontalLines |> printLines
        printfn "Vertical lines"
        board.verticalLines |> printLines

        let candidatesFilled = fillInCandidates board
        candidatesFilled |> printLines
