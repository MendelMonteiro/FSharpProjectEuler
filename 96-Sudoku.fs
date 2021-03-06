﻿namespace ProjectEuler

open System.IO
open Shared

module Sudoku = 
    [<Literal>]
    let NonetSize = 3
    
    [<Literal>]
    let BoardSize = 3
    
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

    [<StructuredFormatDisplay("({col}, {row})")>]
    type Position = { row : int; col : int }
    
    [<StructuredFormatDisplay("({col}, {row}) => {Display}")>]
    type Cell = 
        { row : int
          col : int
          absRow : int
          absCol : int
          value : CellValue }
        member m.Display = printCellValue m.value
    
    [<StructuredFormatDisplay("{cells}")>]
    type Nonet = 
        { num : int
          cells : Cell list
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
    
    type Line = Cell list

    type CellWithCandidates =
        { cell : Cell
          candidates : int list }
    
    [<StructuredFormatDisplay("{AsString}")>]
    type Board = 
        { name : string
          nonets : Nonet list
          horizontalLines : Line list
          verticalLines : Line list } with

        member this.print =
            let printCell c = (printCellValue c.value) 
            let wrap s = "|" + s + "|"
            printfn "%A" this.name
            let printNonetRow boardRow nonetRow = 
                (this.nonets
                |> List.skip (boardRow * BoardSize)
                |> List.take BoardSize
                |> List.map (fun n -> wrap ( n.cells |> List.skip (nonetRow * NonetSize) |> List.take NonetSize |> List.map printCell |> String.concat "|" ) ) |> String.concat " ")
            let printBoardRow = List.map (fun boardRow -> [0..2] |> List.map (fun nonetRow -> printNonetRow boardRow nonetRow))
            let output = ( [0..2] |> printBoardRow ) |> List.map (fun x -> x |> String.concat System.Environment.NewLine) |> String.concat (System.Environment.NewLine + System.Environment.NewLine)
            output

        member m.AsString = m.print
    
    // Start functions
    let parseNonetFromRaw verticalPos nonetLines horizontalPos =
        let parseNonetValue x =
            match x with
            | 0 -> NoValue
            | _ -> ValidValue x
     
        let offset = horizontalPos * NonetSize
        let parseNonetLine r chars = 
            chars |> List.mapi (fun c x -> 
                         { row = r
                           col = c
                           absRow = verticalPos * NonetSize + r 
                           absCol = offset + c
                           value = System.Int32.Parse(x.ToString()) |> parseNonetValue  })
        
        // 3 lines with other nonets in it
        let vals = 
            nonetLines
            |> List.take NonetSize
            |> List.mapi (fun i x -> 
                   (explode x
                    |> List.skip offset
                    |> List.take NonetSize
                    |> parseNonetLine i))
            |> List.concat
        
        let rowCells = vals |> List.chunkBySize NonetSize
        { num = horizontalPos; cells = vals; rows = rowCells; cols = rowCells |> transpose }
    
    let extractCellsFrom getCells xs = 
        xs |> List.map (fun nonet -> nonet |> getCells |> List.map (fun line -> line |> List.map (fun cell -> cell)))
    
    // Convert all the rows in the nonets to a list of rows
    let getHorizontalLines xs = 
        xs
        |> extractCellsFrom (fun nonet -> nonet.rows)
        |> List.chunkBySize (BoardSize)
        |> List.collect (transpose)
        |> List.map (fun xs -> xs |> List.concat)
        
    // Convert all the columns in the nonets to a list of columns
    let getVerticalLines xs = 
        xs
        |> extractCellsFrom (fun nonet -> nonet.cols)
        |> List.concat
        |> List.chunkBySize (NonetSize)
        |> List.chunkBySize (BoardSize)
        |> transpose
        |> List.collect transpose
        |> List.map List.concat

    // Transform the file into nonets
    let parseFile rawLines = 
        let parseBoard (name :: rawNonetLines : string list) = 
            
            let nonets = 
                rawNonetLines
                |> List.chunkBySize NonetSize
                |> List.mapi (fun i chunk -> [ 0..(NonetSize - 1) ] |> List.map (parseNonetFromRaw i chunk))
                |> List.concat
            
            let board = 
                { name = name
                  nonets = nonets
                  horizontalLines = (nonets |> getHorizontalLines)
                  verticalLines = (nonets |> getVerticalLines) }
            board

        rawLines   
        |> List.chunkBySize (1 + NonetSize * BoardSize)
        //|> List.take 1
        |> List.map parseBoard
        
    // Return either valid cell values or None
    let cellValues cells =
        cells |> List.choose (fun x -> match x.value with 
                                       | ValidValue v -> Some v
                                       | NoValue -> None)

    // Validate that there are no duplicate numbers in rows or columns
    let validateBoard board =
        let horizontalDuplicates = board.horizontalLines |> List.map (fun xs -> xs |> getDuplicates |> List.length)
        let verticalDuplicates = board.verticalLines |> List.map (fun xs -> xs |> getDuplicates |> List.length)
        horizontalDuplicates |> List.forall ((=) 0) && verticalDuplicates |> List.forall ((=) 0) 

    // Fill in candidates method
    let rec fillInCandidates board =
        let candidatesInLine cells =
            let existing = cells |> cellValues
            let ret = [1..9] |> List.except existing
            ret
        
        // Find all candidates that don't already exist in the given row or column
        let intersectionCandidates row col existingInNonet =
            let rowCandidates = candidatesInLine row
            let colCandidates = candidatesInLine col
            let candidateSet = Set.ofList rowCandidates |> Set.intersect (Set.ofList colCandidates)
            let withoutExisting = candidateSet - existingInNonet
            //printfn "%A" col
            //printfn "%A" colCandidates
            withoutExisting

        // Find all candidates that don't already exist in the cell's row or column
        let intersectionCandidatesForCell (rows : Line list) (cols : Line list) (existingInNonet : Set<int>) (cell : Cell) =
            let row = rows |> List.item cell.absRow
            let col = cols |> List.item cell.absCol
            (cell, intersectionCandidates row col existingInNonet)

        let isEmptyCell x = match x.value with | NoValue -> true | ValidValue _ -> false
        let isNotEmptyCell x = not (isEmptyCell x)

        // Find candidates for all cells in a nonet
        let findCandidatesInNonet rows cols (nonet : Nonet) = 
            //printfn "Nonet #%A" nonet.num
            let existingCellValues = nonet.cells |> List.filter isNotEmptyCell |> cellValues |> Set.ofList
            let cellsWithCandidates = nonet.cells |> List.filter isEmptyCell |> List.map (intersectionCandidatesForCell rows cols existingCellValues) 
             
            let candidatesOnlyValidInOneCell = 
                cellsWithCandidates 
                    |> List.map (fun (_, candidates) -> candidates) 
                    |> Seq.concat 
                    |> Seq.countBy (fun x -> x) 
                    |> Seq.filter (fun (_, count) -> count = 1)

            let findFirstCellForCandidate (x, _) = 
                let matchingCell = cellsWithCandidates |> Seq.find (fun (_, candidates) -> candidates |> Set.contains x)
                (fst matchingCell, x)
            candidatesOnlyValidInOneCell |> Seq.map findFirstCellForCandidate
        
        // Find all candidates and convert output to cells with candidates
        let toCellsWithCandidates nonet rows cols = 
            let blankCellsWithCandidates = nonet |> (findCandidatesInNonet rows cols)
            let tryGetCandidates c = 
                let matching = blankCellsWithCandidates |> Seq.tryFind (fun (cell, _) -> cell = c) 
                match matching with
                | Some (_, v) -> [v]
                | None -> []
            nonet.cells |> List.map (fun c -> {cell = c; candidates = (tryGetCandidates c)})

        let findCandidates nonet = toCellsWithCandidates nonet board.horizontalLines board.verticalLines 

        // Convert cells to a nonet
        let cellsToNonet num nonet = 
            let rowsFromNonet nonet = nonet |> List.chunkBySize NonetSize
            let colsFromNonet nonet = nonet |> rowsFromNonet |> transpose
            { num = num; cells = nonet; rows = nonet |> rowsFromNonet; cols = nonet |> colsFromNonet }

        // If there is only one candidate left in this cell, assign it to the cell value
        let assign cell =
                match cell.candidates with
                | [x] -> { cell = { row = cell.cell.row; col = cell.cell.col; absRow = cell.cell.absRow; absCol = cell.cell.absCol; value = ValidValue x }; candidates = [] }
                | _ -> cell
        
        let ignoreCandidates x = x |> List.map (fun y -> y.cell)

        // Find and assign the candidates in the given nonet
        let findAndAssignCandidates nonet = 
            let candidates = findCandidates nonet 
            let hasAssignable = candidates |> List.exists (fun c -> c.candidates.Length = 1)
            let assigned = candidates |> List.map assign |> ignoreCandidates
            (hasAssignable, assigned)

        // Replace a nonet in the given board
        let replaceNonetIn board nonetIndex nonet = 
            let switchAt i x j y = if i = j then x else y
            let nonets = (board.nonets |> List.mapi (switchAt nonetIndex nonet))
            { name = board.name;
              nonets = nonets;
              horizontalLines = board.nonets |> getHorizontalLines
              verticalLines = board.nonets |> getVerticalLines }

        // Recursively assign all viable candidates and return the new board
        let rec assignAndReplaceBoard board nonetIndex assignedCandidates =
            let currentNonet = board.nonets |> List.skip nonetIndex |> List.tryHead
            match currentNonet with
            | Some n -> let (hasAssignable, assigned) = findAndAssignCandidates n
                        let newBoard =  assigned
                                        |> cellsToNonet n.num
                                        |> replaceNonetIn board nonetIndex
                        assignAndReplaceBoard newBoard (nonetIndex + 1) (assignedCandidates || hasAssignable)
            | _ -> (assignedCandidates, board)
            
        let (assignedCandidates, newBoard) = assignAndReplaceBoard board 0 false
        if not (validateBoard newBoard) then 
            failwith "Board is not consistent"
        else
            if assignedCandidates then
                fillInCandidates newBoard
            else
                newBoard

    // Find the answer
    let answer = 
        let filePath = "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p096_sudoku.txt"
        let rawLines = System.IO.File.ReadLines(filePath)
        parseFile (rawLines |> List.ofSeq) |> List.map (fun board -> 
            printfn "%A" board

            //printfn "\r\nFilled in candidates\r\n"
            let candidatesFilled = fillInCandidates board
            let isValid = validateBoard candidatesFilled
            printfn "IsValid: %A" isValid
            printfn "%A" candidatesFilled
            )

        
