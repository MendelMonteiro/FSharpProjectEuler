namespace ProjectEuler

open System.IO

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
    
    [<StructuredFormatDisplay("({col}, {row}) => {value}")>]
    type Cell = 
        { row : int
          col : int
          value : int }
    
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
    
    [<StructuredFormatDisplay("{name} {nonets}")>]
    type Board = 
        { name : string
          nonets : Nonet list
          horizontalLines : Line list
          verticalLines : Line list }
    
    let rec transpose matrix = 
        match matrix with
        | (_ :: _) :: _ -> List.map List.head matrix :: transpose (List.map List.tail matrix)
        | _ -> []
    
    let rec stitch xss = 
        match xss with
        | x :: xs when List.length x > 1 -> (xss |> List.map List.head) :: stitch (xss |> List.map List.tail)
        | x :: xs -> [ xss |> List.map List.head ]
        | _ -> []
    
    let printLines xs = 
        printfn "["
        xs |> List.iter (fun x -> 
                  printf "\t[ "
                  List.iter (printf "%A; ") x
                  printfn "]")
        printfn "]"
    
    let rec printManyLines xss =
        printfn "----- %A" (List.length xss)
        match xss with
        | xs :: xss -> printLines xs 
                       printManyLines xss
        | [] -> ()

    let parseNonetFromRaw nonetLines horizontalPos = 
        let parseNonetLine r chars = 
            chars |> List.mapi (fun c x -> 
                         { row = r
                           col = c
                           value = System.Int32.Parse(x.ToString()) })
        
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
        xs |> List.map (fun x -> 
                  x
                  |> f
                  |> List.map (fun y -> 
                         y |> List.map (fun z -> 
                                  { cell = z
                                    nonet = x })))
    
    let parseFile rawLines = 
        let parseBoard (name :: rawNonetLines : string list) = 
            let getHorizontalLines xs = 
                xs
                |> attachCellsToNonets (fun x -> x.rows)
                |> List.chunkBySize (BoardSize)
                |> List.collect (transpose)
                |> List.map (fun xs -> xs |> List.concat)
            
            let getVerticalLines xs = 
                xs
                |> attachCellsToNonets (fun x -> x.cols)
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
    
    let answer = 
        let filePath = "C:\Users\Mendel\Documents\Visual Studio 2015\Projects\ProjectEuler\p096_sudoku.txt"
        let rawLines = System.IO.File.ReadLines(filePath)
        let board = parseFile (rawLines |> List.ofSeq) |> List.head
        printfn "Horizontal lines"
        board.horizontalLines |> printLines
        printfn "Vertical lines"
        board.verticalLines |> printLines
