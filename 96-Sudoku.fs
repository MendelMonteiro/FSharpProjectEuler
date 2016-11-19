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
    
    type Cell = 
        { row : int
          col : int
          value : int }
    
    type Nonet = 
        { cells : Cell list
          rows : Cell list list
          cols : Cell list list }
    
    type CellInNonet = 
        { cell : Cell
          nonet : Nonet }
    
    type NonetInBoard = 
        { nonet : Nonet
          left : Nonet option
          top : Nonet option
          right : Nonet option
          down : Nonet option }
    
    type Line = CellInNonet list
    
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
    
    let parseFile rawLines = 
        let parseNonetFromRaw horizontalPos nonetLines = 
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
            { cells = vals
              rows = r
              cols = r |> transpose }
        
        let parseBoard (name :: nonetLines : string list) = 
            let getHorizontalLines xs = 
                let foo = 
                    xs |> List.map (fun x -> 
                              x.rows |> List.map (fun y -> 
                                            y |> List.map (fun z -> 
                                                     { cell = z
                                                       nonet = x })))
                foo
                |> transpose
                |> (List.map List.concat)
            
            let nonets = [ 0..2 ] |> List.map (fun x -> nonetLines |> parseNonetFromRaw x)
            
            //printfn "%A" nonets
            let board = 
                { name = name
                  nonets = nonets
                  horizontalLines = (nonets |> getHorizontalLines)
                  verticalLines = [] }
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
        board.horizontalLines |> List.iter (printfn "%A")
