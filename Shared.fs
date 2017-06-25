module Shared

    let rec transpose matrix = 
        match matrix with
        | (_ :: _) :: _ -> List.map List.head matrix :: transpose (List.map List.tail matrix)
        | _ -> []
    
    let flattenSeq nonet = nonet |> List.map List.concat

    let exceptSet (x : Set<'a>) (y : Set<'a>) = Set.union (x - y) (y - x)

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

    /// Converts a string into a list of characters.
    let explode (s : string) = [ for c in s -> c ]
    
    /// Converts a list of characters into a string.
    let implode (xs : char list) = 
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

