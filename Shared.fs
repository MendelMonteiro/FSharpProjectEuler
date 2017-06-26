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

    let getDuplicates (xs : 'a list) =
        let dupes = 
            xs |> List.fold (fun output x -> 
                let existing = output |> Map.tryFind x
                match existing with
                | Some c -> output |> Map.add x (c + 1)
                | _ -> output |> Map.add x 1
                ) Map.empty<'a, int>
        dupes |> Map.filter (fun _ i -> i > 1) |> Map.toList |> List.map fst
