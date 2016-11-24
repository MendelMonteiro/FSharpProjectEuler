module Shared

    let rec transpose matrix = 
        match matrix with
        | (_ :: _) :: _ -> List.map List.head matrix :: transpose (List.map List.tail matrix)
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

