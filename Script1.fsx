

    let rec transpose matrix = 
        match matrix with
        | (_ :: _) :: _ -> List.map List.head matrix :: transpose (List.map List.tail matrix)
        | _ -> []