﻿#load "Shared.fs"
open Shared

let list = [
        [ [(0, 0); (1, 0); (2, 0)]; [(0, 0); (1, 0); (2, 0)]; [(0, 0); (1, 0); (2, 0)]; ]
        [ [(0, 1); (1, 1); (2, 1)]; [(0, 1); (1, 1); (2, 1)]; [(0, 1); (1, 1); (2, 1)]; ]
        [ [(0, 2); (1, 2); (2, 2)]; [(0, 2); (1, 2); (2, 2)]; [(0, 2); (1, 2); (2, 2)]; ]
        [ [(0, 0); (1, 0); (2, 0)]; [(0, 0); (1, 0); (2, 0)]; [(0, 0); (1, 0); (2, 0)]; ]
        [ [(0, 1); (1, 1); (2, 1)]; [(0, 1); (1, 1); (2, 1)]; [(0, 1); (1, 1); (2, 1)]; ]
        [ [(0, 2); (1, 2); (2, 2)]; [(0, 2); (1, 2); (2, 2)]; [(0, 2); (1, 2); (2, 2)]; ]
        [ [(0, 0); (1, 0); (2, 0)]; [(0, 0); (1, 0); (2, 0)]; [(0, 0); (1, 0); (2, 0)]; ]
        [ [(0, 1); (1, 1); (2, 1)]; [(0, 1); (1, 1); (2, 1)]; [(0, 1); (1, 1); (2, 1)]; ]
        [ [(0, 2); (1, 2); (2, 2)]; [(0, 2); (1, 2); (2, 2)]; [(0, 2); (1, 2); (2, 2)]; ]
]

list |> transpose
list |> List.map transpose |> transpose
list |>  transpose |> List.map transpose

let input = ['a'; 'b'; 'b'; 'c']


getDuplicates input