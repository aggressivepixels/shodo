module List.Extra exposing (chunk, get)


chunk : Int -> List a -> List (List a)
chunk count list =
    case List.take count list of
        [] ->
            []

        head ->
            head :: chunk count (List.drop count list)


get : Int -> List a -> Maybe a
get index list =
    List.head (List.drop index list)
