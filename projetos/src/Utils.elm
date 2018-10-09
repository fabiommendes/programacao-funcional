module Utils exposing
    ( Tag
    , htmlIndexedMap
    , htmlList
    , htmlMap
    , mapAt
    , olIndexedMap
    , olMap
    , ulIndexedMap
    , ulMap
    , viewList
    )

{-| Utility functions
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Tag msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias HtmlWrapper msg =
    List (Html msg) -> Html msg


{-| Convert list of Html elements into a single HTML list. Usually called with
ul [] and li [] pairs:

    htmlList (ul []) (li []) listOfHtmlElements

This will return an Html msg node corresponding to an ordered list.

-}
htmlList : HtmlWrapper msg -> HtmlWrapper msg -> List (Html msg) -> Html msg
htmlList root item lst =
    root (List.map (\x -> item [ x ]) lst)


viewList : Tag msg -> Tag msg -> List (Html msg) -> Html msg
viewList root item lst =
    root [] (List.map (\x -> item [] [ x ]) lst)


{-| Maps view function into list of elements and instead of returning a list
of elements, return an ul list.
-}
ulMap : (a -> Html msg) -> List a -> Html msg
ulMap =
    htmlMap ul li


ulIndexedMap =
    htmlIndexedMap ul li


olMap =
    htmlMap ol li


olIndexedMap =
    htmlIndexedMap ol li


{-| Maps view function into list of elements and instead of returning a list
of elements, return an ul list.
-}
htmlMap : Tag msg -> Tag msg -> (a -> Html msg) -> List a -> Html msg
htmlMap parent wrapper func lst =
    let
        wrap f e =
            wrapper [] [ f e ]
    in
    parent [] (List.map (wrap func) lst)


{-| Maps view function into list of elements and instead of returning a list
of elements, return an ul list.
-}
htmlIndexedMap : Tag msg -> Tag msg -> (Int -> a -> Html msg) -> List a -> Html msg
htmlIndexedMap parent wrapper func lst =
    let
        wrap f i e =
            wrapper [] [ f i e ]
    in
    parent [] (List.indexedMap (wrap func) lst)



--- LIST FUNCTIONS


mapAt : Int -> (a -> a) -> List a -> List a
mapAt idx f lst =
    mapAtAcc idx f lst []


mapAtAcc : Int -> (a -> a) -> List a -> List a -> List a
mapAtAcc idx f lst acc =
    case lst of
        [] ->
            acc

        x :: tail ->
            if idx == 0 then
                List.reverse acc ++ f x :: tail

            else
                mapAtAcc (idx - 1) f tail (x :: acc)
