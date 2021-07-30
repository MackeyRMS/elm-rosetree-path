module TreePath exposing
    ( TreePath, atTrunk, go
    , depth, goesToChildOf, goesToParentOf
    , step, toParent, toChild
    , serialize
    )

{-| `import exposing (TreePath, atTrunk)`.

@docs TreePath, atTrunk, go


## scan

@docs depth, goesToChildOf, goesToParentOf


## modify

@docs step, toParent, toChild


## transform

@docs serialize

-}

import List.Extra as List
import Serialize exposing (Codec)


{-| The unique location of a child-`Tree` in a `Tree`.

You describe which branch index to go along every step.

    [ {-first go to its child at index-}0
    , {-then go to the child of its child at index-}2
    , ...
    ]

-}
type alias TreePath =
    List Int


{-| Stay at that tree. Don't go anywhere.

    TreePath.atTrunk
    --> TreePath.go []

    TreePath.atTrunk
        |> TreePath.depth
    --> 0

-}
atTrunk : TreePath
atTrunk =
    []


{-| On which level of sub-branch is the final tree located?

    TreePath.depth TreePath.atTrunk
    --> 0

    TreePath.atTrunk
        |> TreePath.toChild 5
        |> TreePath.toChild 2
        |> TreePath.toChild 0
        |> TreePath.depth
    --> 3

-}
depth : TreePath -> Int
depth =
    List.length


{-| Does this path lead to a child or child of a child or ... of the tree at this path?

    TreePath.go [ 2, 3, 0 ]
        |> TreePath.goesToChildOf
            (TreePath.go [ 2, 3 ])
    --> True

    TreePath.go [ 2 ]
        |> TreePath.goesToChildOf
            (TreePath.go [ 2, 3 ])
    --> False

-}
goesToChildOf : TreePath -> TreePath -> Bool
goesToChildOf potentialParent =
    \path ->
        (path |> List.isPrefixOf potentialParent)
            && (path /= potentialParent)


{-| Does this path lead to its parent or the parent of its parent or ... of the tree at this path?

    TreePath.go [ 2 ]
        |> TreePath.goesToParentOf
            (TreePath.go [ 2, 3 ])
    --> True

    TreePath.go [ 2, 3, 0 ]
        |> TreePath.goesToParentOf
            (TreePath.go [ 2, 3 ])
    --> False

-}
goesToParentOf : TreePath -> TreePath -> Bool
goesToParentOf potentialChild =
    \path ->
        potentialChild |> goesToChildOf path


{-| The `TreePath` to its parent tree – if there is one.

    TreePath.go [ 1, 2, 3 ]
        |> TreePath.toParent
    --> Just (TreePath.go [ 1, 2 ])

    TreePath.atTrunk
        |> TreePath.toParent
    --> Nothing

-}
toParent : TreePath -> Maybe TreePath
toParent =
    \path ->
        List.unconsLast path
            |> Maybe.map
                (\( _, wayToParent ) -> wayToParent)


{-| The path to its ...th child from there.

    TreePath.atTrunk
        |> TreePath.toChild 4
        |> TreePath.toChild 1
        |> TreePath.toChild 8
    --> TreePath.go [ 4, 1, 8 ]

1.  Take the branch at index 4
2.  Then take its branch at index 1
3.  Its child at index 8 is where you wanted to go

This is often useful in recursive functions.

    view path tree =
        button [ onPress (Clicked path) ]
            :: (Tree.children tree
                    |> List.indexedMap
                        (\index ->
                            view (path |> TreePath.toChild index)
                        )
               )
            |> column [ Ui.padding (10 / TreePath.depth path) ]

-}
toChild : Int -> TreePath -> TreePath
toChild childIndex =
    \path -> path ++ [ childIndex ]


{-| Conveniently describe the route to go from the trunk.

    TreePath.go [ 2, 4, 0 ]
    --> [ 2, 4, 0 ] or
    --> TreePath.atTrunk
    -->     |> TreePath.toChild 2
    -->     |> TreePath.toChild 4
    -->     |> TreePath.toChild 0

-}
go : List Int -> TreePath
go childIndices =
    childIndices


{-| Go to the next child index.

    TreePath.go [ 2, 4, 0 ]
        |> TreePath.step
    --> Just ( 2, TreePath.go [ 4, 0 ] )

    TreePath.atTrunk
        |> TreePath.step
    --> Nothing

    at : TreePath -> Tree a -> Maybe (Tree a)
    at path =
        case TreePath.step path of
            Nothing ->
                Just

            Just ( index, further ) ->
                Tree.children
                    >> List.getAt index
                    >> Maybe.andThen (at further)

-}
step : TreePath -> Maybe ( Int, TreePath )
step =
    List.uncons


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize a `Tree`.
-}
serialize : Codec serializeError_ TreePath
serialize =
    Serialize.list Serialize.int
