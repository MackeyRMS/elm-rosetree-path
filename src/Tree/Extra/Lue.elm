module Tree.Extra.Lue exposing
    ( leaf
    , at
    , updateAt, removeAt, replaceAt, appendChildren, prependChildren
    , when
    , serialize
    )

{-| To `import as Tree exposing (leaf)`.

@docs leaf


## scan

@docs at


## modify

@docs updateAt, removeAt, replaceAt, appendChildren, prependChildren


## filter

@docs when


## transform

@docs serialize

-}

import List.Extra as List
import Serialize exposing (Codec)
import Tree exposing (Tree)
import TreePath exposing (TreePath)


{-| Alias to `Tree.singleton`: A `Tree` without children. It can be exposed → less noise.

    import Tree.Extra.Lue as Tree exposing (leaf)

    leaf 5 |> Tree.label
    --> 5

    leaf "foo" |> Tree.children
    --> []

-}
leaf : a -> Tree a
leaf =
    Tree.singleton


{-| Put a list of children left to all current children.

    tree "dear" [ leaf "George" ]
        |> Tree.prependChildren
            [ leaf "May", leaf "and" ]
    --> tree "dear"
    -->     [ leaf "May", leaf "and", leaf "George" ]

-}
prependChildren : List (Tree a) -> Tree a -> Tree a
prependChildren prependedChildren =
    Tree.mapChildren
        (\l -> prependedChildren ++ l)


{-| Put a list of children after all current children.

    tree "hello" [ leaf "you" ]
        |> Tree.appendChildren
            [ leaf "and", leaf "you" ]
    --> tree "hello"
    -->     [ leaf "you", leaf "and", leaf "you" ]

-}
appendChildren : List (Tree a) -> Tree a -> Tree a
appendChildren appendedChildren =
    Tree.mapChildren
        (\l -> l ++ appendedChildren)


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize a `Tree`.
-}
serialize : Codec error a -> Codec error (Tree a)
serialize serializeLabel =
    Serialize.customType
        (\encodeTree tree ->
            encodeTree (Tree.label tree) (Tree.children tree)
        )
        |> Serialize.variant2 Tree.tree
            serializeLabel
            (Serialize.list
                (Serialize.lazy
                    (\() -> serialize serializeLabel)
                )
            )
        |> Serialize.finishCustomType


{-| Following the [`TreePath`](TreePath#TreePath), where do we end?

    leaf "jo"
        |> Tree.at TreePath.atTrunk
    --> Just (leaf "jo")

    tree "jo"
        [ leaf "ann"
        , tree "mic"
            [ leaf "igg"
            , leaf "dee"
            , leaf "bee"
            ]
        ]
        |> Tree.at (TreePath.go [ 1, 2 ])
    --> Just (leaf "bee")

    tree "jo"
        [ leaf "ann"
        , tree "mic"
            [ leaf "igg"
            , leaf "dee"
            ]
        ]
        |> Tree.at (TreePath.go [ 1, 2 ])
    --> Nothing

-}
at : TreePath -> Tree a -> Maybe (Tree a)
at path =
    case TreePath.step path of
        Nothing ->
            Just

        Just ( index, further ) ->
            Tree.children
                >> List.getAt index
                >> Maybe.andThen (at further)


{-| Replace the sub-tree at a [`TreePath`](TreePath#TreePath) with a new `Tree`.

    tree "jo"
        [ leaf "ann"
        , tree "mic"
            [ leaf "igg"
            , leaf "dee"
            , leaf "bee"
            ]
        ]
        |> Tree.replaceAt (TreePath.go [ 1, 2 ])
            (leaf "be")
    --> tree "jo"
    -->     [ leaf "ann"
    -->     , tree "mic"
    -->         [ leaf "igg"
    -->         , leaf "dee"
    -->         , leaf "be"
    -->         ]
    -->     ]

    tree "jo"
        [ leaf "ann"
        , tree "mic"
            [ leaf "igg"
            , leaf "dee"
            ]
        ]
        |> Tree.replaceAt (TreePath.go [ 1, 2 ])
            (leaf "be")
    --> tree "jo"
    -->     [ leaf "ann"
    -->     , tree "mic"
    -->         [ leaf "igg"
    -->         , leaf "dee"
    -->         ]
    -->     ]

-}
replaceAt : TreePath -> Tree a -> Tree a -> Tree a
replaceAt path newSubTreeAtPath =
    case TreePath.step path of
        Nothing ->
            \_ -> newSubTreeAtPath

        Just ( index, further ) ->
            Tree.mapChildren
                (List.updateAt index
                    (replaceAt further newSubTreeAtPath)
                )


{-| Remove the sub-`Tree` at the end of a [`TreePath`](TreePath#TreePath) from a `Tree`.

    [ leaf "ann"
    , tree "mic"
        [ leaf "igg"
        , leaf "dee"
        , leaf "bee"
        ]
    ]
    |> Tree.removeAt (TreePath.go [ 1, 2 ])
    --> [ leaf "ann"
    --> , tree "mic"
    -->     [ leaf "igg"
    -->     , leaf "dee"
    -->     ]
    --> ]


    [ leaf "ann"
    , tree "mic"
        [ leaf "igg"
        , leaf "dee"
        ]
    ]
    |> Tree.removeAt (TreePath.go [ 1, 2 ])
    --> [ leaf "ann"
    --> , tree "mic"
    -->     [ leaf "igg"
    -->     , leaf "dee"
    -->     ]
    --> ]

    [ leaf "jo" ]
        |> Tree.removeAt TreePath.atTrunk
    --> []

-}
removeAt : TreePath -> List (Tree a) -> List (Tree a)
removeAt path =
    case TreePath.step path of
        Nothing ->
            \_ -> []

        Just ( index, further ) ->
            case further |> TreePath.step of
                Nothing ->
                    List.removeAt index

                Just _ ->
                    List.updateAt index
                        (Tree.mapChildren (removeAt further))


{-| Change the sub-tree at a [`TreePath`](TreePath#TreePath) based on its current value.

    tree "jo"
        [ leaf "ann"
        , tree "mic"
            [ leaf "igg"
            , leaf "dee"
            , leaf "bee"
            ]
        ]
        |> Tree.updateAt (TreePath.go [ 1, 2 ])
            (Tree.mapLabel String.toUpper)
    --> tree "jo"
    -->     [ leaf "ann"
    -->     , tree "mic"
    -->         [ leaf "igg"
    -->         , leaf "dee"
    -->         , leaf "BEE"
    -->         ]
    -->     ]

    tree "jo"
        [ leaf "ann"
        , tree "mic"
            [ leaf "igg"
            , leaf "dee"
            ]
        ]
        |> Tree.updateAt (TreePath.go [ 1, 2 ])
            (Tree.mapLabel String.toUpper)
    --> tree "jo"
    -->     [ leaf "ann"
    -->     , tree "mic"
    -->         [ leaf "igg"
    -->         , leaf "dee"
    -->         ]
    -->     ]

-}
updateAt : TreePath -> (Tree a -> Tree a) -> Tree a -> Tree a
updateAt path updateAtPath =
    case TreePath.step path of
        Nothing ->
            updateAtPath

        Just ( index, further ) ->
            Tree.mapChildren
                (List.updateAt index
                    (updateAt further updateAtPath)
                )


{-| Take all the branches where condition is met.
When a branch fails, all its children are checked.

    tree { name = "boss tree", selected = Unselected }
        [ tree { name = "hugg", selected = Selected }
            [ leaf { name = "zugg", selected = Selected }
            , leaf { name = "gugg", selected = Unselected }
            ]
        , leaf { name = "naomi", selected = Unselected }
        , tree { name = "unselmo", selected = Unselected }
            [ leaf { name = "selmo", selected = Selected }
            , leaf { name = "helmo", selected = Unselected }
            ]
        ]
        |> Tree.children
        |> Tree.when
            (Tree.label
                >> .selected
                >> (==) Selected
            )

    --> [ tree { name = "hugg", selected = Selected }
    -->     [ leaf { name = "zugg", selected = Selected }
    -->     , leaf { name = "gugg", selected = Unselected }
    -->     ]
    --> , leaf { name = "selmo", selected = Selected }
    --> ]

-}
when : (Tree a -> Bool) -> List (Tree a) -> List (Tree a)
when isGood trees =
    (trees |> List.filter isGood)
        ++ (trees
                |> List.filter (not << isGood)
                |> List.concatMap
                    (Tree.children >> when isGood)
           )
