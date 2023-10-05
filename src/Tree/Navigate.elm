module Tree.Navigate exposing
    ( to
    , map, alter
    , restructure
    )

{-| Additional helpers for a [zwilias/elm-rosetree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/)
using [`TreePath`](Tree-Path#TreePath)


## observe

@docs to


## altering

@docs map, alter


## transform

@docs restructure

-}

import List.Extra
import Tree exposing (Tree)
import Tree.Path exposing (TreePath)


{-| A powerful tree transformation which can be described as "replacing the tree node constructors"

    invertTree : Tree label -> Tree label
    invertTree =
        Tree.Navigate.restructure (\sub -> tree sub.label (sub.children |> List.reverse))

    toPreOrder : Tree label -> List label
    toPreOrder =
        Tree.Navigate.restructure (\sub -> sub.label :: (sub.children |> List.concat))

    type DifferentTree a
        = DifferentTree a (List (DifferentTree a))

    toDifferentTree : Tree label -> DifferentTree label
    toDifferentTree =
        Tree.Navigate.restructure (\sub -> DifferentTree sub.label sub.children)

Other names for this pattern are "unwrap", "fold" and ["banana"](https://en.wikipedia.org/wiki/Catamorphism#Terminology_and_history)
Resources I can recommend:

  - [video recording "Inverting a binary tree with 1 line of Elm - Joël" from elm online meetup](https://www.youtube.com/watch?v=dSMB3rsufC8)
  - [blog post "folds are constructor substitution"](https://www.haskellforall.com/2021/02/folds-are-constructor-substitution.html)

Additionally, you get the [`TreePath`](Tree-Path#TreePath) at each step. Check the readme for how this can be useful.

-}
restructure :
    ({ path : TreePath, label : label, children : List folded } -> folded)
    -> (Tree label -> folded)
restructure reduce =
    \tree ->
        reduce
            { path = Tree.Path.atTrunk
            , label = tree |> Tree.label
            , children =
                tree
                    |> Tree.children
                    |> List.indexedMap
                        (\index childTree ->
                            childTree
                                |> restructure
                                    (\state ->
                                        reduce
                                            { path = index :: state.path
                                            , label = state.label
                                            , children = state.children
                                            }
                                    )
                        )
            }


{-| Following the [`TreePath`](Tree-Path#TreePath), where do we end?

    import Tree exposing (tree)
    import Tree.Navigate
    import Tree.Path

    Tree.singleton "jo"
        |> Tree.Navigate.to Tree.Path.atTrunk
    --> Just (Tree.singleton "jo")

    tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            , Tree.singleton "bee"
            ]
        ]
        |> Tree.Navigate.to (Tree.Path.follow [ 1, 2 ])
    --> Just (Tree.singleton "bee")

    tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            ]
        ]
        |> Tree.Navigate.to (Tree.Path.follow [ 1, 2 ])
    --> Nothing

-}
to : TreePath -> Tree label -> Maybe (Tree label)
to path =
    case Tree.Path.step path of
        Nothing ->
            Just

        Just ( index, further ) ->
            \tree ->
                tree
                    |> Tree.children
                    |> List.Extra.getAt index
                    |> Maybe.andThen (to further)


{-| Change its sub-tree at a given [`TreePath`](Tree-Path#TreePath) based on its current value.

    import Tree exposing (tree)
    import Tree.Navigate
    import Tree.Path

    tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            , Tree.singleton "bee"
            ]
        ]
        |> Tree.Navigate.alter (Tree.Path.follow [ 1, 2 ])
            (Tree.mapLabel String.toUpper)
    --> tree "jo"
    -->     [ Tree.singleton "ann"
    -->     , tree "mic"
    -->         [ Tree.singleton "igg"
    -->         , Tree.singleton "dee"
    -->         , Tree.singleton "BEE"
    -->         ]
    -->     ]

    tree "jo"
        [ Tree.singleton "ann"
        , tree "mic"
            [ Tree.singleton "igg"
            , Tree.singleton "dee"
            ]
        ]
        |> Tree.Navigate.alter (Tree.Path.follow [ 1, 2 ])
            (Tree.mapLabel String.toUpper)
    --> tree "jo"
    -->     [ Tree.singleton "ann"
    -->     , tree "mic"
    -->         [ Tree.singleton "igg"
    -->         , Tree.singleton "dee"
    -->         ]
    -->     ]

-}
alter : TreePath -> (Tree a -> Tree a) -> Tree a -> Tree a
alter path updateAtPath =
    case Tree.Path.step path of
        Nothing ->
            updateAtPath

        Just ( index, further ) ->
            Tree.updateChildren
                (List.Extra.updateAt index
                    (alter further updateAtPath)
                )


{-| Change every label based on its [`TreePath`](Tree-Path#TreePath) and current value.

    import Tree exposing (tree)
    import Tree.Navigate
    import Tree.Path

    tree 1
        [ Tree.singleton 2
        , tree 3 [ Tree.singleton 4 ]
        , Tree.singleton 5
        ]
        |> Tree.Navigate.map
            (\n -> ( n.label * 2, n.path ))
    --> tree ( 2, Tree.Path.atTrunk )
    -->     [ Tree.singleton ( 4, Tree.Path.follow [ 0 ] )
    -->     , tree ( 6, Tree.Path.follow [ 1 ] )
    -->         [ Tree.singleton ( 8, Tree.Path.follow [ 1, 0 ] ) ]
    -->     , Tree.singleton ( 10, Tree.Path.follow [ 2 ] )
    -->     ]


    tree 0 [ Tree.singleton 1, tree 2 [ Tree.singleton 3 ], Tree.singleton 5 ]
        |> Tree.Navigate.map identity
        |> Tree.flatten
    --> [ { label = 0, path = Tree.Path.atTrunk }
    --> , { label = 1, path = Tree.Path.follow [ 0 ] }
    --> , { label = 2, path = Tree.Path.follow [ 1 ] }
    --> , { label = 3, path = Tree.Path.follow [ 1, 0 ] }
    --> , { label = 5, path = Tree.Path.follow [ 2 ] }
    --> ]

-}
map :
    ({ path : TreePath, label : label } -> mappedLabel)
    -> (Tree label -> Tree mappedLabel)
map labelWithPathChange =
    let
        mapStep : TreePath -> (Tree label -> Tree mappedLabel)
        mapStep path =
            \tree ->
                Tree.tree
                    (labelWithPathChange { path = path, label = tree |> Tree.label })
                    (tree
                        |> Tree.children
                        |> List.indexedMap
                            (\index ->
                                mapStep (path |> Tree.Path.toChild index)
                            )
                    )
    in
    mapStep Tree.Path.atTrunk
