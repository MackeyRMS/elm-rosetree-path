module Tree.Path exposing
    ( TreePath, atTrunk, follow
    , depth, targetsChildOf, targetsParentOf
    , step, toParent, toChild
    )

{-| Location in a [`Tree`](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/)

@docs TreePath, atTrunk, follow


## observe

@docs depth, targetsChildOf, targetsParentOf


## alter

@docs step, toParent, toChild

-}

import List.Extra


{-| The unique location of a (child-)`Tree` in a [`Tree`](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/).

You describe which branch index to go along every step.

    [ {-first go to its child at index-}0
    , {-then go to the child of its child at index-}2
    , ...
    ]

Represented as a tuple to simplify pattern matching and enable using it as a `comparable` key.

Instead of working with that `List` directly, I recommend using the helpers here to create and operate on these paths.

-}
type alias TreePath =
    List Int


{-| Stay at that tree. Don't go anywhere.

    Tree.Path.atTrunk
    --> Tree.Path.follow []

    Tree.Path.atTrunk
        |> Tree.Path.depth
    --> 0

-}
atTrunk : TreePath
atTrunk =
    follow []


{-| On which level of sub-branch is the final tree located?

    Tree.Path.depth Tree.Path.atTrunk
    --> 0

    Tree.Path.atTrunk
        |> Tree.Path.toChild 5
        |> Tree.Path.toChild 2
        |> Tree.Path.toChild 0
        |> Tree.Path.depth
    --> 3

-}
depth : TreePath -> Int
depth =
    List.length


{-| Does this path lead to a child or child of a child or ... of the tree at this path?

    Tree.Path.follow [ 2, 3, 0 ]
        |> Tree.Path.targetsChildOf
            (Tree.Path.follow [ 2, 3 ])
    --> True

    Tree.Path.follow [ 2 ]
        |> Tree.Path.targetsChildOf
            (Tree.Path.follow [ 2, 3 ])
    --> False

-}
targetsChildOf : TreePath -> TreePath -> Bool
targetsChildOf potentialParent =
    \path ->
        (path |> List.Extra.isPrefixOf potentialParent)
            && (path /= potentialParent)


{-| Does this path lead to its parent or the parent of its parent or ... of the tree at this path?

    Tree.Path.follow [ 2 ]
        |> Tree.Path.targetsParentOf
            (Tree.Path.follow [ 2, 3 ])
    --> True

    Tree.Path.follow [ 2, 3, 0 ]
        |> Tree.Path.targetsParentOf
            (Tree.Path.follow [ 2, 3 ])
    --> False

-}
targetsParentOf : TreePath -> TreePath -> Bool
targetsParentOf potentialChild =
    \path ->
        potentialChild |> targetsChildOf path


{-| The `TreePath` to its parent tree – if there is one.

    Tree.Path.follow [ 1, 2, 3 ]
        |> Tree.Path.toParent
    --> Just (Tree.Path.follow [ 1, 2 ])

    Tree.Path.atTrunk
        |> Tree.Path.toParent
    --> Nothing

-}
toParent : TreePath -> Maybe TreePath
toParent =
    \path -> path |> List.Extra.init


{-| The path to its ...th child from there.

    Tree.Path.atTrunk
        |> Tree.Path.toChild 4
        |> Tree.Path.toChild 1
        |> Tree.Path.toChild 8
    --> Tree.Path.follow [ 4, 1, 8 ]

1.  Take the branch at index 4
2.  Then take its branch at index 1
3.  Its child at index 8 is where the navigation should end

Example ↓ is for illustration only.
You'll have a much easier time using [`Tree.Navigate.restructure`](Tree-Navigate#restructure).

    view path tree =
        button [ onPress (Clicked path) ]
            :: (Tree.children tree
                    |> List.indexedMap
                        (\index ->
                            view (path |> Tree.Path.toChild index)
                        )
               )
            |> column [ padding (10 / Tree.Path.depth path) ]

-}
toChild : Int -> TreePath -> TreePath
toChild childIndex =
    \path -> path ++ [ childIndex ]


{-| Conveniently describe the route to go from the trunk.

    Tree.Path.follow [ 2, 4, 0 ]
    --> [ 2, 4, 0 ]

    -- or
    Tree.Path.follow [ 2, 4, 0 ]
    --> Tree.Path.atTrunk
    -->     |> Tree.Path.toChild 2
    -->     |> Tree.Path.toChild 4
    -->     |> Tree.Path.toChild 0

-}
follow : List Int -> TreePath
follow childIndices =
    childIndices


{-| Try to navigate to a child index, returning a [`ForestPath`](Forest-Path#ForestPath) for the children.

    import Tree exposing (Tree)
    import Tree.Path exposing (TreePath)


    Tree.Path.follow [ 2, 4, 0 ]
        |> Tree.Path.step
    --> Just ( 2, Tree.Path.follow [ 4, 0 ] )

    Tree.Path.atTrunk
        |> Tree.Path.step
    --> Nothing

    -- in this package as Tree.Navigate.to
    at path =
        case Tree.Path.step path of
            Nothing ->
                Just

            Just ( index, further ) ->
                Tree.children
                    >> List.getAt index
                    >> Maybe.andThen (at further)

-}
step : TreePath -> Maybe ForestPath
step =
    List.Extra.uncons


{-| Should only be exposed from `Forest.Path`
-}
type alias ForestPath =
    ( Int, TreePath )
