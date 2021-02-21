module Icon exposing
    ( bars
    , css
    , elm
    , folder
    , folderEmpty
    , folderEmptyOpen
    , folderOpen
    , tools
    )

import Style
import Svg exposing (Svg, path, svg)
import Svg.Attributes as Attributes


css : Style.Sheet
css =
    Style.elements
        [ icon__root
        ]


icon__root : Style.Element
icon__root =
    Style.el "icon__root"
        [ Style.rule "height" "14px"
        ]


viewIcon : Int -> Int -> Int -> Int -> String -> Svg msg
viewIcon minX minY width height d =
    svg
        [ Attributes.class (Style.className icon__root)
        , Attributes.fill "currentColor"
        , [ minX, minY, width, height ]
            |> List.map String.fromInt
            |> String.join " "
            |> Attributes.viewBox
        ]
        [ path
            [ Attributes.d d
            ]
            []
        ]


folder : Svg msg
folder =
    viewIcon 0 0 512 512 "M464 128H272l-64-64H48C21.49 64 0 85.49 0 112v288c0 26.51 21.49 48 48 48h416c26.51 0 48-21.49 48-48V176c0-26.51-21.49-48-48-48z"


folderEmpty : Svg msg
folderEmpty =
    viewIcon 0 0 512 512 "M464 128H272l-54.63-54.63c-6-6-14.14-9.37-22.63-9.37H48C21.49 64 0 85.49 0 112v288c0 26.51 21.49 48 48 48h416c26.51 0 48-21.49 48-48V176c0-26.51-21.49-48-48-48zm0 272H48V112h140.12l54.63 54.63c6 6 14.14 9.37 22.63 9.37H464v224z"


folderOpen : Svg msg
folderOpen =
    viewIcon 0 0 576 512 "M572.694 292.093L500.27 416.248A63.997 63.997 0 0 1 444.989 448H45.025c-18.523 0-30.064-20.093-20.731-36.093l72.424-124.155A64 64 0 0 1 152 256h399.964c18.523 0 30.064 20.093 20.73 36.093zM152 224h328v-48c0-26.51-21.49-48-48-48H272l-64-64H48C21.49 64 0 85.49 0 112v278.046l69.077-118.418C86.214 242.25 117.989 224 152 224z"


folderEmptyOpen : Svg msg
folderEmptyOpen =
    viewIcon 0 0 576 512 "M527.9 224H480v-48c0-26.5-21.5-48-48-48H272l-64-64H48C21.5 64 0 85.5 0 112v288c0 26.5 21.5 48 48 48h400c16.5 0 31.9-8.5 40.7-22.6l79.9-128c20-31.9-3-73.4-40.7-73.4zM48 118c0-3.3 2.7-6 6-6h134.1l64 64H426c3.3 0 6 2.7 6 6v42H152c-16.8 0-32.4 8.8-41.1 23.2L48 351.4zm400 282H72l77.2-128H528z"


elm : Svg msg
elm =
    viewIcon -300 -300 600 600 "M-280 300L0 20l280 280zM-300-280L-20 0l-280 280zM20.324-300.338h280.014v280.014zM20 0l130 130L280 0 150-130zM-280-300h260l122 122h-260zM130-150L0-20l-130-130zM300 280L170 150 300 20z"


tools : Svg msg
tools =
    viewIcon 0 0 512 512 "M501.1 395.7L384 278.6c-23.1-23.1-57.6-27.6-85.4-13.9L192 158.1V96L64 0 0 64l96 128h62.1l106.6 106.6c-13.6 27.8-9.2 62.3 13.9 85.4l117.1 117.1a37.18 37.18 0 0052.7 0l52.7-52.7a37.36 37.36 0 000-52.7zM331.7 225c28.3 0 54.9 11 74.9 31l19.4 19.4c15.8-6.9 30.8-16.5 43.8-29.5 37.1-37.1 49.7-89.3 37.9-136.7-2.2-9-13.5-12.1-20.1-5.5l-74.4 74.4-67.9-11.3L334 98.9l74.4-74.4c6.6-6.6 3.4-17.9-5.7-20.2-47.4-11.7-99.6.9-136.6 37.9-28.5 28.5-41.9 66.1-41.2 103.6l82.1 82.1c8.1-1.9 16.5-2.9 24.7-2.9zm-103.9 82l-56.7-56.7L18.7 402.8c-25 25-25 65.5 0 90.5s65.5 25 90.5 0l123.6-123.6c-7.6-19.9-9.9-41.6-5-62.7zM64 472c-13.2 0-24-10.8-24-24 0-13.3 10.7-24 24-24s24 10.7 24 24a24 24 0 01-24 24z"


bars : Svg msg
bars =
    viewIcon 0 0 448 512 "M16 132h416c9 0 16-7 16-16V76c0-9-7-16-16-16H16C7 60 0 67 0 76v40c0 9 7 16 16 16zm0 160h416c9 0 16-7 16-16v-40c0-9-7-16-16-16H16c-9 0-16 7-16 16v40c0 9 7 16 16 16zm0 160h416c9 0 16-7 16-16v-40c0-9-7-16-16-16H16c-9 0-16 7-16 16v40c0 9 7 16 16 16z"
