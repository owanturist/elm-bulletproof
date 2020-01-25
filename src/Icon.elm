module Icon exposing (elm, folder, folderOpen)

import Css
import Svg.Styled as Svg exposing (Svg, path, styled, svg)
import Svg.Styled.Attributes as Attributes


styledIcon : List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
styledIcon =
    styled svg [ Css.height (Css.px 15) ]


p : String -> Svg msg
p d =
    path
        [ Attributes.d d
        ]
        []


viewIcon : Int -> Int -> Int -> Int -> String -> Svg msg
viewIcon minX minY width height d =
    styledIcon
        [ Attributes.fill "currentColor"
        , [ minX, minY, width, height ]
            |> List.map String.fromInt
            |> String.join " "
            |> Attributes.viewBox
        ]
        [ p d ]


folder : Svg msg
folder =
    viewIcon 0 0 512 512 "M464 128H272l-54.63-54.63c-6-6-14.14-9.37-22.63-9.37H48C21.49 64 0 85.49 0 112v288c0 26.51 21.49 48 48 48h416c26.51 0 48-21.49 48-48V176c0-26.51-21.49-48-48-48zm0 272H48V112h140.12l54.63 54.63c6 6 14.14 9.37 22.63 9.37H464v224z"


folderOpen : Svg msg
folderOpen =
    viewIcon 0 0 576 512 "M527.9 224H480v-48c0-26.5-21.5-48-48-48H272l-64-64H48C21.5 64 0 85.5 0 112v288c0 26.5 21.5 48 48 48h400c16.5 0 31.9-8.5 40.7-22.6l79.9-128c20-31.9-3-73.4-40.7-73.4zM48 118c0-3.3 2.7-6 6-6h134.1l64 64H426c3.3 0 6 2.7 6 6v42H152c-16.8 0-32.4 8.8-41.1 23.2L48 351.4zm400 282H72l77.2-128H528z"


elm : Svg msg
elm =
    viewIcon -300 -300 600 600 "M-280 300L0 20l280 280zM-300-280L-20 0l-280 280zM20.324-300.338h280.014v280.014zM20 0l130 130L280 0 150-130zM-280-300h260l122 122h-260zM130-150L0-20l-130-130zM300 280L170 150 300 20z"
