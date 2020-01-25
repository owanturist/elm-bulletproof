module Icon exposing
    ( dockHorizontal
    , dockVertical
    , elm
    , folder
    , folderEmpty
    , folderEmptyOpen
    , folderOpen
    )

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


dockHorizontal : Svg msg
dockHorizontal =
    viewIcon 0 0 1024 1024 "M85 121h854c24 0 42 18 42 41v700c0 23-18 41-42 41H608a44 44 0 0 1-7 0H85c-24 0-42-18-42-41V162c0-23 18-41 42-41zm41 535v165h772V656H126zm0-82h772V202H126v372zm185 197h-69c-19 0-34-14-34-32s15-33 34-33h69c19 0 34 15 34 33s-15 32-34 32zm236 0h-70c-18 0-33-14-33-32s15-33 33-33h70c18 0 33 15 33 33s-15 32-33 32zm235 0h-70c-18 0-33-14-33-32s15-33 33-33h70c18 0 33 15 33 33s-15 32-33 32z"


dockVertical : Svg msg
dockVertical =
    viewIcon 0 0 1024 1024 "M64 167.944v688c0 22.092 17.908 40 40 40h816c22.092 0 40-17.908 40-40v-688c0-22.092-17.908-40-40-40h-816c-22.092 0-40 17.908-40 40zM880 815.944h-240v-608h240v608zM144 207.944h416v608h-416v-608zM793.296 320.118h-66.398c-17.676 0-32-14.324-32-32 0-17.674 14.328-32 32-32h66.396c17.674 0 32.002 14.326 32.002 32 0 17.672-14.324 32-32 32zM793.296 448.118h-66.398c-17.676 0-32-14.324-32-32 0-17.674 14.328-32 32-32h66.396c17.674 0 32.002 14.326 32.002 32 0 17.672-14.324 32-32 32zM793.296 576.118h-66.398c-17.676 0-32-14.324-32-32 0-17.674 14.328-32 32-32h66.396c17.674 0 32.002 14.326 32.002 32 0 17.672-14.324 32-32 32z"


elm : Svg msg
elm =
    viewIcon -300 -300 600 600 "M-280 300L0 20l280 280zM-300-280L-20 0l-280 280zM20.324-300.338h280.014v280.014zM20 0l130 130L280 0 150-130zM-280-300h260l122 122h-260zM130-150L0-20l-130-130zM300 280L170 150 300 20z"
