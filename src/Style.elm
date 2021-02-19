module Style exposing
    ( Rule
    , Selector
    , Sheet
    , class
    , className
    , classNameString
    , classNames
    , focusVisible
    , hover
    , render
    , rule
    , sheet
    )

import Html.Styled as Html
import Html.Styled.Attributes as Attributes


unpack : (a -> String) -> List a -> String
unpack stringify items =
    String.join "" (List.map stringify items)


type Rule
    = Rule String


unpackRule : Rule -> String
unpackRule (Rule str) =
    str


rule : String -> String -> Rule
rule property value =
    Rule (property ++ ":" ++ value ++ ";")


type Selector
    = Selector String String


unpackSelector : Selector -> String
unpackSelector (Selector name rules) =
    "." ++ name ++ "{" ++ rules ++ "}"


class : String -> List Rule -> Selector
class name rules =
    Selector ("_bp__" ++ name) (unpack unpackRule rules)


pseudoClass : String -> Selector -> List Rule -> Selector
pseudoClass pseudo (Selector name _) rules =
    Selector (name ++ ":" ++ pseudo) (unpack unpackRule rules)


hover : Selector -> List Rule -> Selector
hover =
    pseudoClass "hover"


focusVisible : Selector -> List Rule -> Selector
focusVisible =
    pseudoClass "focus-visible"


classNameString : Selector -> String
classNameString (Selector name _) =
    name


className : Selector -> Html.Attribute msg
className =
    Attributes.class << classNameString


classNames : List ( Selector, Bool ) -> Html.Attribute msg
classNames selectors =
    selectors
        |> List.map (Tuple.mapFirst classNameString)
        |> Attributes.classList


type Sheet
    = Sheet String


unpackSheet : Sheet -> String
unpackSheet (Sheet str) =
    str


sheet : List Selector -> Sheet
sheet selectors =
    Sheet (unpack unpackSelector selectors)


render : List Sheet -> String
render sheets =
    unpack unpackSheet sheets
