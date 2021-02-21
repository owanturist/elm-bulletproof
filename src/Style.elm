module Style exposing
    ( Element
    , Rule
    , Sheet
    , batch
    , class
    , classList
    , className
    , el
    , elements
    , focusVisible
    , hover
    , mod
    , render
    , rule
    , selector
    )

import Html
import Html.Attributes as Attributes


renderDefinition : String -> List Rule -> String
renderDefinition cssSelector rules =
    cssSelector ++ "{" ++ String.join "" (List.map unpackRule rules) ++ "}"


type Rule
    = Rule String


unpackRule : Rule -> String
unpackRule (Rule str) =
    str


rule : String -> String -> Rule
rule property value =
    Rule (property ++ ":" ++ value ++ ";")


type Target
    = Class (List Rule)
    | PseudoClass String (List Rule)


renderTarget : String -> Target -> String
renderTarget name target =
    case target of
        Class rules ->
            renderDefinition ("." ++ name) rules

        PseudoClass pseudo rules ->
            renderDefinition ("." ++ name ++ ":" ++ pseudo) rules


type Element
    = Element String (List Target)


renderElement : Element -> List String
renderElement (Element name targets) =
    List.map (renderTarget name) targets


el : String -> List Rule -> Element
el name rules =
    Element ("_bp_" ++ name) [ Class rules ]


mod : Element -> String -> List Rule -> Element
mod (Element name _) modificator rules =
    el (name ++ "__" ++ modificator) rules


pseudoClass : String -> List Rule -> Element -> Element
pseudoClass pseudo rules (Element name targets) =
    Element name (PseudoClass pseudo rules :: targets)


hover : List Rule -> Element -> Element
hover =
    pseudoClass "hover"


focusVisible : List Rule -> Element -> Element
focusVisible =
    pseudoClass "focus-visible"



-- S H E E T


type Sheet
    = Sheet (List String)


unpackSheet : Sheet -> List String
unpackSheet (Sheet strings) =
    strings


elements : List Element -> Sheet
elements =
    Sheet << List.concatMap renderElement


selector : String -> List Rule -> Sheet
selector cssSelector rules =
    Sheet [ renderDefinition cssSelector rules ]


batch : List Sheet -> Sheet
batch sheets =
    Sheet (List.concatMap unpackSheet sheets)


render : Sheet -> String
render (Sheet strings) =
    String.join "" strings



-- A T T R I B U T E S


className : Element -> String
className (Element name _) =
    name


class : Element -> Html.Attribute msg
class =
    Attributes.class << className


classList : List ( Element, Bool ) -> Html.Attribute msg
classList =
    Attributes.classList << List.map (Tuple.mapFirst className)
