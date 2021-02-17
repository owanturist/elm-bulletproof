module Style exposing (Rule, Selector, Sheet, class, className, render, rule, sheet)

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
    Selector name (unpack unpackRule rules)


className : Selector -> Html.Attribute msg
className (Selector name _) =
    Attributes.class name


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
