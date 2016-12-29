module Quizzer exposing (..)

import Html exposing (..)


-- import Html.Attributes exposing (..)

import Html.Events exposing (..)


-- MODEL


type Msg
    = A1
    | A2
    | B1
    | B2
    | C1
    | C2


type alias Verb =
    { infinitive : String
    , inEnglish : String
    , firstSingular : String
    , secondSingular : String
    , thirdSingular : String
    , firstPlural : String
    , secondPlural : String
    , thirdPlural : String
    }


verbs : List Verb
verbs =
    [ Verb "tener" "to have" "tengo" "tienes" "tiene" "tenemos" "tenéis" "tienen"
    ]


type alias Windows =
    { a1 : Bool
    , a2 : Bool
    , b1 : Bool
    , b2 : Bool
    , c1 : Bool
    , c2 : Bool
    }


allWindowsClosed : Windows
allWindowsClosed =
    Windows False False False False False False


type alias Model =
    { verb : Maybe Verb
    , openWindows : Windows
    , clicked : String
    }


initialModel : Model
initialModel =
    { verb = List.head verbs
    , openWindows = allWindowsClosed
    , clicked = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model.verb of
        Just aVerb ->
            div []
                [ viewInstructions
                , viewVerb aVerb model
                ]

        Nothing ->
            text "Not found"


viewInstructions : Html Msg
viewInstructions =
    div []
        [ text "Click a question mark to reveal the answer. Click the answer to hide it." ]


viewVerb : Verb -> Model -> Html Msg
viewVerb verb model =
    let
        wins =
            model.openWindows
    in
        table []
            [ thead []
                [ tr []
                    [ th [] []
                    , th [] [ text (bothLanguages verb) ]
                    , th [] []
                    ]
                ]
            , tr []
                [ th [] []
                , th [] [ text "Singular" ]
                , th [] [ text "Plural" ]
                ]
            , tr []
                [ th [] [ text "First Person" ]
                , td [] [ visibilityButton A1 wins.a1 verb.firstSingular ]
                , td [] [ visibilityButton A2 wins.a2 verb.firstPlural ]
                ]
            , tr []
                [ th [] [ text "Second Person" ]
                , td [] [ visibilityButton B1 wins.b1 verb.secondSingular ]
                , td [] [ visibilityButton B2 wins.b2 verb.secondPlural ]
                ]
            , tr []
                [ th [] [ text "Third Person" ]
                , td [] [ visibilityButton C1 wins.c1 verb.thirdSingular ]
                , td [] [ visibilityButton C2 wins.c2 verb.thirdPlural ]
                ]
            ]


bothLanguages : Verb -> String
bothLanguages verb =
    verb.infinitive ++ " (" ++ verb.inEnglish ++ ")"


visibilityButton : Msg -> Bool -> String -> Html Msg
visibilityButton msg isVisible word =
    let
        displayedWord =
            case isVisible of
                True ->
                    word

                False ->
                    "?"
    in
        span [ onClick msg ] [ text displayedWord ]



-- UPDATE


update : Msg -> Model -> Model
update action model =
    let
        windows =
            model.openWindows

        updatedWindows =
            case action of
                A1 ->
                    { windows | a1 = not windows.a1 }

                A2 ->
                    { windows | a2 = not windows.a2 }

                B1 ->
                    { windows | b1 = not windows.b1 }

                B2 ->
                    { windows | b2 = not windows.b2 }

                C1 ->
                    { windows | c1 = not windows.c1 }

                C2 ->
                    { windows | c2 = not windows.c2 }
    in
        { model | openWindows = updatedWindows }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
