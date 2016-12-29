module Quizzer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type Msg
    = Infinitive
    | A1
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


type alias Answered =
    { inf : Bool
    , a1 : Bool
    , a2 : Bool
    , b1 : Bool
    , b2 : Bool
    , c1 : Bool
    , c2 : Bool
    }


noQuestionsAnswered : Answered
noQuestionsAnswered =
    Answered False False False False False False False


type alias Model =
    { verb : Maybe Verb
    , answeredQuestions : Answered
    , clicked : String
    }


initialModel : Model
initialModel =
    { verb = List.head verbs
    , answeredQuestions = noQuestionsAnswered
    , clicked = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model.verb of
        Just aVerb ->
            div [ class "container" ]
                [ viewInstructions
                , viewVerb aVerb model
                ]

        Nothing ->
            text "Not found"


viewInstructions : Html Msg
viewInstructions =
    div []
        [ text "Click the question marks (or the English phrase) to reveal an answer. Click the answer to hide it." ]


viewVerb : Verb -> Model -> Html Msg
viewVerb verb model =
    let
        answers =
            model.answeredQuestions
    in
        table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [ colspan 3 ] [ toggleAnswer Infinitive answers.inf verb.inEnglish verb.infinitive ]
                    ]
                ]
            , tr []
                [ th [] []
                , th [] [ text "Singular" ]
                , th [] [ text "Plural" ]
                ]
            , tr []
                [ th [] [ text "First Person" ]
                , td [] [ toggleAnswer A1 answers.a1 "?????" verb.firstSingular ]
                , td [] [ toggleAnswer A2 answers.a2 "?????" verb.firstPlural ]
                ]
            , tr []
                [ th [] [ text "Second Person" ]
                , td [] [ toggleAnswer B1 answers.b1 "?????" verb.secondSingular ]
                , td [] [ toggleAnswer B2 answers.b2 "?????" verb.secondPlural ]
                ]
            , tr []
                [ th [] [ text "Third Person" ]
                , td [] [ toggleAnswer C1 answers.c1 "?????" verb.thirdSingular ]
                , td [] [ toggleAnswer C2 answers.c2 "?????" verb.thirdPlural ]
                ]
            ]


toggleAnswer : Msg -> Bool -> String -> String -> Html Msg
toggleAnswer msg isVisible question answer =
    let
        displayedWord =
            case isVisible of
                True ->
                    answer

                False ->
                    question
    in
        span [ onClick msg ] [ text displayedWord ]



-- UPDATE


update : Msg -> Model -> Model
update action model =
    let
        answers =
            model.answeredQuestions

        updatedQuestions =
            case action of
                Infinitive ->
                    { answers | inf = not answers.inf }

                A1 ->
                    { answers | a1 = not answers.a1 }

                A2 ->
                    { answers | a2 = not answers.a2 }

                B1 ->
                    { answers | b1 = not answers.b1 }

                B2 ->
                    { answers | b2 = not answers.b2 }

                C1 ->
                    { answers | c1 = not answers.c1 }

                C2 ->
                    { answers | c2 = not answers.c2 }
    in
        { model | answeredQuestions = updatedQuestions }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
