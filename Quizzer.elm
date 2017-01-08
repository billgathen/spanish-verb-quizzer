module Quizzer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (int)
import Array


-- MODEL


type Msg
    = Infinitive
    | A1
    | A2
    | B1
    | B2
    | C1
    | C2
    | PickNewWord
    | NewWord Int


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
    , Verb "ser" "to be (permanent)" "soy" "eres" "es" "somos" "sois" "son"
    , Verb "estar" "to be (temporary)" "estoy" "estás" "está" "estamos" "estáis" "están"
    , Verb "ir" "to go" "voy" "vas" "va" "vamos" "vais" "van"
    , Verb "haber" "to have" "he" "has" "ha, hay" "hemos" "habéis" "han"
    , Verb "hacer" "to do/make" "hago" "haces" "hace" "hacemos" "hacéis" "hacen"
    , Verb "querer" "to want" "quiero" "quieres" "quiere" "queremos" "queréis" "quieren"
    , Verb "deber" "to owe/must" "debo" "debes" "debe" "debemos" "debéis" "deben"
    , Verb "poder" "to be able to" "puedo" "puedes" "puede" "podemos" "podéis" "pueden"
    , Verb "decir" "to say/tell" "digo" "dices" "dice" "decimos" "decís" "dicen"
    , Verb "hablar" "to speak/talk" "hablo" "hablas" "habla" "hablamos" "habláis" "hablan"
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
    }


initialModel : Model
initialModel =
    { verb = List.head verbs
    , answeredQuestions = noQuestionsAnswered
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model.verb of
        Just aVerb ->
            div [ class "container" ]
                [ viewHeader
                , viewVerb aVerb model
                , viewInstructions
                ]

        Nothing ->
            text "Not found"


viewHeader : Html Msg
viewHeader =
    div [ class "text-center" ]
        [ text "Spanish Verb Quizzer" ]


viewInstructions : Html Msg
viewInstructions =
    div [ class "text-center" ]
        [ text "Click the question marks (or the English phrase) to reveal the Spanish version. Click again to hide it." ]


viewVerb : Verb -> Model -> Html Msg
viewVerb verb model =
    let
        answers =
            model.answeredQuestions
    in
        table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [ colspan 3, class "text-center" ]
                        [ h1 [] [ toggleAnswer Infinitive answers.inf verb.inEnglish verb.infinitive ] ]
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
            , tr []
                [ th [ colspan 3, class "text-center" ] [ button [ class "btn", onClick PickNewWord ] [ text "Practice new word" ] ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
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

                _ ->
                    answers
    in
        case action of
            NewWord idx ->
                ( { model | verb = (selectWord idx), answeredQuestions = noQuestionsAnswered }, Cmd.none )

            PickNewWord ->
                ( model, randomWord )

            _ ->
                ( { model | answeredQuestions = updatedQuestions }, Cmd.none )


selectWord : Int -> Maybe Verb
selectWord idx =
    Array.get (idx - 1) (Array.fromList verbs)



-- COMMANDS


randomWord : Cmd Msg
randomWord =
    Random.generate NewWord (Random.int 1 (List.length verbs))



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, randomWord )
        , update = update
        , view = view
        , subscriptions = (\n -> Sub.none)
        }
