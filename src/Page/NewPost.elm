module Page.NewPost exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Data.Session exposing (Session)


-- MODEL


type alias Post =
    { id : Int
    , title : String
    , content : String
    , date : String
    }


type alias Model =
    Post


initialModel : Model
initialModel =
    { id = 0
    , title = ""
    , content = ""
    , date = "just now"
    }



-- UPDATE


type Msg
    = SetTitle String
    | SetContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTitle t ->
            ( { model | title = t }, Cmd.none )

        SetContent c ->
            ( { model | content = c }, Cmd.none )



-- VIEW


view : Session -> Model -> Html Msg
view session model =
    div [ class "col mt-5 px-2" ]
        [ div [ class "text-right mr-3" ]
            [ button [ class "btn btn-secondary", type_ "button" ]
                [ text "Save" ]
            ]
        , br [] []
        , br [] []
        , Html.form [ class "mx-3" ]
            [ div [ class "form-group" ]
                [ input [ type_ "text", class "form-control", onInput SetTitle, placeholder "Post title" ] []
                ]
            , div [ class "form-group" ]
                [ textarea [ class "form-control", onInput SetContent, placeholder "Contents" ] []
                ]
            ]
        ]
