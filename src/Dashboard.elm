module Main exposing (main)

import Html exposing (..)
import Navigation exposing (Location)
import Page.Posts as Posts
import Page.NewPost as NewPost
import Data.Session exposing (Session)
import Data.User as User
import Task


-- import Page.Errored as Errored exposing (PageLoadError)
-- import Page.Settings as Settings

import Route exposing (Route)
import Views.Page as Page


type Page
    = Blank
    | Posts Posts.Model
    | NewPost NewPost.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = { user = { id = 1, name = "André Walker", isConfirmed = False } }
        }


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session
    in
        case page of
            Blank ->
                -- This is for the very initial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame Page.Other

            Posts subModel ->
                Posts.view session subModel
                    |> frame Page.Posts
                    |> Html.map PostsMsg

            NewPost subModel ->
                NewPost.view session subModel
                    |> frame Page.NewPost
                    |> Html.map NewPostMsg



--
--           Settings subModel ->
--               Settings.view subModel
--                   |> frame Page.Other
--                   |> Html.map SettingsMsg
--


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page



-- UPDATE --


type Msg
    = SetRoute (Maybe Route)
    | PostsMsg Posts.Msg
    | NewPostMsg NewPost.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | pageState = Loaded Blank }, Cmd.none )

        Just Route.Posts ->
            ( { model | pageState = Loaded (Posts Posts.initialModel) }, Cmd.none )

        Just Route.NewPost ->
            ( { model | pageState = Loaded (NewPost NewPost.initialModel) }, Cmd.none )

        _ ->
            ( { model | pageState = Loaded Blank }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( PostsMsg subMsg, Posts subModel ) ->
                toPage Posts PostsMsg Posts.update subMsg subModel

            ( NewPostMsg subMsg, NewPost subModel ) ->
                toPage NewPost NewPostMsg NewPost.update subMsg subModel

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                ( model, Cmd.none )



-- MAIN --


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
