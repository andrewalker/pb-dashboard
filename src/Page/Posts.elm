module Page.Posts exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Data.Session exposing (Session)


-- MODEL


type PostSorting
    = DateAsc
    | DateDesc


type alias Post =
    { id : Int
    , title : String
    , content : String
    , date : String
    }


type alias Model =
    { posts : List Post
    }


initialModel : Model
initialModel =
    { posts =
        [ { id = 7, title = "My Post Title 7", content = loremIpsum, date = "just now" }
        , { id = 6, title = "My Post Title 6", content = loremIpsum, date = "3 days ago" }
        , { id = 5, title = "My Post Title 5", content = loremIpsum, date = "1 week ago" }
        , { id = 4, title = "My Post Title 4", content = loremIpsum, date = "2 weeks ago" }
        , { id = 3, title = "My Post Title 3", content = loremIpsum, date = "2 weeks ago" }
        , { id = 2, title = "My Post Title 2", content = loremIpsum, date = "1 month ago" }
        , { id = 1, title = "My Post Title 1", content = loremIpsum, date = "1 month ago" }
        ]
    }



-- UPDATE


type Msg
    = Sort PostSorting


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Sort DateAsc ->
            ( { model | posts = List.sortBy .id model.posts }, Cmd.none )

        Sort DateDesc ->
            ( { model | posts = List.sortBy .id model.posts |> List.reverse }, Cmd.none )



-- VIEW


view : Session -> Model -> Html Msg
view session model =
    div [ class "col mt-5 px-2" ]
        [ div [ class "dropdown float-right mr-3" ]
            [ button [ class "btn btn-secondary dropdown-toggle", type_ "button", id "dropdownMenuButton", attribute "data-toggle" "dropdown" ]
                [ text "Sort by" ]
            , div [ class "dropdown-menu dropdown-menu-right", attribute "aria-labelledby" "dropdownMenuButton" ]
                [ a [ class "dropdown-item", href "javascript:void(0)", onClick (Sort DateAsc) ] [ text "Oldest first" ]
                , a [ class "dropdown-item", href "javascript:void(0)", onClick (Sort DateDesc) ] [ text "Newest first" ]
                ]
            ]
        , h1 [ class "ml-4" ] [ text "Posts" ]
        , div [ class "list-group list-group-flush ml-1" ]
            (List.map
                viewPost
                model.posts
            )
        , p [ class "text-center m-5" ]
            [ img [ src "spinner.svg", style [ ( "height", "25px" ), ( "width", "25px" ) ] ] [] ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    a [ href "javascript:void(0)", class "list-group-item list-group-item-action" ]
        [ div [ class "d-flex justify-content-between" ]
            [ h5 [] [ text post.title ]
            , small [] [ em [ class "text-muted" ] [ text post.date ] ]
            ]
        , p [] [ text post.content ]
        ]


loremIpsum : String
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed eu erat eget metus molestie lacinia. Pellentesque sed imperdiet orci. Cras tellus nibh, varius sed interdum a, rhoncus quis libero. Duis in libero metus. Praesent diam elit, elementum eu pretium vel, molestie id odio. In vitae vestibulum ante, at porta sapien. Nulla tellus ligula, lacinia id molestie vitae, fermentum vel arcu. Nunc lobortis volutpat lacus at feugiat. Nam fermentum in dolor pretium rutrum. Aenean molestie in urna sed interdum. Integer at neque auctor, interdum enim nec, aliquet justo. In faucibus orci blandit maximus fermentum. Etiam luctus libero eu orci condimentum, at viverra lectus fermentum. Fusce non augue eros. Praesent sit amet congue dolor. Vivamus erat mauris, tempus sit amet libero vitae, vestibulum porttitor nulla."
