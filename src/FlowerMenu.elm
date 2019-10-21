module Main exposing (..)

{-| Recreating the menu found here: <https://github.com/nashvail/ReactPathMenu>
Make using:
elm-make FlowerMenu.elm --output elm.js
open index.html
-}

import Animation exposing (px, turn)
import Animation.Messenger
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (millisToPosix)


type Msg
    = Toggle
    | ClickSubmenu Int
    | Animate Animation.Msg


type alias Model =
    { submenus : List Submenu
    , open : Bool
    , message : ( String, Animation.State )
    , menu : Animation.Messenger.State Msg
    }


type alias Submenu =
    { icon : String
    , style : Animation.State
    }


onSubmenuStyle : (Int -> Animation.State -> Animation.State) -> List Submenu -> List Submenu
onSubmenuStyle fn submenus =
    List.indexedMap
        (\i submenu ->
            { submenu
                | style =
                    fn i submenu.style
            }
        )
        submenus


animateMainMenu : Float -> Animation.Messenger.State msg -> Animation.Messenger.State msg
animateMainMenu angle menu =
    Animation.interrupt
        [ Animation.to
            [ Animation.rotate (turn angle) ]
        ]
        menu


animateSubMenuItem : Float -> Int -> Animation.State -> Animation.State
animateSubMenuItem position index style =
    Animation.interrupt
        [ Animation.wait (millisToPosix (index * 50))
        , Animation.to [ Animation.translate (px 0) (px position) ]
        ]
        style


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Toggle ->
            if model.open then
                -- Close the menu
                ( { model
                    | open = False
                    , submenus = onSubmenuStyle (animateSubMenuItem 0) model.submenus
                    , menu = animateMainMenu -0.125 model.menu
                  }
                , Cmd.none
                )

            else
                -- Open the menu
                ( { model
                    | open = True
                    , submenus = onSubmenuStyle (animateSubMenuItem 100) model.submenus
                    , menu = animateMainMenu 0 model.menu
                  }
                , Cmd.none
                )

        ClickSubmenu i ->
            let
                msg =
                    Maybe.withDefault "whoops" <|
                        Maybe.map .icon <|
                            List.head <|
                                List.drop i model.submenus

                newMessageAnim =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 1
                            ]
                        , Animation.wait (millisToPosix 1000)
                        , Animation.to
                            [ Animation.opacity 0
                            ]
                        ]
                        (Tuple.second model.message)
            in
            ( { model
                | message =
                    ( msg, newMessageAnim )
              }
            , Cmd.none
            )

        Animate time ->
            let
                ( newMenu, menuCmds ) =
                    Animation.Messenger.update time model.menu

                newSubmenus =
                    onSubmenuStyle
                        (\i style ->
                            Animation.update time style
                        )
                        model.submenus

                messageAnim =
                    Animation.update time (Tuple.second model.message)
            in
            ( { model
                | menu = newMenu
                , submenus = newSubmenus
                , message = ( Tuple.first model.message, messageAnim )
              }
            , menuCmds
            )


view : Model -> Html Msg
view model =
    let
        icon =
            i
                (Animation.render model.menu
                    ++ [ class "fa fa-close fa-3x"
                       ]
                )
                []

        message =
            div
                (Animation.render (Tuple.second model.message)
                    ++ [ class "message"
                       ]
                )
                [ text (Tuple.first model.message) ]
    in
    div
        [ class "main-button"
        , onClick Toggle
        ]
        (icon :: message :: List.indexedMap viewSubmenu model.submenus)


viewSubmenu : Int -> Submenu -> Html Msg
viewSubmenu index submenu =
    div
        (Animation.render submenu.style
            ++ [ class "child-button"
               , onClick (ClickSubmenu index)
               ]
        )
        [ i [ class ("fa  fa-lg fa-" ++ submenu.icon) ] []
        ]


icons : List String
icons =
    List.take 5 [ "pencil", "at", "camera", "bell", "comment", "bolt", "ban", "code" ]


makeSubmenu i icon =
    { icon = icon
    , style =
        Animation.styleWith (Animation.spring { stiffness = 400, damping = 28 }) <|
            let
                adjustment =
                    0.5 - (((toFloat (List.length icons) - 1) / 2.0) * fanAngle)

                angle =
                    (toFloat i * fanAngle) + adjustment
            in
            [ Animation.rotate (turn angle)
            , Animation.translate (px 0) (px 0)
            , Animation.rotate (turn (-1 * angle))
            , Animation.backgroundColor (Animation.Color 238 238 236 1)

            -- Counter rotation so the icon is upright
            ]
    }


{-| In Turns
-}
fanAngle : Float
fanAngle =
    0.11


init =
    ( { open = False
      , menu =
            Animation.styleWith (Animation.spring { stiffness = 400, damping = 28 })
                [ Animation.rotate (turn -0.125)
                ]
      , submenus =
            List.indexedMap makeSubmenu icons
      , message =
            ( ""
            , Animation.style
                [ Animation.display Animation.block
                , Animation.opacity 0
                ]
            )
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| We have two subscriptions to our animations because we're using both Animation.State and Animation.Messenger.State, which can't both live in the same list.
-}
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate
            (Tuple.second model.message :: List.map .style model.submenus)
        , Animation.subscription Animate
            [ model.menu ]
        ]
