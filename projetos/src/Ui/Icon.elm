module Ui.Icon exposing
    ( mdIcon, mdNamed
    , mdInactive, mdDark, mdLight, mdSmall, mdNormal, mdLarge, mdHuge
    , css, googleFontsUrl
    )

{-| Material design icons


# Icon creation

@docs mdIcon, mdNamed


# Attributes

@docs mdInactive, mdDark, mdLight, mdSmall, mdNormal, mdLarge, mdHuge


# Stylesheet integration

@docs css, googleFontsUrl

-}

import Html exposing (..)
import Html.Attributes exposing (..)



--------------------------------------------------------------------------------
-- ICON FACTORIES
--------------------------------------------------------------------------------


{-| A simple material design icon refered only by its name.
-}
mdNamed : String -> Html msg
mdNamed st =
    i [ class "material-icons" ] [ text st ]


{-| A material design icon
-}
mdIcon : List (Attribute msg) -> String -> Html msg
mdIcon attrs st =
    i (class "material-icons" :: attrs) [ text st ]



--------------------------------------------------------------------------------
-- ATTRIBUTES
--------------------------------------------------------------------------------


{-| Sets the inactive class in icon
-}
mdInactive : Attribute msg
mdInactive =
    class "md-inactive"


{-| Sets the dark class in icon
-}
mdDark : Attribute msg
mdDark =
    class "md-dark"


{-| Sets the light class in icon
-}
mdLight : Attribute msg
mdLight =
    class "md-light"


{-| Sets the size to small (18px)
-}
mdSmall : Attribute msg
mdSmall =
    class "md-18"


{-| Sets the size to normal (24px)
-}
mdNormal : Attribute msg
mdNormal =
    class "md-24"


{-| Sets the size to large (36px)
-}
mdLarge : Attribute msg
mdLarge =
    class "md-36"


{-| Sets the size to huge (48px)
-}
mdHuge : Attribute msg
mdHuge =
    class "md-48"



--------------------------------------------------------------------------------
-- CSS STYLESHEET AND HACKS
--------------------------------------------------------------------------------


{-| Url to Material icons in Google Fonts
-}
googleFontsUrl : String
googleFontsUrl =
    "https://fonts.googleapis.com/icon?family=Material+Icons"


{-| A stylesheet link node to Google Fonts
-}
css : Html msg
css =
    Html.node "link" [ rel "stylesheet", href googleFontsUrl ] []
