module Page.Home exposing (view)

import Element exposing (..)
import Element.Attributes exposing (..)
import BoxyStyle exposing (..)


-- VIEW --


view : Element Styles variation msg
view =
    grid None
        []
        { columns = [ percent 100 ]
        , rows = [ fill, fill ]
        , cells =
            [ cell
                { start = ( 0, 0 )
                , width = 1
                , height = 1
                , content =
                    row Box
                        [ center, verticalCenter, width (px 200), height (px 40) ]
                        [ (Element.text "Skapa ett formulär") ]
                }
            , cell
                { start = ( 0, 1 )
                , width = 1
                , height = 1
                , content = wrappedRow None [ center, paddingTop 20 ] [ Element.text "Hej! Välkommen till BoxyForms. Här kan du skapa formulär för t.ex. anmälan till ditt sportarrangemang eller ert bröllop!" ]
                }
            ]
        }
