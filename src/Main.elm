module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import String



-- PAPER SIZE


type PaperSize
    = A4
    | A5
    | Letter


paperSizeToString : PaperSize -> String
paperSizeToString paperSize =
    case paperSize of
        A4 ->
            "A4"

        A5 ->
            "A5"

        Letter ->
            "Letter"


paperSizeFromString : String -> Maybe PaperSize
paperSizeFromString paperSize =
    case paperSize of
        "A4" ->
            Just A4

        "A5" ->
            Just A5

        "Letter" ->
            Just Letter

        _ ->
            Nothing


paperSizeDimensions : PaperSize -> ( Float, Float )
paperSizeDimensions paperSize =
    case paperSize of
        A4 ->
            ( 210, 297 )

        A5 ->
            ( 148, 210 )

        Letter ->
            ( 215.9, 279.4 )



-- PAGE


page : Float -> Float -> Float -> Float -> Int -> Int -> Float -> List Char -> List Char -> H.Html msg
page margin width height colSpacing colCount rowCount charSize firstRowChars secondRowChars =
    H.div
        [ HA.class "font-epkyouka table w-full print:h-full align-middle"
        , HA.class "[page-break-after:always] last:[page-break-after:auto]"
        , HA.class "mb-[var(--margin)] first-of-type:mt-[var(--margin)] print:mb-0 first-of-type:print:mt-0"

        -- TODO: This is horrible but we need to disable this spacing in print, which means we need to use
        --       Tailwind's selectors, and Elm doesn't let you set CSS variables with the `style` function
        , HA.attribute "style" ("--margin: " ++ (String.fromFloat margin ++ "mm"))
        ]
        [ H.div [ HA.class "align-middle table-cell" ]
            [ H.div
                [ HA.class "flex mx-auto"
                , HA.style "width" (String.fromFloat width ++ "mm")
                , HA.style "height" (String.fromFloat height ++ "mm")
                , HA.style "gap" (String.fromFloat colSpacing ++ "mm")
                ]
                (List.map
                    (\index ->
                        pageCol
                            rowCount
                            charSize
                            (List.Extra.get index firstRowChars)
                            (List.Extra.get index secondRowChars)
                    )
                    (List.range 0 colCount)
                )
            ]
        ]


pageCol : Int -> Float -> Maybe Char -> Maybe Char -> H.Html msg
pageCol rowCount charSize firstRowChar secondRowChar =
    H.div [ HA.class "flex-1 p-[0.5mm] flex flex-col gap-[0.5mm] bg-blue-200" ]
        (List.map
            (\index ->
                case ( index, firstRowChar, secondRowChar ) of
                    ( 0, Just char, _ ) ->
                        pageRow (Just (viewChar charSize 1.0 char))

                    ( 1, _, Just char ) ->
                        pageRow (Just (viewChar charSize 0.25 char))

                    _ ->
                        pageRow Nothing
            )
            (List.range 0 rowCount)
        )


viewChar : Float -> Float -> Char -> H.Html msg
viewChar charSize opacity char =
    H.div
        [ HA.class "absolute top-0 left-0 bottom-0 right-0 flex items-center justify-center"
        , HA.style "font-size" (String.fromFloat (charSize - 2) ++ "mm")
        , HA.style "opacity" (String.fromFloat opacity)
        ]
        [ H.text (String.fromChar char) ]


pageRow : Maybe (H.Html msg) -> H.Html msg
pageRow html =
    H.div [ HA.class "relative flex-1 bg-white box-border" ]
        [ H.div [ HA.class "absolute top-0 left-0 right-0 bottom-0 flex flex-col" ]
            [ H.div [ HA.class "flex-1 flex" ]
                [ H.div [ HA.class "flex-1" ] []
                , H.div [ HA.class "border-l-[0.5mm] border-l-blue-200 border-dashed w-[0.5mm]" ] []
                , H.div [ HA.class "flex-1" ] []
                ]
            , H.div [ HA.class "border-t-[0.5mm] border-t-blue-200 border-dashed h-[0.5mm]" ] []
            , H.div [ HA.class "flex-1 flex" ]
                [ H.div [ HA.class "flex-1" ] []
                , H.div [ HA.class "border-l-[0.5mm] border-l-blue-200 border-dashed w-[0.5mm]" ] []
                , H.div [ HA.class "flex-1" ] []
                ]
            ]
        , case html of
            Just h ->
                h

            Nothing ->
                H.text ""
        ]



-- RENDERER


type Orientation
    = Vertical
    | Horizontal


orientationToString : Orientation -> String
orientationToString orientation =
    case orientation of
        Horizontal ->
            "Horizontal"

        Vertical ->
            "Vertical"


orientationFromString : String -> Maybe Orientation
orientationFromString orientation =
    case orientation of
        "Horizontal" ->
            Just Horizontal

        "Vertical" ->
            Just Vertical

        _ ->
            Nothing


type alias Model =
    { paperSize : PaperSize
    , orientation : Orientation
    , margin : Float
    , charSize : Float
    , pageContent : PageContent
    , text : String
    , showSecondRow : Bool
    }


type PageContent
    = Empty
    | RepeatChar
    | Text


type Msg
    = PaperSizeChanged String
    | OrientationChanged String
    | MarginChanged String
    | CharSizeChanged String
    | PageContentChanged PageContent
    | ShowSecondRowChanged Bool
    | TextChanged String


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init () =
    ( { paperSize = Letter
      , orientation = Horizontal
      , margin = 12
      , charSize = 15
      , pageContent = Empty
      , text = ""
      , showSecondRow = False
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view { paperSize, orientation, margin, charSize, pageContent, text, showSecondRow } =
    let
        ( paperWidth, paperHeight ) =
            paperSizeDimensions paperSize

        pageWidth =
            if orientation == Horizontal then
                paperHeight - 1

            else
                paperWidth - 1

        pageHeight =
            if orientation == Horizontal then
                paperWidth - 1

            else
                paperHeight - 1

        workingWidth =
            pageWidth - (margin * 2)

        workingHeight =
            pageHeight - (margin * 2)

        colSpacing =
            charSize / 3

        colCount =
            findOptimalFieldCount workingWidth charSize colSpacing

        rowCount =
            findOptimalFieldCount workingHeight charSize 0

        finalPageWidth =
            toFloat colCount * charSize + toFloat (colCount - 1) * colSpacing

        finalPageHeight =
            toFloat rowCount * charSize
    in
    { title = "Shodo"
    , body =
        settings paperSize orientation margin charSize pageContent text showSecondRow
            :: (case ( pageContent, String.uncons text ) of
                    ( _, Nothing ) ->
                        [ page margin finalPageWidth finalPageHeight colSpacing colCount rowCount charSize [] [] ]

                    ( Empty, _ ) ->
                        [ page margin finalPageWidth finalPageHeight colSpacing colCount rowCount charSize [] [] ]

                    ( RepeatChar, Just ( first, _ ) ) ->
                        let
                            chars =
                                List.repeat (colCount + 1) first
                        in
                        [ page margin
                            finalPageWidth
                            finalPageHeight
                            colSpacing
                            colCount
                            rowCount
                            charSize
                            chars
                            (if showSecondRow then
                                chars

                             else
                                []
                            )
                        ]

                    ( Text, Just ( first, rest ) ) ->
                        List.Extra.chunk (colCount + 1) (first :: String.toList rest)
                            |> List.map
                                (\pageChars ->
                                    page margin
                                        finalPageWidth
                                        finalPageHeight
                                        colSpacing
                                        colCount
                                        rowCount
                                        charSize
                                        pageChars
                                        (if showSecondRow then
                                            pageChars

                                         else
                                            []
                                        )
                                )
               )
    }


findOptimalFieldCount : Float -> Float -> Float -> Int
findOptimalFieldCount =
    let
        go : Int -> Float -> Float -> Float -> Int
        go cols availableSpace charSize colSpacing =
            let
                newCols =
                    cols + 1

                totalWidth =
                    toFloat newCols * charSize + toFloat (newCols - 1) * colSpacing
            in
            if totalWidth > availableSpace then
                cols

            else
                go newCols availableSpace charSize colSpacing
    in
    go 1


settings : PaperSize -> Orientation -> Float -> Float -> PageContent -> String -> Bool -> H.Html Msg
settings currentPaperSize currentOrientation currentMargin currentCharSize pageContent text showSecondRow =
    let
        textFields label =
            H.div
                [ HA.class "border border-gray-200 sm:rounded-md p-6 flex flex-col gap-6" ]
                [ textarea "text" label [] [ HA.value text, HE.onInput TextChanged ]
                , checkbox "show-second-row" "Show second row of text with low opacity" [] [ HA.checked showSecondRow, HE.onCheck ShowSecondRowChanged ]
                ]
    in
    H.form [ HA.class "print:hidden flex flex-col mt-4 sm:mx-4 items-center" ]
        [ H.div [ HA.class "border border-gray-200 sm:rounded-md w-full max-w-screen-md p-6" ]
            [ H.fieldset
                [ HA.class "grid sm:grid-cols-2 gap-6" ]
                [ H.legend [ HA.class "sr-only" ] [ H.text "Page" ]
                , H.div [ HA.attribute "aria-hidden" "true", HA.class "text-base font-medium text-gray-900 sm:col-span-2" ] [ H.text "Page" ]
                , input "margin" "Margin" [] [ HA.min "0", HA.type_ "number", HA.value (round currentMargin |> String.fromInt), HE.onInput MarginChanged ]
                , input "char-size" "Character size" [] [ HA.min "10", HA.type_ "number", HA.value (round currentCharSize |> String.fromInt), HE.onInput CharSizeChanged ]
                , select "paper-size" "Paper size" [] [ HE.onInput PaperSizeChanged ] ([ Letter, A4, A5 ] |> List.map (\paperSize -> H.option [ HA.value (paperSizeToString paperSize), HA.selected (paperSize == currentPaperSize) ] [ H.text (paperSizeToString paperSize) ]))
                , select "orientation" "Orientation" [] [ HE.onInput OrientationChanged ] ([ Horizontal, Vertical ] |> List.map (\orientation -> H.option [ HA.value (orientationToString orientation), HA.selected (orientation == currentOrientation) ] [ H.text (orientationToString orientation) ]))
                ]
            , H.fieldset
                [ HA.class "flex flex-col mt-6 gap-6" ]
                [ H.legend [ HA.class "sr-only" ] [ H.text "Text" ]
                , H.div [ HA.attribute "aria-hidden" "true", HA.class "text-base font-medium text-gray-900 sm:col-span-2" ] [ H.text "Text" ]
                , radio "page-content-empty" "Empty" [] [ HA.checked (pageContent == Empty), HE.onInput (\_ -> PageContentChanged Empty) ]
                , radio "page-content-repeat-character" "Repeat one character" [] [ HA.checked (pageContent == RepeatChar), HE.onInput (\_ -> PageContentChanged RepeatChar) ]
                , if pageContent == RepeatChar then
                    textFields "Character"

                  else
                    H.text ""
                , radio "page-content-text" "Show text" [] [ HA.checked (pageContent == Text), HE.onInput (\_ -> PageContentChanged Text) ]
                , if pageContent == Text then
                    textFields "Text"

                  else
                    H.text ""
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PaperSizeChanged newPaperSize ->
            ( { model
                | paperSize = paperSizeFromString newPaperSize |> Maybe.withDefault Letter
              }
            , Cmd.none
            )

        OrientationChanged newOrientation ->
            ( { model
                | orientation = orientationFromString newOrientation |> Maybe.withDefault Horizontal
              }
            , Cmd.none
            )

        MarginChanged newMargin ->
            ( { model
                | margin = String.toFloat newMargin |> Maybe.withDefault 0
              }
            , Cmd.none
            )

        CharSizeChanged newCharSize ->
            ( { model
                | charSize = String.toFloat newCharSize |> Maybe.withDefault 0
              }
            , Cmd.none
            )

        PageContentChanged newPageContent ->
            ( { model | pageContent = newPageContent }
            , Cmd.none
            )

        ShowSecondRowChanged newShowSecondRow ->
            ( { model | showSecondRow = newShowSecondRow }
            , Cmd.none
            )

        TextChanged newText ->
            ( { model | text = newText }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


input : String -> String -> List (H.Attribute msg) -> List (H.Attribute msg) -> H.Html msg
input id label containerAttrs inputAttrs =
    H.div containerAttrs
        [ H.label [ HA.class "block text-sm font-medium text-gray-700", HA.for id ]
            [ H.text label ]
        , H.div [ HA.class "mt-1 relative rounded-md shadow-sm" ]
            [ H.input
                ([ HA.class "focus:ring-blue-500 focus:border-blue-500 block w-full sm:text-sm border-gray-300 rounded-md"
                 , HA.id id
                 , HA.name id
                 ]
                    ++ inputAttrs
                )
                []
            ]
        ]


select : String -> String -> List (H.Attribute msg) -> List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg
select id label containerAttrs selectAttrs options =
    H.div containerAttrs
        [ H.label [ HA.class "block text-sm font-medium text-gray-700", HA.for id ]
            [ H.text label ]
        , H.div [ HA.class "mt-1 relative rounded-md shadow-sm" ]
            [ H.select
                ([ HA.class "focus:ring-blue-500 focus:border-blue-500 block w-full sm:text-sm border-gray-300 rounded-md"
                 , HA.id id
                 , HA.name id
                 ]
                    ++ selectAttrs
                )
                options
            ]
        ]


radio : String -> String -> List (H.Attribute msg) -> List (H.Attribute msg) -> H.Html msg
radio id label containerAttrs inputAttrs =
    H.div (HA.class "flex items-center" :: containerAttrs)
        [ H.input
            ([ HA.class "focus:ring-blue-500 h-4 w-4 text-blue-600 border-gray-300"
             , HA.id id
             , HA.type_ "radio"
             ]
                ++ inputAttrs
            )
            []
        , H.label
            [ HA.class "ml-3 block text-sm font-medium text-gray-700", HA.for id ]
            [ H.text (label ++ " ") ]
        ]


checkbox : String -> String -> List (H.Attribute msg) -> List (H.Attribute msg) -> H.Html msg
checkbox id label containerAttrs inputAttrs =
    H.div (HA.class "flex items-center" :: containerAttrs)
        [ H.input
            ([ HA.class "focus:ring-blue-500 h-4 w-4 text-blue-600 border-gray-300 rounded"
             , HA.id id
             , HA.type_ "checkbox"
             ]
                ++ inputAttrs
            )
            []
        , H.label
            [ HA.class "ml-3 block text-sm font-medium text-gray-700", HA.for id ]
            [ H.text (label ++ " ") ]
        ]


textarea : String -> String -> List (H.Attribute msg) -> List (H.Attribute msg) -> H.Html msg
textarea id label containerAttrs inputAttrs =
    H.div containerAttrs
        [ H.label [ HA.class "block text-sm font-medium text-gray-700", HA.for id ]
            [ H.text label ]
        , H.div [ HA.class "mt-1 relative rounded-md shadow-sm" ]
            [ H.textarea
                ([ HA.class "focus:ring-blue-500 focus:border-blue-500 block w-full sm:text-sm border-gray-300 rounded-md"
                 , HA.id id
                 , HA.name id
                 ]
                    ++ inputAttrs
                )
                []
            ]
        ]
