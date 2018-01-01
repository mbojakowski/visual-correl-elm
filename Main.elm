import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Tuple exposing (first, second)
import Mouse

main =
  Html.program {
    init = ({
      origin = { x = 50, y = 580, id = 10000 },
      data = defaultData,
      width = 1200,
      height = 600,
      selectedPoint = Nothing
    }, Cmd.none),
    view = view,
    update = update,
    subscriptions = subscriptions
  }

type alias Point = { id: Int, x: Int, y: Int }
type alias Chart = { origin: Point, data: List Point, width: Int, height: Int, selectedPoint: Maybe Int }
type Message = SelectPoint Int | DragAt Mouse.Position | DragEnd Mouse.Position

subscriptions : Chart -> Sub Message
subscriptions chart = case chart.selectedPoint of
  Just id -> Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
  Nothing -> Sub.none

update : Message -> Chart -> (Chart, Cmd Message)
update message chart =
  case message of
    SelectPoint pointId -> ({ chart | selectedPoint = Just pointId }, Cmd.none)
    DragAt position -> case chart.selectedPoint of
      Just pointId -> (updatePosition chart pointId (toChartPosition chart position), Cmd.none)
      Nothing -> (chart, Cmd.none)
    DragEnd _ -> ({ chart | selectedPoint = Nothing }, Cmd.none)

defaultData : List Point
defaultData =
  List.map (\n -> { id = n, x = 100 + n * 20, y = 100 + n * 20 }) (List.range 1 15)

toChartPosition : Chart -> Mouse.Position -> Point
toChartPosition chart mousePosition =
  -- assumes the SVG upper left corner is the window's upper left corner
  { x = mousePosition.x - chart.origin.x, y = chart.height - mousePosition.y, id = -1 }

updatePosition : Chart -> Int -> Point -> Chart
updatePosition chart movedPointId targetPoint =
    {chart | data = (chart.data |> List.filter (\point -> point.id /= movedPointId))
    ++ [{ targetPoint | id = movedPointId }]}

variance : Chart -> (Float, Float)
variance chart =
  let
    meanPoint = mean chart

    var : (Point -> Int) -> Float
    var coordinate = toFloat (List.sum (List.map (\point -> (((coordinate point) - (coordinate meanPoint))^2)) chart.data)) / toFloat (List.length chart.data)
  in
    (var .x, var .y)

stdDev : Chart -> (Float, Float)
stdDev chart =
  let
    var = variance chart
  in
  (var |> first |> sqrt, var |> second |> sqrt)

mean : Chart -> Point
mean chart =
  let
    -- we should ideally keep the exact values
    meanX = (List.sum (List.map (\point -> point.x) chart.data)) // (List.length chart.data)
    meanY = (List.sum (List.map (\point -> point.y) chart.data)) // (List.length chart.data)
  in
    { x = meanX, y = meanY, id = 9000 }

covar : Chart -> Float
covar chart =
  let
    meanPoint = mean chart
  in
    toFloat (List.sum (List.map (\point -> (point.x - meanPoint.x) * (point.y - meanPoint.x)) chart.data)) / toFloat (List.length chart.data)

correl : Chart -> Float
correl chart =
  let
    stdd = stdDev chart
  in
    (covar chart) / ((first stdd) * (second stdd))

chartAxis : Chart -> List (Svg Message)
chartAxis chart =
  let
    x = chart.origin.x
    y = chart.origin.y
  in
    [
      line [ x1 (toString x), y1 (toString y), x2 (toString x), y2 "5", Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2" ] [],
      line [ x1 (toString x), y1 (toString y), x2 (toString(chart.width - x)), y2 (toString y), Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2" ] []
    ]

svgCircle : Point -> Point -> Svg Message
svgCircle origin point =
    circle [
      cx (toString (origin.x + point.x)),
      cy (toString (origin.y - point.y)),
      r "5" ,
      Svg.Attributes.cursor "pointer",
      Svg.Events.onMouseDown (SelectPoint point.id)
    ] []

circles : Chart -> List (Svg Message)
circles chart =
    List.map (svgCircle chart.origin) chart.data

deviationRectangle : Chart -> Point -> Svg Message
deviationRectangle chart point =
  let
    meanPoint = mean chart
    deviationX = point.x - meanPoint.x
    deviationY = point.y - meanPoint.y
    strokeColor = if (deviationX * deviationY > 0) then "green" else "red"
  in
    rect [
      x (toString (chart.origin.x + Basics.min point.x meanPoint.x)),
      y (toString (chart.origin.y - Basics.max point.y meanPoint.y)),
      width (toString (abs (point.x - meanPoint.x))),
      height (toString (abs (point.y - meanPoint.y))),
      Svg.Attributes.style ("fill:none;stroke:" ++ strokeColor)
    ] []

rectangles : Chart -> List (Svg Message)
rectangles chart =
    List.map (deviationRectangle chart) chart.data

meanCircle : Chart -> Svg Message
meanCircle chart =
  let
    meanPoint = mean chart
  in
    circle [
      cx (toString (chart.origin.x + meanPoint.x)),
      cy (toString (chart.origin.y - meanPoint.y)),
      r "5",
      Svg.Attributes.style "fill:red"
    ] []

correlationBox : Chart -> List (Svg Message)
correlationBox chart =
  let
    rectX = chart.width - 300
    firstSquareHeight = round (100 + 100 * (abs (correl chart)))
    firstSquareColor = if (correl chart) >= 0 then "green" else "red"
    secondSquareColor = if (firstSquareColor == "green") then "red" else "green"
  in
    [
      rect [ x (toString rectX), y "300", width "200", height "200", Svg.Attributes.style "fill:none;stroke:black" ] [],
      rect [ x (toString rectX), y "300", width "200", height (toString firstSquareHeight), Svg.Attributes.style ("fill:"++firstSquareColor++";stroke:black") ] [],
      rect [ x (toString rectX), y (toString (300 + firstSquareHeight)), width "200", height (toString (200 - firstSquareHeight)), Svg.Attributes.style ("fill:"++secondSquareColor++";stroke:black") ] [],
      Svg.text_ [ x "710", y "400" ] [ Svg.text ("Correlation = " ++ (correl chart |> toString |> String.left 4)) ]
    ]

legend : List (Svg Message)
legend =
  let
    (legendSvgX, legendSvgY) = (900, 100)
  in
    [
      circle [ cx (toString legendSvgX), cy (toString legendSvgY), r "5", Svg.Attributes.style "fill:red" ] [],
      Svg.text_ [ x (toString (legendSvgX + 10)), y (toString (legendSvgY + 5)) ] [ Svg.text "mean" ],
      circle [ cx (toString legendSvgX), cy (toString (legendSvgY + 20)), r "5", Svg.Attributes.style "fill:black" ] [],
      Svg.text_ [ x (toString (legendSvgX + 10)), y (toString (legendSvgY + 25)) ] [ Svg.text "data point" ]
    ]

draw : Chart -> Html Message
draw chart = svg
  [ width "1200", height "600", viewBox "0 0 1200 600" ]
  ((chartAxis chart) ++ (circles chart) ++ legend ++ (rectangles chart) ++ [meanCircle chart] ++ (correlationBox chart))

view : Chart -> Html Message
view chart =
  draw chart
