module Main exposing (..)

import AnimationFrame
import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, toTuple)
import Math.Vector4 as Vec4
import WebGL exposing (Mesh, Shader)
import Time exposing (Time)
import Task
import Array exposing (Array)
import Keyboard
import Random exposing (Generator, list, float)
import Noise


---- MODEL ----


type alias Model =
    { chunks : Array (Array Chunk)
    , lastDelta : Float
    , gridHeight : Array Float
    , camOffset : Vec3
    , seed : Random.Seed
    , table : Noise.PermutationTable
    }


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , color : Vec3
    , light : Vec3
    }


type alias Chunk =
    { gridHeight : Array Float
    , mesh : Mesh Vertex
    , position : Position
    }


seed =
    Random.initialSeed 1


floatList : Generator (List (List Float))
floatList =
    list 10 <| list 10 (float 0 1)


chunkWidth : Int
chunkWidth =
    8


init : ( Model, Cmd Msg )
init =
    let
        getNoise table i =
            let
                x =
                    rem i chunkWidth

                y =
                    i // chunkWidth
            in
                (Noise.noise2d table (toFloat x) (toFloat y)) * 4

        ( table, _ ) =
            Noise.permutationTable seed

        gridHeight =
            List.range 0 100 |> List.map (getNoise table) |> Array.fromList

        chunk =
            { mesh = List.range 0 63 |> List.map (hex gridHeight) |> List.concat |> WebGL.triangles
            , gridHeight = gridHeight
            , position = Position 0 0
            }
    in
        ( { chunks = Array.fromList [ Array.fromList [ chunk ] ]
          , lastDelta = 0
          , gridHeight = gridHeight
          , camOffset = vec3 0 0 0
          , seed = seed
          , table = table
          }
        , Cmd.batch [ send (DeltaTime (Time.inMilliseconds 0)) ]
        )



{- Send initial message to update drawing -}


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity



---- UPDATE ----


type Msg
    = DeltaTime Time
    | KeyMsg Keyboard.KeyCode


type alias Position =
    { x : Int
    , y : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeltaTime time ->
            ( { model | lastDelta = time }, Cmd.none )

        KeyMsg code ->
            let
                newOffset camOffset =
                    ( { model | camOffset = camOffset }, Cmd.none )
            in
                case code of
                    37 ->
                        newOffset (Vec3.add model.camOffset (vec3 -1 0 0))

                    38 ->
                        newOffset (Vec3.add model.camOffset (vec3 0 0 -1))

                    39 ->
                        newOffset (Vec3.add model.camOffset (vec3 1 0 0))

                    40 ->
                        newOffset (Vec3.add model.camOffset (vec3 0 0 1))

                    32 ->
                        let
                            newChunks =
                                case Array.get 0 model.chunks of
                                    Just chunks ->
                                        let
                                            newChunk =
                                                getNewChunk model
                                        in
                                            Array.push newChunk chunks

                                    Nothing ->
                                        Array.empty

                            setChunks =
                                Array.set 0 newChunks model.chunks
                        in
                            ( { model | chunks = setChunks }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )


getNewChunk : Model -> Chunk
getNewChunk model =
    let
        getNoise table offset i =
            let
                x =
                    rem i (chunkWidth * (offset + 1))

                y =
                    i // (chunkWidth * (offset + 1))
            in
                (Noise.noise2d table (toFloat x) (toFloat y)) * 4

        gridHeight =
            List.range 0 100 |> List.map (getNoise model.table 1) |> Array.fromList

        chunk =
            { mesh = List.range 0 63 |> List.map (hex gridHeight) |> List.concat |> WebGL.triangles
            , gridHeight = gridHeight
            , position = Position 0 0
            }
    in
        chunk



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs DeltaTime
        , Keyboard.downs KeyMsg
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        defaultChunk =
            (Chunk (Array.empty) (WebGL.triangles []) (Position 0 0))

        chunk =
            case Array.get 0 model.chunks of
                Just chunks ->
                    case Array.get 0 chunks of
                        Just chunk ->
                            chunk

                        Nothing ->
                            defaultChunk

                Nothing ->
                    defaultChunk
    in
        WebGL.toHtml
            [ Html.Attributes.width 1000
            , Html.Attributes.height 1000
            , Html.Attributes.style [ ( "display", "block" ) ]
            ]
            [ (hexView model.camOffset chunk.mesh) ]


hexView : Vec3 -> Mesh Vertex -> WebGL.Entity
hexView camOffset hex2 =
    WebGL.entity
        vertexShader
        fragmentShader
        hex2
        (uniforms camOffset)


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 50


initialPos =
    vec3 2 18 5


camera : Vec3 -> Mat4
camera camOffset =
    Mat4.makeLookAt (Vec3.add camOffset initialPos) (Vec3.add camOffset (vec3 2 0 3)) (vec3 0 1 0)


uniforms : Vec3 -> Uniforms
uniforms camOffset =
    { rotation = Mat4.identity
    , perspective = perspective
    , camera = camera camOffset
    , color = vec3 0.4 0.4 0.4
    , light = light
    }


{-| Adds a normal to the vertex
-}
attributes : Vec3 -> Vec3 -> Vec3 -> ( Vertex, Vertex, Vertex )
attributes p1 p2 p3 =
    let
        normal =
            Vec3.cross (Vec3.sub p1 p2) (Vec3.sub p1 p3) |> Vec3.normalize
    in
        ( Vertex p1 normal, Vertex p2 normal, Vertex p3 normal )


type alias Cube =
    { x : Int
    , y : Int
    , z : Int
    }


oddrToCube : Position -> Cube
oddrToCube pos =
    let
        x =
            (toFloat pos.x) - (toFloat (pos.y - (pos.y % 2))) / 2

        z =
            pos.y

        y =
            -x - (toFloat z)
    in
        Cube (floor x) (floor y) z


cubeToOddr : Cube -> Position
cubeToOddr cube =
    let
        x =
            (toFloat cube.x) + (toFloat (cube.z - (cube.z % 2))) / 2

        y =
            cube.z
    in
        Position (floor x) y


eastCube : Cube -> Cube
eastCube { x, y, z } =
    Cube (x + 1) (y - 1) z


northEastCube : Cube -> Cube
northEastCube { x, y, z } =
    Cube (x + 1) y (z - 1)


southEastCube : Cube -> Cube
southEastCube { x, y, z } =
    Cube x (y - 1) (z + 1)


westCube : Cube -> Cube
westCube { x, y, z } =
    Cube (x - 1) (y + 1) z


northWestCube : Cube -> Cube
northWestCube { x, y, z } =
    Cube x (y + 1) (z - 1)


southWestCube : Cube -> Cube
southWestCube { x, y, z } =
    Cube (x - 1) y (z + 1)


getPositionOfNeighbour : Position -> (Cube -> Cube) -> Position
getPositionOfNeighbour pos neighbour =
    oddrToCube pos
        |> neighbour
        |> cubeToOddr


getHeightFromPos : Array Float -> Position -> Float
getHeightFromPos grid pos =
    Array.get (pos.y * chunkWidth + pos.x + chunkWidth + 1) grid
        |> Maybe.withDefault 0



-- Mesh


hex : Array Float -> Int -> List ( Vertex, Vertex, Vertex )
hex gridHeight i =
    let
        x =
            rem i chunkWidth

        y =
            i // chunkWidth

        translatedX =
            vec3 ((toFloat x) * 2) 0 0

        translatedY =
            vec3
                (if rem y 2 == 0 then
                    0
                 else
                    1
                )
                0
                ((toFloat y) * 1.5)

        translatedXY =
            Vec3.add translatedX translatedY

        width =
            10

        height =
            Maybe.withDefault 0 (Array.get (i + chunkWidth + 1) gridHeight)

        getNeighbour =
            getPositionOfNeighbour (Position x y)

        getHeight =
            getHeightFromPos gridHeight

        heightEast =
            getNeighbour eastCube
                |> getHeight

        heightNorthEast =
            getNeighbour northEastCube
                |> getHeight

        heightSouthEast =
            getNeighbour southEastCube
                |> getHeight

        heightSouthWest =
            getNeighbour southWestCube
                |> getHeight

        heightWest =
            getNeighbour westCube
                |> getHeight

        heightNorthWest =
            getNeighbour northWestCube
                |> getHeight

        p0 =
            Vec3.add translatedXY (vec3 -1 ((height + heightWest + heightNorthWest) / 3) -0.5)

        p1 =
            Vec3.add translatedXY (vec3 -1 ((height + heightSouthWest + heightWest) / 3) 0.5)

        p2 =
            Vec3.add translatedXY (vec3 0 ((height + heightSouthWest + heightSouthEast) / 3) 1)

        p3 =
            Vec3.add translatedXY (vec3 1 ((height + heightEast + heightSouthEast) / 3) 0.5)

        p4 =
            Vec3.add translatedXY (vec3 1 ((height + heightEast + heightNorthEast) / 3) -0.5)

        p5 =
            Vec3.add translatedXY (vec3 0 ((height + heightNorthWest + heightNorthEast) / 3) -1)

        p6 =
            Vec3.add translatedXY (vec3 0 height 0)
    in
        [ attributes p0 p6 p1
        , attributes p1 p6 p2
        , attributes p2 p6 p3
        , attributes p3 p6 p4
        , attributes p4 p6 p5
        , attributes p5 p6 p0
        ]


light : Vec3
light =
    vec3 -1.0 -8.0 -3.0 |> Vec3.normalize



-- Mesh


type alias Varying =
    { vlighting : Float
    }



-- Shaders


vertexShader : Shader Vertex Uniforms Varying
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 perspective;
        uniform mat4 rotation;
        uniform mat4 camera;
        uniform vec3 light;
        varying highp float vlighting;
        void main () {
            highp float ambientLight = 0.5;
            highp float directionalLight = 1.0;
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vlighting = ambientLight + max(dot(normal, light), 0.0) * directionalLight;

        }
    |]


fragmentShader : Shader {} Uniforms Varying
fragmentShader =
    [glsl|
        precision mediump float;
        varying highp float vlighting;
        uniform vec3 color;
        void main () {
            gl_FragColor = vec4(color * vlighting, 1.0);
        }
    |]


mulVector : Mat4 -> Vec4.Vec4 -> Vec4.Vec4
mulVector mat v =
    let
        rec =
            Mat4.toRecord mat

        r1 =
            Vec4.vec4 rec.m11 rec.m12 rec.m13 rec.m14

        r2 =
            Vec4.vec4 rec.m21 rec.m22 rec.m23 rec.m24

        r3 =
            Vec4.vec4 rec.m31 rec.m32 rec.m33 rec.m34

        r4 =
            Vec4.vec4 rec.m41 rec.m42 rec.m43 rec.m44
    in
        Vec4.vec4 (Vec4.dot r1 v) (Vec4.dot r2 v) (Vec4.dot r3 v) (Vec4.dot r4 v)
