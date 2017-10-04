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
import Dict exposing (Dict)


---- MODEL ----


type alias Model =
    { chunks : Dict Int (Dict Int Chunk)
    , lastDelta : Float
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
    { mesh : () -> Mesh Vertex
    , position : Position
    }


seed =
    Random.initialSeed 1


floatList : Generator (List (List Float))
floatList =
    list 10 <| list 10 (float 0 1)


chunkWidth : Int
chunkWidth =
    32


init : ( Model, Cmd Msg )
init =
    let
        ( table, _ ) =
            Noise.permutationTable seed

        chunks =
            insertMultipleChunks table (Position 0 0) 0 Dict.empty
    in
        ( { chunks = chunks
          , lastDelta = 0
          , camOffset = vec3 0 0 0
          , seed = seed
          , table = table
          }
        , Cmd.batch [ send (DeltaTime (Time.inMilliseconds 0)) ]
        )


getNewChunk : Noise.PermutationTable -> Position -> Chunk
getNewChunk table worldPosition =
    let
        getNoise : Noise.PermutationTable -> Position -> Position -> Float
        getNoise table offset pos =
            (Noise.noise2d table (toFloat (offset.x * chunkWidth + pos.x)) (toFloat (offset.y * chunkWidth + pos.y))) * 4

        gridHeight : Array (Array Float)
        gridHeight =
            List.range -1 chunkWidth
                |> List.map
                    (\x ->
                        List.range -1 chunkWidth
                            |> List.map (\y -> (getNoise table worldPosition (Position x y)))
                            |> Array.fromList
                    )
                |> Array.fromList

        chunk =
            { mesh = always (hex2 gridHeight)
            , position = worldPosition
            }
    in
        chunk



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


insertMultipleChunks : Noise.PermutationTable -> Position -> Int -> Dict Int (Dict Int Chunk) -> Dict Int (Dict Int Chunk)
insertMultipleChunks table pos range chunks =
    let
        insert =
            insertNewChunk table
    in
        chunks
            |> (insert (Position (pos.x - 1) (pos.y - 1)))
            |> (insert (Position (pos.x) (pos.y - 1)))
            |> (insert (Position (pos.x + 1) (pos.y - 1)))
            |> (insert (Position (pos.x - 1) (pos.y)))
            |> (insert (Position (pos.x) (pos.y)))
            |> (insert (Position (pos.x + 1) (pos.y)))
            |> (insert (Position (pos.x - 1) (pos.y + 1)))
            |> (insert (Position (pos.x) (pos.y + 1)))
            |> (insert (Position (pos.x + 1) (pos.y + 1)))


insertNewChunk : Noise.PermutationTable -> Position -> Dict Int (Dict Int Chunk) -> Dict Int (Dict Int Chunk)
insertNewChunk table pos chunks =
    case Dict.get pos.x chunks of
        Just row ->
            case Dict.get pos.y row of
                Just chunk ->
                    chunks

                Nothing ->
                    let
                        chunk =
                            getNewChunk table pos

                        newRow =
                            Dict.insert pos.y chunk row
                    in
                        Dict.insert
                            pos.x
                            newRow
                            chunks

        Nothing ->
            let
                chunk =
                    getNewChunk
                        table
                        pos

                row =
                    Dict.insert pos.y chunk Dict.empty
            in
                Dict.insert pos.x row chunks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeltaTime time ->
            ( { model | lastDelta = time }, Cmd.none )

        KeyMsg code ->
            let
                newOffset camOffset =
                    let
                        position =
                            (findClosestPoint model.camOffset)

                        chunks =
                            insertMultipleChunks model.table position 0 model.chunks
                    in
                        ( { model | camOffset = camOffset, chunks = chunks }, Cmd.none )
            in
                case code of
                    37 ->
                        newOffset (Vec3.add model.camOffset (vec3 -10 0 0))

                    38 ->
                        newOffset (Vec3.add model.camOffset (vec3 0 0 -10))

                    39 ->
                        newOffset (Vec3.add model.camOffset (vec3 10 0 0))

                    40 ->
                        newOffset (Vec3.add model.camOffset (vec3 0 0 10))

                    32 ->
                        let
                            newChunks =
                                case Dict.get 0 model.chunks of
                                    Just chunks ->
                                        let
                                            newChunk =
                                                getNewChunk model.table (Position 1 0)
                                        in
                                            Dict.insert 0 newChunk chunks

                                    Nothing ->
                                        Dict.empty

                            setChunks =
                                Dict.insert 1 newChunks model.chunks
                        in
                            ( { model | chunks = setChunks }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )


findClosestPoint : Vec3 -> Position
findClosestPoint pos =
    let
        x =
            Vec3.getX pos

        y =
            Vec3.getZ pos

        floatChunkWidth =
            toFloat chunkWidth

        x2 =
            x / floatChunkWidth / 2

        y2 =
            y / floatChunkWidth / 1.5
    in
        Position (round x2) (round y2)



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
            (Chunk (always (WebGL.triangles [])) (Position 0 0))

        chunk =
            case Dict.get 0 model.chunks of
                Just chunks ->
                    case Dict.get 0 chunks of
                        Just chunk ->
                            chunk

                        Nothing ->
                            defaultChunk

                Nothing ->
                    defaultChunk

        chunks : List Chunk
        chunks =
            Dict.toList model.chunks
                |> List.map (\( _, chunks ) -> Dict.toList chunks |> List.map (\( _, chunk ) -> chunk))
                |> List.concat
    in
        Html.div []
            [ Html.div []
                [ Html.text (toString (round (1000 / model.lastDelta))) ]
            , WebGL.toHtml
                [ Html.Attributes.width 1000
                , Html.Attributes.height 1000
                , Html.Attributes.style [ ( "display", "block" ) ]
                ]
                ((List.map (hexView model.camOffset) chunks)
                    ++ [ (waterView model.camOffset) ]
                )
            ]


hexView : Vec3 -> Chunk -> WebGL.Entity
hexView camOffset chunk =
    WebGL.entity
        vertexShader
        fragmentShader
        (chunk.mesh ())
        (uniforms camOffset chunk.position)


waterView : Vec3 -> WebGL.Entity
waterView camOffset =
    WebGL.entity
        vertexShader
        fragmentShader
        waterMesh
        (waterUniforms camOffset)


waterHeight : Float
waterHeight =
    0


waterMesh : Mesh Vertex
waterMesh =
    let
        bl =
            vec3 -1000 waterHeight 1000

        tr =
            vec3 1000 waterHeight -1000

        tl =
            vec3 -1000 waterHeight -1000

        br =
            vec3 1000 -0.6 1000
    in
        [ attributes bl tr tl
        , attributes bl br tr
        ]
            |> WebGL.triangles


waterUniforms : Vec3 -> Uniforms
waterUniforms camOffset =
    { rotation = Mat4.identity
    , perspective = perspective
    , camera = camera camOffset
    , color = vec3 0.1 0.25 0.8
    , light = light
    }


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 100


initialPos =
    vec3 2 80 5


camera : Vec3 -> Mat4
camera camOffset =
    Mat4.makeLookAt (Vec3.add camOffset initialPos) (Vec3.add camOffset (vec3 2 0 3)) (vec3 0 1 0)


uniforms : Vec3 -> Position -> Uniforms
uniforms camOffset position =
    { rotation = Mat4.translate (getChunkOffset position) Mat4.identity
    , perspective = perspective
    , camera = camera camOffset
    , color = vec3 0.1 0.25 0.08
    , light = light
    }


getChunkOffset : Position -> Vec3
getChunkOffset pos =
    vec3 (toFloat pos.x * ((toFloat chunkWidth) * 2)) 0 (toFloat pos.y * ((toFloat chunkWidth) * 1.5))


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


getHeightFromPos : Array (Array Float) -> Position -> Float
getHeightFromPos grid pos =
    Array.get pos.x grid |> Maybe.andThen (Array.get pos.y) |> Maybe.withDefault 100



-- Mesh


hex2 : Array (Array Float) -> Mesh Vertex
hex2 gridHeight =
    let
        fn1 : Int -> Array Float -> List (List ( Vertex, Vertex, Vertex ))
        fn1 x row =
            Array.slice 1 ((Array.length row) - 1) row
                |> Array.indexedMap (fn2 x)
                |> Array.toList

        fn2 : Int -> Int -> Float -> List ( Vertex, Vertex, Vertex )
        fn2 x y height =
            hex gridHeight (Position (x + 1) (y + 1))
    in
        Array.slice 1 ((Array.length gridHeight) - 1) gridHeight
            |> Array.indexedMap fn1
            |> Array.toList
            |> List.concat
            |> List.concat
            |> WebGL.triangles


hex : Array (Array Float) -> Position -> List ( Vertex, Vertex, Vertex )
hex gridHeight { x, y } =
    let
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

        getAverageHeight h1 h2 h3 =
            (h1 + h2 + h3) / 3

        height : Float
        height =
            getHeightFromPos gridHeight (Position x y)

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
            Vec3.add translatedXY (vec3 -1 (getAverageHeight height heightWest heightNorthWest) -0.5)

        p1 =
            Vec3.add translatedXY (vec3 -1 (getAverageHeight height heightSouthWest heightWest) 0.5)

        p2 =
            Vec3.add translatedXY (vec3 0 (getAverageHeight height heightSouthWest heightSouthEast) 1)

        p3 =
            Vec3.add translatedXY (vec3 1 (getAverageHeight height heightEast heightSouthEast) 0.5)

        p4 =
            Vec3.add translatedXY (vec3 1 (getAverageHeight height heightEast heightNorthEast) -0.5)

        p5 =
            Vec3.add translatedXY (vec3 0 (getAverageHeight height heightNorthWest heightNorthEast) -1)

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
