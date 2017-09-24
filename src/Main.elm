module Main exposing (..)

import AnimationFrame
import Html exposing (Html, text, div, img)
import Html.Lazy
import Html.Attributes exposing (src)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3, toTuple)
import Math.Vector4 as Vec4
import WebGL exposing (Mesh, Shader)
import Time exposing (Time)
import Task


---- MODEL ----


type alias Model =
    { theta : Float
    , grid : List Position
    , lastDelta : Float
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


init : ( Model, Cmd Msg )
init =
    ( { theta = 0
      , grid = List.range 0 100 |> List.map (\x -> { x = rem x 10, y = x // 10 })
      , lastDelta = 0
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


type alias Position =
    { x : Int
    , y : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeltaTime time ->
            ( { model | lastDelta = time }, Cmd.none )



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
        [ AnimationFrame.diffs DeltaTime ]



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.Lazy.lazy
        (\grid ->
            WebGL.toHtml
                [ Html.Attributes.width 1000
                , Html.Attributes.height 1000
                , Html.Attributes.style [ ( "display", "block" ) ]
                ]
                (List.map hexView grid)
        )
        model.grid


hexView : Position -> WebGL.Entity
hexView pos =
    WebGL.entity
        vertexShader
        fragmentShader
        (hex pos)
        uniforms


perspective : Mat4
perspective =
    Mat4.makePerspective 45 1 0.01 50


cameraPos : Vec3
cameraPos =
    vec3 0 8 5


camera : Mat4
camera =
    Mat4.makeLookAt cameraPos (vec3 0 0 0) (vec3 0 1 0)


uniforms : Uniforms
uniforms =
    { rotation = Mat4.identity
    , perspective = perspective
    , camera = camera
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



-- Mesh


hex : Position -> Mesh Vertex
hex { x, y } =
    let
        width =
            10

        height =
            0.8

        p0 =
            vec3 -1 0 -0.5

        p1 =
            vec3 -1 0 0.5

        p2 =
            vec3 0 0 1

        p3 =
            vec3 1 0 0.5

        p4 =
            vec3 1 0 -0.5

        p5 =
            vec3 0 0 -1

        p6 =
            vec3 0 height 0
    in
        [ attributes p0 p6 p1
        , attributes p1 p6 p2
        , attributes p2 p6 p3
        , attributes p3 p6 p4
        , attributes p4 p6 p5
        , attributes p5 p6 p0
        ]
            |> WebGL.triangles


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
