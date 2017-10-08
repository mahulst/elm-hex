module Noisier exposing (scaled2D)

import Noise


type alias Rec =
    { maxAmplitude : Float
    , amplitude : Float
    , noise : Float
    , frequency : Float
    , iteration : Int
    }


octaves : Int
octaves =
    4


persistence : Float
persistence =
    0.5


scale : Float
scale =
    100


scaled2D : Noise.PermutationTable -> Rec -> Int -> Int -> Float
scaled2D table rec x y =
    let
        fX =
            (toFloat x) / scale

        fY =
            (toFloat y) / scale

        newNoise =
            (Noise.noise2d table (fX * rec.frequency) (fY * rec.frequency)) * rec.amplitude

        localMaxAmplitude =
            rec.maxAmplitude + rec.amplitude

        localAmplitude =
            rec.amplitude * persistence

        localFrequency =
            rec.frequency * 2

        localNoise =
            rec.noise + newNoise

        localIteration =
            rec.iteration + 1

        newRec =
            { maxAmplitude = localMaxAmplitude
            , amplitude = localAmplitude
            , noise = localNoise
            , frequency = localFrequency
            , iteration = localIteration
            }
    in
        if octaves == localIteration then
            (localNoise / localMaxAmplitude)
        else
            scaled2D table newRec x y
