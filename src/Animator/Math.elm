module Animator.Math exposing (modByFloat, sign, interp)

{-|
Small math helpers for making animations.

@docs modByFloat, sign, interp
-}

{-|
Takes a modulo, but is accepting of floats.

    modByFloat 3.0 9.1 == 0.1
-}
modByFloat : Float -> Float -> Float
modByFloat modulo x =
    let
        n =
            floor (x / modulo)
    in
    x - (toFloat n * modulo)


{-| Returns the sign of the number

    sign 0 == 0
    sign 3 == 1
    sign -5 == -1
-}
sign : Float -> Float
sign x =
    if x == 0 then
        0

    else if x > 0 then
        1

    else
        -1


{-| Linearly interpolates between the first two numbers.

    -- interp start end 0 == start
    -- interp start end 1 == end

    interp 0 10 0.3 == 3
    interp 0 100 -2 == -200
-}
interp : Float -> Float -> Float -> Float
interp from to t =
    from * (1 - t) + to * t



