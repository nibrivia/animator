module Math exposing (modByFloat, sign, interp)

modByFloat : Float -> Float -> Float
modByFloat modulo x =
    let
        n =
            floor (x / modulo)
    in
    x - (toFloat n * modulo)


sign : Float -> Float
sign x =
    if x == 0 then
        0

    else if x > 0 then
        1

    else
        -1


interp : Float -> Float -> Float -> Float
interp from to t =
    from * (1 - t) + to * t



