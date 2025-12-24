module Animator exposing (..)


modBy : Float -> Float -> Float
modBy modulo x =
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


clamp : Float -> Float -> Float -> Float
clamp small big x =
    x
        |> min big
        |> max small


type Animation a
    = Animation { end : Float, fn : Float -> a }


withNoBounds : a -> Animation a
withNoBounds val =
    withDuration 0 (\_ -> val)


startWith : a -> Animation a -> Animation a
startWith initial (Animation animData) =
    Animation
        { animData
            | fn =
                \t ->
                    if t < 0 then
                        initial

                    else
                        animData.fn t
        }


endWith : a -> Animation a -> Animation a
endWith final (Animation animData) =
    Animation
        { animData
            | fn =
                \t ->
                    if t >= animData.end then
                        final

                    else
                        animData.fn t
        }


oobValue : a -> Animation a -> Animation a
oobValue val (Animation animData) =
    Animation
        { animData
            | fn =
                \t ->
                    if t < 0 || t > animData.end then
                        val

                    else
                        animData.fn t
        }


clampAnim : Animation a -> Animation a
clampAnim (Animation ({ end, fn } as animData)) =
    Animation { animData | fn = \t -> fn (clamp 0 end t) }


pause : a -> Float -> Animation a
pause val dur =
    withDuration dur (\_ -> val)


withDuration : Float -> (Float -> a) -> Animation a
withDuration dur fn =
    Animation { end = dur, fn = fn }


withUnitBounds : (Float -> a) -> Animation a
withUnitBounds =
    withDuration 1


render : Float -> Animation a -> a
render t (Animation { end, fn }) =
    fn t


duration : Animation a -> Float
duration (Animation { end }) =
    end


getEndState : Animation a -> a
getEndState anim =
    anim |> render (duration anim)


mapRes : (a -> b) -> Animation a -> Animation b
mapRes mapFn (Animation { end, fn }) =
    Animation { end = end, fn = fn >> mapFn }


delay : Float -> Animation a -> Animation a
delay amount (Animation { end, fn }) =
    Animation
        { end = end + amount
        , fn = \t -> fn (t - amount)
        }


reverse : Animation a -> Animation a
reverse (Animation { end, fn }) =
    Animation
        { end = end
        , fn = \t -> fn (end - t)
        }


cutAt : Float -> Animation a -> Animation a
cutAt newLength (Animation animData) =
    Animation { animData | end = newLength }


freezeAfter : Float -> Animation a -> Animation a
freezeAfter freezeTime (Animation animData) =
    Animation
        { animData
            | fn = \t -> min t freezeTime |> animData.fn
        }


speedup : Float -> Animation a -> Animation a
speedup factor (Animation { end, fn }) =
    Animation
        { end = end / factor
        , fn = \t -> t |> (*) factor |> fn
        }


fitIn : Float -> Animation a -> Animation a
fitIn newEnd anim =
    anim
        |> speedup (duration anim / newEnd)


loopForever : Animation a -> Animation a
loopForever (Animation { end, fn }) =
    Animation
        { end = 1 / 0 -- infinity
        , fn = \t -> t |> modBy end |> fn
        }


setDuration : Float -> Animation a -> Animation a
setDuration desiredLength anim =
    let
        curDuration =
            duration anim

        speedupFactor =
            curDuration / desiredLength
    in
    anim
        |> speedup speedupFactor
        -- explicitly set the length
        |> cutAt desiredLength


alongWith : (Float -> a -> b -> c) -> Animation a -> Animation b -> Animation c
alongWith combineFn (Animation first) (Animation second) =
    Animation
        { end = max first.end second.end
        , fn =
            \t ->
                combineFn t
                    (first.fn t)
                    (second.fn t)
        }


andAfter : Animation a -> Animation a -> Animation a
andAfter afterAnim beforeAnim =
    let
        cutTime =
            duration beforeAnim

        delayedAnimation =
            afterAnim
                |> delay cutTime

        combine t first second =
            if t < cutTime then
                first

            else
                second
    in
    alongWith combine beforeAnim delayedAnimation


together : List (Animation a) -> Animation (List a)
together =
    List.foldl
        (alongWith (\_ -> (::)))
        (withNoBounds [])


sequence : a -> List (Animation a) -> Animation a
sequence init =
    List.foldl
        andAfter
        (withNoBounds init)
