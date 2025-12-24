module Animator exposing
    ( withNoBounds, withUnitBounds, withDuration
    , render
    , duration, getEndState
    , startWith, endWith, oobValue, clampAnim
    , reverse, loopForever
    , delay, cutAt, freezeAfter
    , speedup, fitIn
    , mapRes
    , andAfter, alongWith
    , together, sequence
    )

{-|


## Create an animation

@docs withNoBounds, withUnitBounds, withDuration


## Run the animation

@docs render


## Get information about an animation

@docs duration, getEndState


## Modify an animation


### Initial and final states

@docs startWith, endWith, oobValue, clampAnim


### Modifications of individual animations

@docs reverse, loopForever

@docs delay, cutAt, freezeAfter

@docs speedup, fitIn

@docs mapRes


## Compose animations


### Just two animations

@docs andAfter, alongWith


### Many animations

@docs together, sequence

-}

import Math exposing (interp, modByFloat, sign)


type Animation a
    = Animation { end : Float, fn : Float -> a }


{-| Most general constructor for an `Animation`.
-}
withDuration : Float -> (Float -> a) -> Animation a
withDuration dur fn =
    Animation { end = dur, fn = fn }


{-| Probably what wants to be used by default, expects an animation from 0 to 1.
-}
withUnitBounds : (Float -> a) -> Animation a
withUnitBounds =
    withDuration 1


{-| Create a constant animation of 0-duration
-}
withNoBounds : a -> Animation a
withNoBounds val =
    withDuration 0 (\_ -> val)



-- constant : a -> Float -> Animation a
-- constant val dur =
--     withDuration dur (\_ -> val)


{-| Actually "runs" the animation at the given time.
-}
render : Float -> Animation a -> a
render t (Animation { end, fn }) =
    fn t


{-| Force the animation's initial value to the given argument
-}
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


{-| Force the animation's final value to the given argument
-}
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


{-| Force the animation's initial and final values to the given argument
-}
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


{-| Force the animation's initial and final values to what they would be at the start and end.

By default, animations can "leak" out of their bounds. This makes it so that when the animation is rendered "beyond" its bounds, it "holds" the value it ends with.

-}
clampAnim : Animation a -> Animation a
clampAnim (Animation ({ end, fn } as animData)) =
    Animation { animData | fn = \t -> fn (clamp 0 end t) }


{-| Get the duration of an existing animation
-}
duration : Animation a -> Float
duration (Animation { end }) =
    end


{-| Get the final state of an existing animation
-}
getEndState : Animation a -> a
getEndState anim =
    anim |> render (duration anim)


{-| Modifies the output of the animation
-}
mapRes : (a -> b) -> Animation a -> Animation b
mapRes mapFn (Animation { end, fn }) =
    Animation { end = end, fn = fn >> mapFn }


{-| Delays the start of an animation by the given time
-}
delay : Float -> Animation a -> Animation a
delay amount (Animation { end, fn }) =
    Animation
        { end = end + amount
        , fn = \t -> fn (t - amount)
        }


{-| Reverse the animation entirely
-}
reverse : Animation a -> Animation a
reverse (Animation { end, fn }) =
    Animation
        { end = end
        , fn = \t -> fn (end - t)
        }


{-| Effectively sets the duration of the animation to be shorter.

This might not behave how you expect it: animations can keep going past their end, you might want to use `freezeAfter` if you want the animation to actually stop.

-}
cutAt : Float -> Animation a -> Animation a
cutAt newLength (Animation animData) =
    Animation { animData | end = newLength }


{-| Stops the animation after the given time.
-}
freezeAfter : Float -> Animation a -> Animation a
freezeAfter freezeTime (Animation animData) =
    Animation
        { animData
            | fn = \t -> min t freezeTime |> animData.fn
        }


{-| Speeds up the animation by a desired factor
-}
speedup : Float -> Animation a -> Animation a
speedup factor (Animation { end, fn }) =
    Animation
        { end = end / factor
        , fn = \t -> t |> (*) factor |> fn
        }


{-| Sets an duration of `Infinity` and forever loops the animation
-}
loopForever : Animation a -> Animation a
loopForever (Animation { end, fn }) =
    Animation
        { end = 1 / 0 -- infinity
        , fn = \t -> t |> modByFloat end |> fn
        }


{-| Speeds up the animation to fit within the given time
-}
fitIn : Float -> Animation a -> Animation a
fitIn newEnd anim =
    anim
        |> speedup (duration anim / newEnd)


{-| Most general combination of two `Animation`s.

You probably want to narrow this down for your specific domain.

-}
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


{-| Takes two animations and plays them one after the other.

The order here is such that it's convenient when using a pipe:

    beforAnimation
        |> andAfter afterAnimation

-}
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


{-| Plays a list of `Animation`s together.

Almost certainly more useful narrowed to a specific domain. Use `mapRes` to turn `List a` into something more helpful.

-}
together : List (Animation a) -> Animation (List a)
together =
    List.foldl
        (alongWith (\_ -> (::)))
        (withNoBounds [])


{-| With an initial value, plays a list of `Animation` in sequence.
-}
sequence : a -> List (Animation a) -> Animation a
sequence init =
    List.foldl
        andAfter
        (withNoBounds init)
