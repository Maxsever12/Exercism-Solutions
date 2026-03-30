module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = (seconds/31557600) / (divisor planet)

divisor :: Planet -> Float
divisor Mercury = 0.2408467
divisor Venus = 0.61519726
divisor Earth = 1
divisor Mars = 1.8808158
divisor Jupiter = 11.862615
divisor Saturn = 29.447498
divisor Uranus = 84.016846
divisor Neptune = 164.79132