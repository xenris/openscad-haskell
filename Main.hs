module Main where

import OpenSCAD

main = writeFile "model.scad" (preview animation 40)

animation = model `rotate` (a, (v3 0 1 0)) `translate` (v3 0 0 10)
    where
        a = (pi / 6) * (sin $ TimeStep * (2 * pi))

model = pendulum `translate` (v3 0 0 (-10))

pendulum = stick +. weight

stick = cube `scale` (v3 1 1 10) `translate` (v3 0 0 5) `color` green

weight = sphere `scale` (v3 4 4 4) `color` (1, 0, 1, 1)
