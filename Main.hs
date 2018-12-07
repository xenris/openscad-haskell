module Main where

import OpenSCAD

main = writeFile "model.scad" (preview animation 40)

animation = model `rotate` (a, (p3 0 1 0)) `translate` (p3 0 0 10)
    where
        a = (pi / 6) * (sin $ TimeStep * (2 * pi))

model = pendulum `translate` (p3 0 0 (-10))

pendulum = stick +. weight

stick = cube `scale` (p3 1 1 10) `translate` (p3 0 0 5) `color` green

weight = sphere `scale` (p3 4 4 4) `color` (1, 0, 1, 1)
