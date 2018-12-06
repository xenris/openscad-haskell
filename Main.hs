module Main where

-- import qualified OpenSCAD as S
import OpenSCAD

-- main = writeFile "model.scad" (render model 50)
main = writeFile "model.scad" (preview model 40)

-- model = translate (color (resize circle (p2 2 2)) red) (p2 2 2)

-- model = ((color (circle =. (p2 3 5)) red) <-> (p2 1 0)) +. (color circle green)

-- model = (sphere `resize` (p3 1 2 2)) -. (sphere `resize` (p3 1.1 0.9 0.9))

size = 40

model :: Shape3d
model = (pyramid `color` green) -. (hole `color` blue)
    where
        pyramid = (union $ map (\ i -> (layer i) `translate` (p3 0 0 (i * size / 10))) [0..8]) `translate` (p3 0 0 2)
        hole = (cylinder `resize` (p3 2 2 10)) `translate` (p3 0 0 (size * 0.88))
        layer i = cube `resize` (p3 (w i) (w i) (h i))
        w i = size - (i * 4.3)
        h i = size / 10

-- model = Model3d $ extrude ((square 20) +. (translate2d (circle 5) 10 10)) 10
-- model = Model2d $ (square 20) +. (translate2d (circle 5) 10 10)
-- model = (cube 10 20 20) -. (cylinder 3 30)

