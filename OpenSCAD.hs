{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module OpenSCAD where

-- TODO Work out why this doesn't work easily.
--  (error: ambiguous type variable, but only one potential instance)
type Point2d = (Double, Double)

p2 :: Double -> Double -> Point2d
p2 x y = (x, y) :: Point2d

type Point3d = (Double, Double, Double)

p3 :: Double -> Double -> Double -> Point3d
p3 x y z = (x, y, z) :: Point3d

type Color = (Float, Float, Float, Float)

black :: Color
black = (0, 0, 0, 1)

red :: Color
red = (1, 0, 0, 1)

green :: Color
green = (0, 1, 0, 1)

blue :: Color
blue = (0, 0, 1, 1)

yellow :: Color
yellow = (1, 1, 0, 1)

cyan :: Color
cyan = (0, 1, 1, 1)

magenta :: Color
magenta = (1, 0, 1, 1)

white :: Color
white = (1, 1, 1, 1)

class Colorable a where
    color :: a -> Color -> a

class Combinable a where
    (+.) :: a -> a -> a
    (-.) :: a -> a -> a
    (*.) :: a -> a -> a
    (#.) :: a -> a -> a
    (<>.) :: a -> a -> a
    union :: [a] -> a
    difference :: [a] -> a
    intersection :: [a] -> a
    minkowski :: [a] -> a
    hull :: [a] -> a

class Movable a b where
    translate :: a -> b -> a
    rotate :: a -> b -> a
    scale :: a -> b -> a
    resize :: a -> b -> a
    mirror :: a -> b -> a

class GenerateScad a where
    generateScad :: a -> String

data Shape2d
    = Circle
    | Square
    | Polygon [Point2d]
    | Text String
    | Union2d [Shape2d]
    | Difference2d [Shape2d]
    | Intersection2d [Shape2d]
    | Minkowski2d [Shape2d]
    | Hull2d [Shape2d]
    | Offset Shape2d Float OffsetStyle
    | Translate2d Shape2d Point2d
    | Rotate2d Shape2d Point2d
    | Scale2d Shape2d Point2d -- TODO error on negative scaling, as apparently doesn't work.
    | Resize2d Shape2d Point2d
    | Mirror2d Shape2d Point2d
    | Color2d Shape2d Color
    | Slice Shape3d SliceStyle
    deriving (Show)

data OffsetStyle
    = Extend
    | Round
    | Chamfer
    deriving (Show)

data SliceStyle
    = Project
    | Cut
    deriving (Show)

data Shape3d
    = Sphere
    | Cube
    | Cylinder
    | Polyhedron [Point3d] [[Int]]
    | Extrude Shape2d ExtrudeStyle
    | Union3d [Shape3d]
    | Difference3d [Shape3d]
    | Intersection3d [Shape3d]
    | Minkowski3d [Shape3d]
    | Hull3d [Shape3d]
    | Translate3d Shape3d Point3d
    | Rotate3d Shape3d Point3d
    | Scale3d Shape3d Point3d
    | Resize3d Shape3d Point3d
    | Mirror3d Shape3d Point3d
    | Color3d Shape3d Color
    deriving (Show)

data ExtrudeStyle
    = Straight Float
    | Twist Float Float
    | Rotate
    deriving (Show)

render :: (GenerateScad a) => a -> Int -> String
render s l = "$fn=" ++ (show l) ++ ";\nrender() {\n" ++ (generateScad s) ++ "}\n"

preview :: (GenerateScad a) => a -> Int -> String
preview s l = "$fn=" ++ (show l) ++ ";\n" ++ (generateScad s) ++ "\n"

instance GenerateScad Shape2d where
    generateScad = generateScad2d

instance Combinable Shape2d where
    (+.) = union2d
    (-.) = difference2d
    (*.) = intersection2d
    (#.) = minkowski2d
    (<>.) = hull2d
    union = Union2d
    difference = Difference2d
    intersection = Intersection2d
    minkowski = Minkowski2d
    hull = Hull2d

instance Colorable Shape2d where
    color = Color2d

instance Movable Shape2d Point2d where
    translate = Translate2d
    rotate = Rotate2d
    scale = Scale2d
    resize = Resize2d
    mirror = Mirror2d

union2d :: Shape2d -> Shape2d -> Shape2d
union2d (Union2d as) (Union2d bs) = Union2d (as ++ bs)
union2d (Union2d as) b = Union2d (as ++ [b])
union2d a (Union2d bs) = Union2d ([a] ++ bs)
union2d a b = Union2d [a, b]

difference2d :: Shape2d -> Shape2d -> Shape2d
difference2d (Difference2d as) (Difference2d bs) = Difference2d (as ++ bs)
difference2d (Difference2d as) b = Difference2d (as ++ [b])
difference2d a (Difference2d bs) = Difference2d ([a] ++ bs)
difference2d a b = Difference2d [a, b]

intersection2d :: Shape2d -> Shape2d -> Shape2d
intersection2d (Intersection2d as) (Intersection2d bs) = Intersection2d (as ++ bs)
intersection2d (Intersection2d as) b = Intersection2d (as ++ [b])
intersection2d a (Intersection2d bs) = Intersection2d ([a] ++ bs)
intersection2d a b = Intersection2d [a, b]

minkowski2d :: Shape2d -> Shape2d -> Shape2d
minkowski2d (Minkowski2d as) (Minkowski2d bs) = Minkowski2d (as ++ bs)
minkowski2d (Minkowski2d as) b = Minkowski2d (as ++ [b])
minkowski2d a (Minkowski2d bs) = Minkowski2d ([a] ++ bs)
minkowski2d a b = Minkowski2d [a, b]

hull2d :: Shape2d -> Shape2d -> Shape2d
hull2d (Hull2d as) (Hull2d bs) = Hull2d (as ++ bs)
hull2d (Hull2d as) b = Hull2d (as ++ [b])
hull2d a (Hull2d bs) = Hull2d ([a] ++ bs)
hull2d a b = Hull2d [a, b]

generateScad2d :: Shape2d -> String
generateScad2d (Circle) = "circle();\n"
generateScad2d (Square) = "square(center = true);\n"
generateScad2d (Polygon ps) = "polygon(" ++ (showPoint2ds ps) ++ ");\n"
generateScad2d (Text s) = "text(" ++ (show s) ++ ");\n"
generateScad2d (Union2d ss) = "union() {\n" ++ (concatMap generateScad2d ss) ++ "}\n"
generateScad2d (Difference2d ss) = "difference() {\n" ++ (concatMap generateScad2d ss) ++ "}\n"
generateScad2d (Intersection2d ss) = "intersection() {\n" ++ (concatMap generateScad2d ss) ++ "}\n"
generateScad2d (Minkowski2d ss) = "minkowski() {\n" ++ (concatMap generateScad2d ss) ++ "}\n"
generateScad2d (Hull2d ss) = "hull() {\n" ++ (concatMap generateScad2d ss) ++ "}\n"
generateScad2d (Offset s n Extend) = "offset(delta = " ++ (show n) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Offset s n Round) = "offset(r = " ++ (show n) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Offset s n Chamfer) = "offset(delta = " ++ (show n) ++ ", chamfer = true) {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Translate2d s v) = "translate(" ++ (showPoint2d v) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Rotate2d s v) = "rotate(" ++ (showPoint2d v) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Scale2d s v) = "scale(" ++ (showPoint2d $ abs2 v) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Resize2d s v) = "resize(" ++ (showPoint2d v) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Mirror2d s v) = "mirror(" ++ (showPoint2d v) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Color2d s c) = "color(" ++ (showColor c) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad2d (Slice s Project) = "projection() {\n" ++ (generateScad3d s) ++ "}\n"
generateScad2d (Slice s Cut) = "projection(cut = true) {\n" ++ (generateScad3d s) ++ "}\n"

instance GenerateScad Shape3d where
    generateScad = generateScad3d

instance Combinable Shape3d where
    (+.) = union3d
    (-.) = difference3d
    (*.) = intersection3d
    (#.) = minkowski3d
    (<>.) = hull3d
    union = Union3d
    difference = Difference3d
    intersection = Intersection3d
    minkowski = Minkowski3d
    hull = Hull3d

instance Colorable Shape3d where
    color = Color3d

instance Movable Shape3d Point3d where
    translate = Translate3d
    rotate = Rotate3d
    scale = Scale3d
    resize = Resize3d
    mirror = Mirror3d

union3d :: Shape3d -> Shape3d -> Shape3d
union3d (Union3d as) (Union3d bs) = Union3d (as ++ bs)
union3d (Union3d as) b = Union3d (as ++ [b])
union3d a (Union3d bs) = Union3d ([a] ++ bs)
union3d a b = Union3d [a, b]

difference3d :: Shape3d -> Shape3d -> Shape3d
difference3d (Difference3d as) (Difference3d bs) = Difference3d (as ++ bs)
difference3d (Difference3d as) b = Difference3d (as ++ [b])
difference3d a (Difference3d bs) = Difference3d ([a] ++ bs)
difference3d a b = Difference3d [a, b]

intersection3d :: Shape3d -> Shape3d -> Shape3d
intersection3d (Intersection3d as) (Intersection3d bs) = Intersection3d (as ++ bs)
intersection3d (Intersection3d as) b = Intersection3d (as ++ [b])
intersection3d a (Intersection3d bs) = Intersection3d ([a] ++ bs)
intersection3d a b = Intersection3d [a, b]

minkowski3d :: Shape3d -> Shape3d -> Shape3d
minkowski3d (Minkowski3d as) (Minkowski3d bs) = Minkowski3d (as ++ bs)
minkowski3d (Minkowski3d as) b = Minkowski3d (as ++ [b])
minkowski3d a (Minkowski3d bs) = Minkowski3d ([a] ++ bs)
minkowski3d a b = Minkowski3d [a, b]

hull3d :: Shape3d -> Shape3d -> Shape3d
hull3d (Hull3d as) (Hull3d bs) = Hull3d (as ++ bs)
hull3d (Hull3d as) b = Hull3d (as ++ [b])
hull3d a (Hull3d bs) = Hull3d ([a] ++ bs)
hull3d a b = Hull3d [a, b]

generateScad3d :: Shape3d -> String
generateScad3d (Sphere) = "sphere(d = 1);\n"
generateScad3d (Cube) = "cube(center = true);\n"
generateScad3d (Cylinder) = "cylinder(d = 1, h = 1, center = true);\n"
generateScad3d (Polyhedron ps fs) = "polyhedron(" ++ (showPoint3ds ps) ++ ", " ++ (show fs) ++ ");\n"
generateScad3d (Extrude s (Straight l)) = "linear_extrude(height = " ++ (show l) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad3d (Extrude s (Twist l t)) = "linear_extrude(height = " ++ (show l) ++ ", twist = " ++ (show t) ++ ") {\n" ++ (generateScad2d s) ++ "}\n"
generateScad3d (Extrude s (Rotate)) = "rotate_extrude() {\n" ++ (generateScad2d s) ++ "}\n" -- TODO subtract everything below the x axis
generateScad3d (Union3d ss) = "union() {\n" ++ (concatMap generateScad3d ss) ++ "}\n"
generateScad3d (Difference3d ss) = "difference() {\n" ++ (concatMap generateScad3d ss) ++ "}\n"
generateScad3d (Intersection3d ss) = "intersection() {\n" ++ (concatMap generateScad3d ss) ++ "}\n"
generateScad3d (Minkowski3d ss) = "minkowski() {\n" ++ (concatMap generateScad3d ss) ++ "}\n"
generateScad3d (Hull3d ss) = "hull() {\n" ++ (concatMap generateScad3d ss) ++ "}\n"
generateScad3d (Translate3d s v) = "translate(" ++ (showPoint3d v) ++ ") {\n" ++ (generateScad3d s) ++ "}\n"
generateScad3d (Rotate3d s v) = "rotate(" ++ (showPoint3d v) ++ ") {\n" ++ (generateScad3d s) ++ "}\n"
generateScad3d (Scale3d s v) = "scale(" ++ (showPoint3d $ abs3 v) ++ ") {\n" ++ (generateScad3d s) ++ "}\n"
generateScad3d (Resize3d s v) = "resize(" ++ (showPoint3d v) ++ ") {\n" ++ (generateScad3d s) ++ "}\n"
generateScad3d (Mirror3d s v) = "mirror(" ++ (showPoint3d v) ++ ") {\n" ++ (generateScad3d s) ++ "}\n"
generateScad3d (Color3d s c) = "color(" ++ (showColor c) ++ ") {\n" ++ (generateScad3d s) ++ "}\n"

showPoint2d :: Point2d -> String
showPoint2d (x, y) = "[" ++ (show x) ++ ", " ++ (show y) ++ "]"

showPoint2ds :: [Point2d] -> String
showPoint2ds = replaceBraces . show

showPoint3d :: Point3d -> String
showPoint3d (x, y, z) = "[" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ "]"

showPoint3ds :: [Point3d] -> String
showPoint3ds = replaceBraces . show

replaceBraces :: String -> String
replaceBraces [] = []
replaceBraces ('(':xs) = '[' : (replaceBraces xs)
replaceBraces (')':xs) = ']' : (replaceBraces xs)
replaceBraces (x:xs) = x : (replaceBraces xs)

showColor :: Color -> String
showColor (r, g, b, a) = "[" ++ (show r) ++ ", " ++ (show g) ++ ", " ++ (show b) ++ ", " ++ (show a) ++ "]"

abs2 :: Point2d -> Point2d
abs2 (x, y) = (abs x, abs y)

abs3 :: Point3d -> Point3d
abs3 (x, y, z) = (abs x, abs y, abs z)

square :: Shape2d
square = Square

circle :: Shape2d
circle = Circle

cube :: Shape3d
cube = Cube

sphere :: Shape3d
sphere = Sphere

cylinder :: Shape3d
cylinder = Cylinder

cone :: Shape3d
cone = Extrude (Polygon [(0, -0.5), (0, 0.5), (0.5, -0.5)]) Rotate
