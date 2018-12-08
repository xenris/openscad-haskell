{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module OpenSCAD where

import Data.List

data Number
    = Number Double
    | Add Number Number
    | Sub Number Number
    | Mul Number Number
    | Neg Number
    | Div Number Number
    | Recip Number
    | Abs Number
    | Sign Number
    | TimeStep
    | Exp Number
    | Log Number
    | Sqrt Number
    | Pow Number Number
    | LogBase Number Number
    | Sin Number
    | Cos Number
    | Tan Number
    | Asin Number
    | Acos Number
    | Atan Number
    | Sinh Number
    | Cosh Number
    | Tanh Number
    | Asinh Number
    | Acosh Number
    | Atanh Number
    deriving (Show)

instance Num Number where
    (+) (Number a) (Number b) = Number (a + b)
    (+) a b = Add a b
    (-) (Number a) (Number b) = Number (a - b)
    (-) a b = Sub a b
    (*) (Number a) (Number b) = Number (a * b)
    (*) a b = Mul a b
    negate (Number a) = Number (-a)
    negate a = Neg a
    abs (Number a) = Number (abs a)
    abs a = Abs a
    signum (Number a) = Number (signum a)
    signum a = Sign a
    fromInteger n = Number $ fromInteger n

instance Fractional Number where
    (/) (Number a) (Number b) = Number (a / b)
    (/) a b = Div a b
    recip (Number a) = Number (recip a)
    recip a = Recip a
    fromRational n = Number $ fromRational n

instance GenerateScad Number where
    generateScad i (Number n) = show n
    generateScad i (Add a b) = (indent i) ++ "(" ++ (generateScad 0 a) ++ " + " ++ (generateScad 0 b) ++ ")"
    generateScad i (Sub a b) = (indent i) ++ "(" ++ (generateScad 0 a) ++ " - " ++ (generateScad 0 b) ++ ")"
    generateScad i (Mul a b) = (indent i) ++ "(" ++ (generateScad 0 a) ++ " * " ++ (generateScad 0 b) ++ ")"
    generateScad i (Neg a) = (indent i) ++ "-(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Div a b) = (indent i) ++ "(" ++ (generateScad 0 a) ++ " / " ++ (generateScad 0 b) ++ ")"
    generateScad i (Recip a) = (indent i) ++ "(1.0 / " ++ (generateScad 0 a) ++ ")"
    generateScad i (Abs a) = (indent i) ++ "abs(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Sign a) = (indent i) ++ "sign(" ++ (generateScad 0 a) ++ ")"
    generateScad i (TimeStep) = (indent i) ++ "$t"
    generateScad i (Exp a) = (indent i) ++ "exp(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Log a) = (indent i) ++ "ln(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Sqrt a) = (indent i) ++ "sqrt(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Pow a b) = (indent i) ++ "pow(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Sin a) = (indent i) ++ "sin(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Cos a) = (indent i) ++ "cos(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Tan a) = (indent i) ++ "tan(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Asin a) = (indent i) ++ "asin(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Acos a) = (indent i) ++ "acos(" ++ (generateScad 0 a) ++ ")"
    generateScad i (Atan a) = (indent i) ++ "atan(" ++ (generateScad 0 a) ++ ")"
    -- generateScad (LogBase a b) = "logBase(" ++ (generateScad a) ++ ")"
    -- generateScad (Sinh a) = "sinh(" ++ (generateScad a) ++ ")"
    -- generateScad (Cosh a) = "cosh(" ++ (generateScad a) ++ ")"
    -- generateScad (Tanh a) = "tanh(" ++ (generateScad a) ++ ")"
    -- generateScad (Asinh a) = "asinh(" ++ (generateScad a) ++ ")"
    -- generateScad (Acosh a) = "acosh(" ++ (generateScad a) ++ ")"
    -- generateScad (Atanh a) = "atanh(" ++ (generateScad a) ++ ")"

instance Floating Number where
    pi = Number pi
    exp = Exp
    log = Log
    sqrt = Sqrt
    (**) = Pow
    logBase = LogBase
    sin = Sin . rtod
    cos = Cos . rtod
    tan = Tan . rtod
    asin = dtor . Asin
    acos = dtor . Acos
    atan = dtor . Atan
    sinh = Sinh
    cosh = Cosh
    tanh = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh

-- log
-- atan2
-- floor
-- round
-- ceil
-- len
-- let
-- rands
-- min
-- max

instance Enum Number where
    succ a = a + 1
    pred a = a - 1
    toEnum a = Number $ fromIntegral a
    fromEnum (Number n) = floor n

v1 :: Number -> Number
v1 n = n

-- TODO Work out why this doesn't work easily.
--  (error: ambiguous type variable, but only one potential instance)
type Point2d = (Number, Number)

v2 :: Number -> Number -> Point2d
v2 x y = (x, y) :: Point2d

type Point3d = (Number, Number, Number)

v3 :: Number -> Number -> Number -> Point3d
v3 x y z = (x, y, z) :: Point3d

type Color = (Number, Number, Number, Number)

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
    scale :: a -> b -> a
    resize :: a -> b -> a
    mirror :: a -> b -> a

class Rotatable a b where
    rotate :: a -> b -> a

class Modifiable a where
    background :: a -> a
    debug :: a -> a
    root :: a -> a
    disable :: a -> a

class GenerateScad a where
    generateScad :: Int -> a -> String

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
    | Offset Shape2d Number OffsetStyle
    | Translate2d Shape2d Point2d
    | Rotate2d Shape2d Number
    | Scale2d Shape2d Point2d
    | Resize2d Shape2d Point2d
    | Mirror2d Shape2d Point2d
    | Color2d Shape2d Color
    | Slice Shape3d SliceStyle
    | Background2d Shape2d
    | Debug2d Shape2d
    | Root2d Shape2d
    | Disable2d Shape2d
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
    | Rotate3d Shape3d Number Point3d
    | Scale3d Shape3d Point3d
    | Resize3d Shape3d Point3d
    | Mirror3d Shape3d Point3d
    | Color3d Shape3d Color
    | Background3d Shape3d
    | Debug3d Shape3d
    | Root3d Shape3d
    | Disable3d Shape3d
    deriving (Show)

data ExtrudeStyle
    = Straight Number
    | Twist Number Number
    | Rotate
    deriving (Show)

render :: (GenerateScad a) => a -> Int -> String
render s l = "$fn = " ++ (show l) ++ ";\nrender() {\n" ++ (generateScad 1 s) ++ "}\n"

preview :: (GenerateScad a) => a -> Int -> String
preview s l = "$fn = " ++ (show l) ++ ";\n" ++ (generateScad 0 s) ++ "\n"

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
    scale = Scale2d
    resize = Resize2d
    mirror = Mirror2d

instance Rotatable Shape2d Number where
    rotate s a = Rotate2d s (rtod a)

instance Modifiable Shape2d where
    background = Background2d
    debug = Debug2d
    root = Root2d
    disable = Disable2d

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

generateScad2d :: Int -> Shape2d -> String
generateScad2d i (Circle) = (indent i) ++ "circle();\n"
generateScad2d i (Square) = (indent i) ++ "square(center = true);\n"
generateScad2d i (Polygon ps) = (indent i) ++ "polygon(" ++ (generateScad 0 ps) ++ ");\n"
generateScad2d i (Text s) = (indent i) ++ "text(" ++ (show s) ++ ");\n"
generateScad2d i (Union2d ss) = (indent i) ++ "union() {\n" ++ (concatMap (generateScad2d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad2d i (Difference2d ss) = (indent i) ++ "difference() {\n" ++ (concatMap (generateScad2d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad2d i (Intersection2d ss) = (indent i) ++ "intersection() {\n" ++ (concatMap (generateScad2d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad2d i (Minkowski2d ss) = (indent i) ++ "minkowski() {\n" ++ (concatMap (generateScad2d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad2d i (Hull2d ss) = (indent i) ++ "hull() {\n" ++ (concatMap (generateScad2d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad2d i (Offset s n Extend) = (indent i) ++ "offset(delta = " ++ (generateScad 0 n) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Offset s n Round) = (indent i) ++ "offset(r = " ++ (generateScad 0 n) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Offset s n Chamfer) = (indent i) ++ "offset(delta = " ++ (generateScad 0 n) ++ ", chamfer = true) {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Translate2d s v) = (indent i) ++ "translate(" ++ (generateScad 0 v) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Rotate2d s n) = (indent i) ++ "rotate(" ++ (generateScad 0 n) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Scale2d s v) = (indent i) ++ "scale(" ++ (generateScad 0 $ abs2 v) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Resize2d s v) = (indent i) ++ "resize(" ++ (generateScad 0 v) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Mirror2d s v) = (indent i) ++ "mirror(" ++ (generateScad 0 v) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Color2d s c) = (indent i) ++ "color(" ++ (generateScad 0 c) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Slice s Project) = (indent i) ++ "projection() {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Slice s Cut) = (indent i) ++ "projection(cut = true) {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Background2d s) = (indent i) ++ "%scale() {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Debug2d s) = (indent i) ++ "#scale() {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Root2d s) = (indent i) ++ "!scale() {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad2d i (Disable2d s) = (indent i) ++ "*scale() {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"

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
    scale = Scale3d
    resize = Resize3d
    mirror = Mirror3d

instance Rotatable Shape3d (Number, Point3d) where
    rotate s (a, v) = Rotate3d s (rtod a) v

instance Modifiable Shape3d where
    background = Background3d
    debug = Debug3d
    root = Root3d
    disable = Disable3d

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

generateScad3d :: Int -> Shape3d -> String
generateScad3d i (Sphere) = (indent i) ++ "sphere(d = 1);\n"
generateScad3d i (Cube) = (indent i) ++ "cube(center = true);\n"
generateScad3d i (Cylinder) = (indent i) ++ "cylinder(d = 1, h = 1, center = true);\n"
generateScad3d i (Polyhedron ps fs) = (indent i) ++ "polyhedron(" ++ (generateScad 0 ps) ++ ", " ++ (show fs) ++ ");\n"
generateScad3d i (Extrude s (Straight l)) = (indent i) ++ "linear_extrude(height = " ++ (generateScad 0 l) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Extrude s (Twist l t)) = (indent i) ++ "linear_extrude(height = " ++ (generateScad 0 l) ++ ", twist = " ++ (generateScad 0 t) ++ ") {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Extrude s (Rotate)) = (indent i) ++ "rotate_extrude() {\n" ++ (generateScad2d (i + 1) s) ++ (indent i) ++ "}\n" -- TODO subtract everything below the x axis
generateScad3d i (Union3d ss) = (indent i) ++ "union() {\n" ++ (concatMap (generateScad3d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad3d i (Difference3d ss) = (indent i) ++ "difference() {\n" ++ (concatMap (generateScad3d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad3d i (Intersection3d ss) = (indent i) ++ "intersection() {\n" ++ (concatMap (generateScad3d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad3d i (Minkowski3d ss) = (indent i) ++ "minkowski() {\n" ++ (concatMap (generateScad3d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad3d i (Hull3d ss) = (indent i) ++ "hull() {\n" ++ (concatMap (generateScad3d (i + 1)) ss) ++ (indent i) ++ "}\n"
generateScad3d i (Translate3d s v) = (indent i) ++ "translate(" ++ (generateScad 0 v) ++ ") {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Rotate3d s n v) = (indent i) ++ "rotate(" ++ (generateScad 0 n) ++ ", "++ (generateScad 0 v) ++ ") {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Scale3d s v) = (indent i) ++ "scale(" ++ (generateScad 0 $ abs3 v) ++ ") {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Resize3d s v) = (indent i) ++ "resize(" ++ (generateScad 0 v) ++ ") {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Mirror3d s v) = (indent i) ++ "mirror(" ++ (generateScad 0 v) ++ ") {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Color3d s c) = (indent i) ++ "color(" ++ (generateScad 0 c) ++ ") {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Background3d s) = (indent i) ++ "%scale() {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Debug3d s) = (indent i) ++ "#scale() {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Root3d s) = (indent i) ++ "!scale() {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"
generateScad3d i (Disable3d s) = (indent i) ++ "*scale() {\n" ++ (generateScad3d (i + 1) s) ++ (indent i) ++ "}\n"

mapt2 f (a, b) = (f a, f b)

mapt3 f (a, b, c) = (f a, f b, f c)

t2List (a, b) = [a, b]

t3List (a, b, c) = [a, b, c]

t4List (a, b, c, d) = [a, b, c, d]

instance GenerateScad Point2d where
    generateScad _ p = "[" ++ (intercalate ", " $ map (generateScad 0) $ t2List p) ++ "]"

instance GenerateScad [Point2d] where
    generateScad _ ps = "[" ++ (intercalate ", " $ map (generateScad 0) ps) ++ "]"

instance GenerateScad Point3d where
    generateScad _ p = "[" ++ (intercalate ", " $ map (generateScad 0) $ t3List p) ++ "]"

instance GenerateScad [Point3d] where
    generateScad _ ps = "[" ++ (intercalate ", " $ map (generateScad 0) ps) ++ "]"

instance GenerateScad Color where
    generateScad _ p = "[" ++ (intercalate ", " $ map (generateScad 0) $ t4List p) ++ "]"

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

indent :: Int -> String
indent i = replicate i ' '

rtod :: Number -> Number
rtod r = r * (360 / (2 * pi))

rtod2 :: Point2d -> Point2d
rtod2 (a, b) = (rtod a, rtod b)

rtod3 :: Point3d -> Point3d
rtod3 (a, b, c) = (rtod a, rtod b, rtod c)

dtor :: Number -> Number
dtor r = r * ((2 * pi) / 360)

slice :: Shape3d -> SliceStyle -> Shape2d
slice = Slice

extrude :: Shape2d -> ExtrudeStyle -> Shape3d
extrude s (Twist l a) = Extrude s (Twist l (rtod a))
extrude s e = Extrude s e
