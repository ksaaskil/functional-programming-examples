-- Example module
module GeometryModule
    ( sphereVolume
    , sphereArea
    , cubeVolume
    , cubeArea
    , cuboidArea
    , cuboidVolume
    )
where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 2 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c =
    rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

-- Not exported
rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
