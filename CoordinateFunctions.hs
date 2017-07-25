module CoordinateFunctions(
    convToXCord,
    convToYCord,
    convFromXCord,
    convFromYCord,
    convFromCord,
    modifyCoordinate,
    middleCoordinate
)where

import Data.Char (ord, chr)
----
type Coordinate = String

convToXCord (x:s) = (ord x) - (ord 'A')
convToYCord (x:y:s) = (ord y) - (ord '1')
convFromXCord x = chr $ x + (ord 'A')
convFromYCord y = chr $ y + (ord '0') +1
convFromCord x y = [convFromXCord x, convFromYCord y]

modifyCoordinate :: Coordinate -> Int -> Int -> Coordinate
modifyCoordinate coordinate modx mody = [convFromXCord $ (convToXCord coordinate)+modx] ++
                                        [convFromYCord $ (convToYCord coordinate)+mody]

middleCoordinate coord1 coord2 = convFromCord (quot (convToXCord coord1 + convToXCord coord2) 2) (quot (convToYCord coord1 + convToYCord coord2) 2)
