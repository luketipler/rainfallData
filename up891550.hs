--
-- MATHFUN
-- UP891550
--

import Data.List
import Data.Char
import System.IO

--
-- [Char] for City name
-- Float for both coordinates
-- Negative numbers need () around each
-- [Int] for the temperature list
--

--Types definitions

type CityName = String
type NorthLat = Float
type EastLat = Float
type DayRainfall = Int


type WeekRainfall = [DayRainfall]

type Place = (CityName , NorthLat , EastLat , WeekRainfall)

testData :: [Place]
testData = [("London"     ,  51.5 , (-0.1) ,  [0, 0, 5, 8, 8, 0, 0]) ,
            ("Cardiff"    ,  51.5 , (-3.2) ,  [12, 8, 15, 0, 0, 0, 2]) ,
            ("Birmingham" ,  52.5 , (-1.9) ,  [0, 2, 10, 7, 8, 2, 2]) ,
            ("Liverpool"  ,  53.4 , (-3.0) ,  [8, 16, 20, 3, 4, 9, 2]) ,
            ("Hull"       ,  53.8 , (-0.3) ,  [0, 6, 5, 0, 0, 0, 4]) ,
            ("Newcastle"  ,  55.0 , (-1.6) ,  [0, 0, 8, 3, 6, 7, 5]) ,
            ("Belfast"    ,  54.6 , (-5.9) ,  [10, 18, 14, 0, 6, 5, 2]) ,
            ("Glasgow"    ,  55.9 , (-4.3) ,  [7, 5, 3, 0, 6, 5, 0]) ,
            ("Plymouth"   ,  50.4 , (-4.1) ,  [4, 9, 0, 0, 0, 6, 5]) ,
            ("Aberdeen"   ,  57.1 , (-2.1) ,  [0, 0, 6, 5, 8, 2, 0]) ,
            ("Stornoway"  ,  58.2 , (-6.4) ,  [15, 6, 15, 0, 0, 4, 2]) ,
            ("Lerwick"    ,  60.2 , (-1.1) ,  [8, 10, 5, 5, 0, 0, 3]) ,
            ("St Helier"  ,  49.2 , (-2.1) ,  [0, 0, 0, 0, 6, 10, 0]) ,
            ("Norwich"    ,  52.6 , ( 1.3) ,  [0, 6, 5, 0, 0, 0, 3])]
          --("Portsmouth" ,  50.8 , (-1.1) ,  [0, 0, 3, 2, 5, 2, 1])

          -- "Portsmouth" 50.8 (-1.1) [0, 0, 3, 2, 5, 2, 1]
--
--  Functional code
--

--Displays all place names
placeName :: [Place] -> [String]
placeName testData =  [name | (name , _ , _ , _) <- testData]

--specificName to select a specificName in a position
specificName :: [Place] -> Int -> CityName
specificName testData x =  [name | (name , _ , _ , _) <- testData] !! x

--Displays north and east respectively
getNorthLat :: [Place] -> [Float]
getNorthLat testData = [north | (_ , north , _ , _) <- testData]

getEastLat :: [Place] -> [Float]
getEastLat testData = [east | (_ , _ , east , _) <- testData]

--Displays all rainfaill data
rainfall :: [Place] -> [[Int]]
rainfall testData = [rain | (_ , _ , _ , rain) <- testData]

--Displays rainfall data for a given place (demo 1)
nameAndRainfall :: [Char] -> [(String , [Int])]
nameAndRainfall nameInput  = [(name , rain) | (name , _ , _ , rain) <- testData , nameInput == name ]

--Adds new place to data
addNewPlace :: String -> Float -> Float -> [DayRainfall] -> [Place] -> [Place]
addNewPlace na nl el rf db = db++[(na , nl , el , rf)]

--Rounds to 2 decimal places
twoDP :: Double -> Double
twoDP x = (fromIntegral (floor (x * 100))) / 100

--Calculates average of the data set using sum of the data
--and then dividing it by the int length
avgerage :: (Real a, Fractional b) => [a] -> b
avgerage xs = realToFrac (sum xs) / (fromIntegral $ length xs)

--Displays average rain of a given place (demo 2)
averageRainOfPlace :: String -> [(String, Float)]
averageRainOfPlace x = [(name , avgerage rain) | (name, _, _, rain) <- testData, x == name]

-- Removes symbols in a string
justNumbers :: String -> String
justNumbers = filter isNumber

--Creates a function to add spacing for the table created
spaceFunction :: Char -> Int -> [Char] -> [Char]
spaceFunction character tableSpace value = value ++ replicate (tableSpace - (length value)) character

-- Returns place names and their 7-day rainfall figures
-- in a single string neatly formatted into 8 collums (demo 3)
--The spaceFunction is applied to each of  the show statements
--which neatly spaces all the data with the given character ' '
placesToString :: [Place] -> String
placesToString testData = unlines [intercalate "\t" [spaceFunction ' ' 7 name,
                                                     spaceFunction ' ' 3 (show rain1) , spaceFunction ' ' 3 (show rain2),
                                                     spaceFunction ' ' 3 (show rain3) , spaceFunction ' ' 3 (show rain4),
                                                     spaceFunction ' ' 3 (show rain5) , spaceFunction ' ' 3 (show rain6),
                                                     spaceFunction ' ' 3 (show rain7)] | (name , _ , _ , [rain1 , rain2 , rain3 , rain4 , rain5 , rain6 , rain7]) <- testData ]

-- Calcualtes whether or not a place is dry or note
isPlaceDry :: [Place] -> Int -> [(CityName, WeekRainfall)]
isPlaceDry testData day = [(name , rain) | (name , _ , _ , rain) <- testData , (rain !! (day - 1) ) == 0]

-- Will update rainfall figures removing last week (demo 5)
--[0,8,0,0,5,0,0,3,4,2,0,8,0,0]
--Basic principle
--xs = [0, 0, 5, 8, 8, 0, 0]
--updateRain :: Int -> [Int]
--xy n = n : (init xs)
--Will update the individual data with the new rainfall for the day
updateRainfall :: Int -> Place -> Place
updateRainfall newR (name, north, east, rain) = (name, north, east , newR : init rain)
--use the function above to complete the whole
--testData list place by place
updateFunction :: [Int] -> [Place] -> [Place]
updateFunction newRain testData = zipWith updateRainfall newRain testData

-- Update place data from a given name (deme 6)
--newName = ("Portsmouth", (50.8, -1.1), [0,0,3,2,5,2,1])
--oldName = "plymouth"
--This works with case sensetive inputs and will allow the program to replace a place data
--This function will not just add the data this function is made above
addReplace :: Place -> CityName -> [Place] -> [Place]
addReplace newName oldName testData  = [if (name == oldName) then (newName) else (name, north, east, rain) | (name, north, east, rain) <- testData]
--("Portsmouth" ,  50.8 , (-1.1) ,  [0, 0, 3, 2, 5, 2, 1])

--demo 7 code to work out distance of driest place closest to a given coordinates
--pythagoras theorom for calculating the distance between two points
--this will be used to Calcualte distance between places.
pythagoras :: (NorthLat, EastLat) -> (NorthLat, EastLat) -> Float
pythagoras (northLat1, eastLat1) (northLat2 , eastLat2) = sqrt ( (northLat1 - northLat2)^2 + (eastLat1 - eastLat2)^2)

--will output the distance of the closest city to the point given
isPlaceDryLat :: [Place] -> Int -> (NorthLat, EastLat) -> Float
isPlaceDryLat testData day pointInput = minimum [ pythagoras pointInput (north, east) | (_ , north , east , rain) <- testData , (rain !! (day - 1) ) == 0]

--the actual code for demo 7 using the above functions to use the City
--closest to the
nameOfShort :: [Place] -> Int -> (NorthLat , EastLat) -> CityName
nameOfShort testData day pointInput = [ name | (name, north , east, _) <- testData , (isPlaceDryLat testData day pointInput) == (pythagoras pointInput (north , east))] !! 0

--
--  Demo
--

demo :: Int -> IO ()
demo 1 = print (placeName testData)

 -- display, to two decimal places, the average rainfall in Cardiff
demo 2 = print (averageRainOfPlace "Cardiff")

--creates a neat table of all the test data
demo 3 = putStrLn (placesToString testData)

-- display the names of all places that were dry two days ago
demo 4 = print (isPlaceDry testData 2)

-- update the data with most recent rainfall
--[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
demo 5 = print (updateFunction [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData)

-- replace "Plymouth" with "Portsmouth" which has
-- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 6 = print (addReplace ("Portsmouth" ,  50.8 , (-1.1) ,  [0, 0, 3, 2, 5, 2, 1]) "Plymouth" testData)

-- display the name of the place closest to 50.9 (N), -1.3 (E)
-- that was dry yesterday
demo 7 = putStrLn ( nameOfShort testData 1 (50.9 , -1.3) )

-- display the rainfall map
demo 8 = do
  clearScreen
  goTo (0,0)
  mapPosition testData 0

--
-- Screen Utilities (use these to do the rainfall map - note that these do
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text

--
-- Your rainfall map code goes here
--

--Will take the coorods of a city from the citys name inputted
--this allows the entered coords to be used in map
cityCoords :: [Place] -> CityName -> (NorthLat, EastLat)
cityCoords testData name  =  [(north, east) | (city , north , east , _) <- testData , city == name] !! 0

--work out limits for max and min points
getMax :: [Place] -> [Char] -> Float
getMax testData val
          | val == "y" = maximum (getNorthLat testData)
          | val == "x" = maximum (getEastLat testData)
          | otherwise    = 0

getMin :: [Place] -> [Char] -> Float
getMin testData val
          | val == "y" = minimum (getNorthLat testData)
          | val == "x" = minimum (getEastLat testData)
          | otherwise    = 0

-- returns an Int value to plot on the console
-- using the floor function to remove decimal places so that the points areplotted neatly
mapScale :: [Place] -> Float -> [Char] -> Integer -> Int
mapScale testData coord axis scale = floor ((coord - getMin testData axis) * (fromInteger scale / (getMax testData axis - (getMin testData axis))))

--will work out the position coordinates so they can be plotted
plotLat :: [Place] -> (Float, Float) -> (Int , Int)
plotLat testData (yLat, xLat) = (50 - (mapScale testData yLat "y" 50), mapScale testData xLat "x" 80)

--Will get the position of the screen
positionCity :: String -> [Place] -> IO ()
positionCity cityName testData = writeAt (plotLat testData (cityCoords testData cityName)) ("+ " ++  cityName)

--Recursively add all the points together with the above functions
mapPosition :: [Place] -> Int -> IO ()
mapPosition testData count
            | count < length testData = do positionCity ( placeName testData !! count) testData
                                           mapPosition testData (count + 1)
            | otherwise               = do goTo (50,0)


--
-- Your user interface (and loading/saving) code goes here
--
