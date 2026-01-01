module HCenterOfMassXX where

import Image
import Data.List

-- Student information:
--  Student 1
--    lastname: Zwerver
--    student number: 2850400

-----------------------------------------------------------------------------------------
-- Assignment 1, From grayscale to black and white
-----------------------------------------------------------------------------------------

threshold :: (Num a) => a
threshold = 128

thresholdIm :: (Ord a, Num a) => a -> [[a]] -> [[a]]
thresholdIm = map . map . applyThres
    where
        applyThres thres a | a > thres = 1
                           | otherwise = 0

lightHouseBW :: FilePath
lightHouseBW = "../images/lightHouseBW.pgm"

imageBw :: [[Pixel]]
imageBw = thresholdIm threshold image

result_ex1 :: IO ()
result_ex1 = wf lightHouseBW imageBw

-----------------------------------------------------------------------------------------
-- Assignment 2, Center of mass of rows and picture
-----------------------------------------------------------------------------------------

centerColor :: (Num a) => a
centerColor = 2

comRows :: [[Int]] -> Int
comRows im = rmx `div` mx
    where
        sums = map sum im
        mx = sum sums
        inds = [1..length sums]
        rmx = sum $ zipWith (*) sums inds

com :: [[Int]] -> (Int, Int)
com im = (comRows im - 1, comRows (transpose im) - 1)

imageWithCom :: Int -> [[Int]] -> [[Int]]
imageWithCom c im = uncurry (changePixelInImage im) (com im) c

lightHouseBWcom :: FilePath
lightHouseBWcom = "../images/lightHouseBWcom.pgm"

result_ex2 :: IO ()
result_ex2 = wf lightHouseBWcom $ imageWithCom centerColor $ thresholdIm threshold image

-----------------------------------------------------------------------------------------
-- Assignment 3 Center of mass of parts of the image, with and without borders
-----------------------------------------------------------------------------------------

comParts :: [[Pixel]] -> [[Pixel]]
comParts im = unblocks2D (length im) $ imageWithCom 2 <$> blocks2D 8 im

lightHouseComParts :: FilePath
lightHouseComParts = "../images/lightHouseComParts.pgm"

result_ex3_1 :: IO ()
result_ex3_1 = wf lightHouseComParts $ comParts image

comPartsWB :: [[Pixel]] -> [[Pixel]]
comPartsWB im = unblocks2D ln' $ addBorders 2 $ blocks2D 8 $ comParts im
    where
        ln = length im
        ln' = ln + ln `div` 4

lightHouseComPartsWB :: FilePath
lightHouseComPartsWB = "../images/lightHouseComPartsWB.pgm"

result_ex3_2 :: IO ()
result_ex3_2 = wf lightHouseComPartsWB $ comPartsWB image
        