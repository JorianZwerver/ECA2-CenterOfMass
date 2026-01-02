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

centerColor :: Int
centerColor = 2

comRows :: [[Pixel]] -> Int
comRows im = sumRmx `div` sumMx
    where
        mx      = sum <$> im
        sumMx   = sum mx
        sumRmx  = sum $ zipWith (*) mx r
        r       = [1..]

com :: [[Pixel]] -> (Int, Int)
com im = (comRows im - 1, comRows (transpose im) - 1)

imageWithCom :: Int -> [[Pixel]] -> [[Pixel]]
imageWithCom c im = uncurry (changePixelInImage im) (com im) c

lightHouseBWcom :: FilePath
lightHouseBWcom = "../images/lightHouseBWcom.pgm"

result_ex2 :: IO ()
result_ex2 = wf lightHouseBWcom $ imageWithCom centerColor imageBw

-----------------------------------------------------------------------------------------
-- Assignment 3 Center of mass of parts of the image, with and without borders
-----------------------------------------------------------------------------------------

blockWidth :: Int
blockWidth = 8

comParts :: [[Pixel]] -> [[Pixel]]
comParts im = unblocks2D (length im) $ imageWithCom centerColor <$> blocks2D blockWidth im

lightHouseComParts :: FilePath
lightHouseComParts = "../images/lightHouseComParts.pgm"

result_ex3_1 :: IO ()
result_ex3_1 = wf lightHouseComParts $ comParts image

comPartsWB :: [[Pixel]] -> [[Pixel]]
comPartsWB im = unblocks2D ln' $ addBorders centerColor $ blocks2D blockWidth $ comParts im
    where
        ln  = length im
        ln' = ln + div ln ( div blockWidth 2 )

lightHouseComPartsWB :: FilePath
lightHouseComPartsWB = "../images/lightHouseComPartsWB.pgm"

result_ex3_2 :: IO ()
result_ex3_2 = wf lightHouseComPartsWB $ comPartsWB image
        