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

result_ex1 :: IO ()
result_ex1 = wf lightHouseBW $ thresholdIm threshold image

-----------------------------------------------------------------------------------------
-- Assignment 2, Center of mass of rows and picture
-----------------------------------------------------------------------------------------

centerColor :: (Num a) => a
centerColor = 10

comRows :: [[Int]] -> Int
comRows im = rmx `div` mx
    where
        sums = map (sum) im
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
comParts = undefined

comPartsWB :: [[Pixel]] -> [[Pixel]]
comPartsWB = undefined