{-# LANGUAGE NoMonomorphismRestriction #-}
module CCenterOfMass11 where
import Clash.Prelude
import Axi
import Common
import ComAxisTb
import Data.Maybe
import qualified Data.List as L
import ImageData
-- import Debug.Trace

-- Student information:
--  Student 1
--    lastname: Zwerver
--    student number: 2850400

-----------------------------------------------------------------------------------------
-- Assignment 4, Changing a pixel in a picture
-----------------------------------------------------------------------------------------

changePixelInImage :: (KnownNat n, Enum a)
  => Vec n (Vec n p)  -- list of list of values
  -> a                -- row
  -> a                -- collumn
  -> p                -- new value
  -> Vec n (Vec n p)  -- list of list of values with updated value
changePixelInImage image y x p = replace y newRow image
    where
        newRow = replace x p $ image !! y

thresholdIm :: (KnownNat n, Ord a, Num a) 
  => a 
  -> Vec n (Vec n a) 
  -> Vec n (Vec n a)
thresholdIm = map . map . applyThres
    where
        applyThres thres a | a > thres = 1
                           | otherwise = 0

comRows :: forall n a . (KnownNat n, Integral a)
  => Vec n (Vec n a)
  -> a
comRows im = sumRmx `div` sumMx
    where
        mx      = sum <$> im
        sumMx   = sum mx
        sumRmx  = sum $ zipWith (*) mx r
        r       = iterateI (+1) 1 :: Vec n a

com :: (KnownNat n, Integral a)
  => Vec n (Vec n a)
  -> (a, a)
com im = (comRows im - 1, comRows (transpose im) - 1)

imageWithCom :: (KnownNat n, Integral a)
  => a
  -> Vec n (Vec n a)
  -> Vec n (Vec n a)
imageWithCom c im = uncurry (changePixelInImage im) (com im) c

-----------------------------------------------------------------------------------------
-- Assignment 5, Center of mass of parts of the image, with and without borders
-----------------------------------------------------------------------------------------

centerColor :: (Num a) => a
centerColor = 2

blockWidth :: SNat 8
blockWidth = d8

comParts im = unblocks2D d128 $ imageWithCom centerColor <$> blocks2D blockWidth im

lightHouseComParts :: FilePath
lightHouseComParts = "../images/lightHouseComParts_clash.pgm"

result_ex5_1 :: IO ()
result_ex5_1 = wf lightHouseComParts $ comParts image

comPartsWB im = unblocks2D d160 $ addBorders 2 $ blocks2D d8 $ comParts im

lightHouseComPartsWB :: FilePath
lightHouseComPartsWB = "../images/lightHouseComPartsWB_clash.pgm"

result_ex5_2 :: IO ()
result_ex5_2 = wf lightHouseComPartsWB $ comPartsWB image

-----------------------------------------------------------------------------------------
-- Assignment 6, Axi streaming serial
-----------------------------------------------------------------------------------------

data CoMState = DATA 
              | COMP 
              deriving (Show, Eq, Generic, NFDataX)

axisComSer :: (KnownNat n)
  => (Vec n (Signed 32), CoMState)
  -> (Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)
  -> ((Vec n (Signed 32), CoMState), (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool))
axisComSer (buffer, state) (s_axi, m_axis_tready) = ((buffer', state'), (m_axi, s_axis_tready))
  where
    -- Store current state and updated buffer
    state'
      | s_axi_tlast   = COMP
      | s_axis_tready_= COMP
      | otherwise     = DATA
    buffer' 
      | handshake     = s_axi_tdata +>> buffer
      | otherwise     = buffer
    -- Check if data is valid and that the module is ready to receive
    handshake = case (s_axi, s_axis_tready) of
      (Just _, True)  -> True
      _               -> False
    s_axis_tready
      = not s_axis_tready_
    s_axis_tready_
      = state == COMP && not m_axis_tready
    -- Get data from input, set to 0 if invalid data
    (s_axi_tdata, s_axi_tlast, s_axi_tkeep) = case s_axi of
      Just val        -> (tData val, tLast val, tKeep val)
      _               -> (0, False, 0)
    -- The output is valid if the computing state is reached
    m_axi_tvalid
      | state == COMP = True
      | otherwise     = False
    -- Generate correct output data
    m_axi
      | m_axi_tvalid  = Just Axi4Stream 
        {tData = (0,0), tLast = True, tKeep = 0b1}
      | otherwise     = Nothing 

mAxisComSer :: (HiddenClockResetEnable dom)
  => Signal dom (Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)
  -> Signal dom (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
mAxisComSer = mealy axisComSer (replicate d64 0, DATA)

-----------------------------------------------------------------------------------------
-- You can use the simulation function spsAxisComSerTb to print out all the iner stages of the states
spsAxisComSer :: [(Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)] -> String
spsAxisComSer inp = simPrintState axisComSer initState inp -- Same as mealy
  where
    initState = (replicate d64 0, DATA)

spsAxisComSerTb :: IO ()
spsAxisComSerTb = putStrLn $ spsAxisComSer mAxisComSerInp
-----------------------------------------------------------------------------------------

simMAxisComSerTb :: [(Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)]
simMAxisComSerTb = simulateN @System (L.length mAxisComSerInp) mAxisComSer mAxisComSerInp
simMAxisComSerTbPrint = mapM_ print $ L.zip [1..] simMAxisComSerTb
simMAxisComSerTbReport = L.filter (\(_,(m,_))-> isJust m) (L.zip [1..] simMAxisComSerTb)

-----------------------------------------------------------------------------------------
-- Assignment 7, Synthesize serial Axi
-----------------------------------------------------------------------------------------

{-# ANN synthAxisComSer
  (Synthesize
    { t_name   = "synthAxisComSer"
    , t_inputs =
      [ PortName "aclk"
      , PortName "nrst"
      , PortProduct ""
        [ PortProduct "s_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "m_axis_tready" ]]
    , t_output = PortProduct ""
        [ PortProduct "m_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "s_axis_tready" ]
    }) #-}
synthAxisComSer ::
     Clock System -- aclk
  -> Reset System -- nrst
  -> Signal System (Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)
  -> Signal System (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
synthAxisComSer clk rst inp = exposeClockResetEnable mAxisComSer clk rst enableGen inp

-----------------------------------------------------------------------------------------
-- Assignment 8, Axi streaming parallel
-----------------------------------------------------------------------------------------

axisComPar :: (KnownNat n)
  => (Vec n (Vec 16 (Unsigned 8)), CoMState)
  -> (Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)
  -> ((Vec n (Vec 16 (Unsigned 8)), CoMState), (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool))
axisComPar state (s_axi, m_axis_tready) = (state', (m_axi, s_axis_tready))
  where
    state' = undefined
    m_axi = undefined
    s_axis_tready = undefined

mAxisComPar :: (HiddenClockResetEnable dom)
  => Signal dom (Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)
  -> Signal dom (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
mAxisComPar = mealy axisComPar (replicate d4 (replicate d16 0), DATA)

-----------------------------------------------------------------------------------------
-- You can use the simulation function spsAxisComParTb to print out all the iner stages of the states
spsAxisComPar :: [(Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)] -> String
spsAxisComPar inp = simPrintState axisComPar initState inp -- Same as mealy
  where
    initState = (replicate d4 (replicate d16 0), DATA)

spsAxisComParTb :: IO ()
spsAxisComParTb = putStrLn $ spsAxisComPar mAxisComParInp
-----------------------------------------------------------------------------------------

simMAxisComParTb :: [(Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)]
simMAxisComParTb = simulateN @System (L.length mAxisComParInp) mAxisComPar mAxisComParInp
simMAxisComParTbPrint = mapM_ print $ L.zip [1..] simMAxisComParTb
simMAxisComParTbReport = L.filter (\(_,(m,_))-> isJust m) (L.zip [1..] simMAxisComParTb)

-----------------------------------------------------------------------------------------
-- Assignment 9, Synthesize parallel Axi
-----------------------------------------------------------------------------------------

{-# ANN synthAxisComPar
  (Synthesize
    { t_name   = "synthAxisComPar"
    , t_inputs =
      [ PortName "aclk"
      , PortName "nrst"
      , PortProduct ""
        [ PortProduct "s_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "m_axis_tready" ]]
    , t_output = PortProduct ""
        [ PortProduct "m_axis"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tlast"
            , PortName "tkeep" ]]
        , PortName "s_axis_tready" ]
    }) #-}
synthAxisComPar ::
     Clock System -- aclk
  -> Reset System -- nrst
  -> Signal System (Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)
  -> Signal System (Maybe (Axi4Stream (Unsigned 4, Unsigned 4) (BitVector 1)), Bool)
synthAxisComPar clk rst inp = exposeClockResetEnable mAxisComPar clk rst enableGen inp


