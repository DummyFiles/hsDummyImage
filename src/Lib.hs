{-# LANGUAGE TypeOperators   #-}

module Lib
    (
      savePngImageWithoutCompression,

      availableGenFunctions,
      availableGenFunctionsStr,

      availableUnits,
      availableUnitsStr,

      calcSizeInBytes,

      defaultDummyName,
      formatOutputName,

      handleArgsAndGenPng,
      genAndSavePngByTargetSize,
      genAndSavePng
    ) where

import JPHack(savePngImageWithoutCompression)

import Codec.Picture
    ( generateImage,
      Pixel8,
      Image,
      PixelRGB8(..),
      DynamicImage(ImageRGB8) )

import GHC.Stats ( getRTSStats, RTSStats(elapsed_ns, cpu_ns) )

import Data.List ( elemIndex, intercalate )
import Text.Printf (printf)
import System.FilePath ( (</>) )

import Data.Array.Repa (Array, DIM2, U, D, Z (..), (:.)(..), (!))
import qualified Data.Array.Repa     as R -- for Repa

type RGB8 = (Pixel8, Pixel8, Pixel8)
type GenFunc = Int -> Int -> (Z :. Int :. Int) -> RGB8
type GenFuncMap = [(String, GenFunc)]

availableGenFunctions :: GenFuncMap
availableGenFunctions = [("stains", stainsGenFunc),("hypnotoad", hypnotoadGenFunc),("fastest_mono", fastestMonoGenFunc),("alt_stains", altStainsGenFunc), ("matrix", matrixGenFunc), ("cold_grid", coldGridGenFunc), ("high_crosswalk", highCrossWalkGenFunc)]

availableGenFunctionsStr :: String
availableGenFunctionsStr = "{" ++ grabGenFuncNames availableGenFunctions ++ "}"

grabGenFuncNames :: GenFuncMap -> String
grabGenFuncNames [] = "ERROR" 
grabGenFuncNames ((name, _):xs) = if not (null xs) then name ++" | "++  grabGenFuncNames xs else name

findGenFuncByName' ::  GenFuncMap -> String -> Maybe GenFunc
findGenFuncByName' ((name, f'''):xs) str = if str==name then Just f''' else findGenFuncByName' xs str 
findGenFuncByName' [] _ = Nothing

findGenFuncByName :: String -> Maybe GenFunc
findGenFuncByName = findGenFuncByName' availableGenFunctions

availableUnits :: [String]
availableUnits     = ["b", "kb", "mb", "gb"]

availableUnitsStr :: String
availableUnitsStr = "{" ++ intercalate ", " availableUnits ++ "}"

calcSizeInBytes :: Int -> String -> Int
calcSizeInBytes size units = do
    case elemIndex units availableUnits of
      Just multiplier -> size*(1024^multiplier)
      Nothing  -> error $ "Incorrect units ["++units++"] select from available -> "++availableUnitsStr

countOfPH :: String -> Int
countOfPH []        = 0
countOfPH [_]       = 0
countOfPH (x:xx:xs) = (if x:xx:""=="%v" then 1 else 0) + countOfPH (if xx=='%' then xx:xs else xs)

defaultDummyName :: String
defaultDummyName = "dummy_%v_%v.png"

formatOutputName :: String -> Int -> String -> String
formatOutputName outputName oSize oUnits
  | null outputName      = "dummy.png"
  | placeholdersCount==2 = printf outputName oSize oUnits
  | placeholdersCount==0 = outputName
  | otherwise = error "The name must contain 2 or 0 placeholders"
  where
    placeholdersCount = countOfPH outputName

checkDirPath :: FilePath -> FilePath
checkDirPath dirPath = dirPath --TODO

-- ~ --------------------------------------------------------------------------------------------------

handleArgsAndGenPng :: Int -> String -> String -> String -> String -> IO ()
handleArgsAndGenPng size units targetName targetDir genFuncName
  | size < 1 = error "Size must not be less than 1"
  | otherwise = genAndSavePngByTargetSize path size units genFunc
  where
    formatedName = formatOutputName targetName size units
    checkedDir   = checkDirPath targetDir
    path = checkedDir </> formatedName
    genFunc = case findGenFuncByName genFuncName  of
      Just gFunc -> gFunc
      Nothing -> error $ "Func {"++genFuncName++"} not found. Please select from "++availableGenFunctionsStr

genAndSavePngByTargetSize :: FilePath -> Int -> String -> GenFunc -> IO ()
genAndSavePngByTargetSize path size units gFunc
  | targetSize < colorDepth = error $ printf "Equivalent size in bytes, must not be less than %d(received: %d)" colorDepth targetSize
  | otherwise = do
    out <- genAndSavePng path shapeX shapeY gFunc
    stats <- getRTSStats
    let
      elapsed_time :: Double
      elapsed_time = fromIntegral (elapsed_ns stats) * 1e-9
      cpu_time :: Double
      cpu_time = fromIntegral (cpu_ns stats) * 1e-9
    putStrLn $ printf "Image (~%d %s in %.4f sec., total: %.4f sec.)\nStored at: %s" size units elapsed_time cpu_time path  
    return out  
  where
    colorDepth = 8*3
    targetSize = calcSizeInBytes size units
    rShape = (floor . (sqrt :: Double -> Double) . fromIntegral) (targetSize `div` colorDepth * 8)
    shapeX = rShape
    shapeY = rShape

genAndSavePng :: FilePath -> Int -> Int -> GenFunc -> IO ()
genAndSavePng path shX shY targetGenFunc = do
  img <- R.computeUnboxedP $ generateImg targetGenFunc shX shY
  (savePngImageWithoutCompression path . ImageRGB8 . toImage) img


-- | Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b

generateImg :: GenFunc -> Int -> Int -> Array D DIM2 RGB8
generateImg genFunc shX shY= R.fromFunction (Z :. shX :. shY) (genFunc shX shY)

-- ~ --------------------------------------------------------------------------------------------------

stainsGenFunc :: GenFunc
stainsGenFunc shX shY (Z :. x :. y) =
  let 
    norm_x = x*0xff `div` shX
    norm_y = y*0xff `div` shY

    lFuncRes = (norm_x*norm_x - norm_y*norm_y) 
    gradientFuncRes = (norm_x + norm_y) `div` 2 
    s a = fromIntegral $ mod a 0xff
  
  in (s lFuncRes, s gradientFuncRes, 160)

altStainsGenFunc :: GenFunc
altStainsGenFunc shX shY (Z :. x :. y) =
  let 
    norm_x = x*0xff `div` shX
    norm_y = y*0xff `div` shY

    lFuncRes = (norm_x*norm_x - norm_y*norm_y) 
    gradientFuncRes = (norm_x + norm_y) `div` 2 
    s a = fromIntegral $ mod a 0xff
  in (s (lFuncRes `div` 3), s (gradientFuncRes + lFuncRes), 0xff - s ( gradientFuncRes `div` 2))

coldGridGenFunc :: GenFunc
coldGridGenFunc shX shY (Z :. x :. y) =
  let 
    norm_x = x*0xff `div` shX
    norm_y = y*0xff `div` shY

    grid :: Integer
    grid = (2^norm_x + 2^norm_y) `div` 2
    s a = fromIntegral $ mod a 0xff
  
  in ((0xff - s grid) `div` 3, s grid * 3 `div` 2, 160)

hypnotoadGenFunc :: GenFunc
hypnotoadGenFunc shX shY (Z :. x :. y) =
  let 
    norm_x = x*0xff `div` shX
    norm_y = y*0xff `div` shY

    grid = s (( norm_x*norm_x + norm_y*norm_y ) `div` 2)
    s a = fromIntegral $ mod a 0xff
  in ((0xff-grid) `div` 2, grid `div` 2, 200)

matrixGenFunc :: GenFunc
matrixGenFunc shX shY (Z :. x :. y) =
  let 
    norm_x = x*0xff `div` shX
    norm_y = y*0xff `div` shY
    matrix = (norm_x^norm_y)  
    s a = fromIntegral $ mod a 0xff
  in (0, s matrix, 0)

fastestMonoGenFunc :: GenFunc
fastestMonoGenFunc _ _ (Z :. _ :. _) = (255, 180, 180)

highCrossWalkGenFunc :: GenFunc
highCrossWalkGenFunc _ _ (Z :. x :. y) =
  let
    (q, r) = x `quotRem` max 3 y
    s = fromIntegral . min 0xff
  in (s q, s r, s (q + r + 30))
