import System.Environment
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Word
import Data.Array.Repa.IO.BMP
import Data.List
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL

maxIterations = 1000

mandelbrotIteration :: Double -> Double -> (Double, Double) -> (Double, Double)
mandelbrotIteration y0 x0 (y, x) = (2 * x * y + y0, xtemp)
 where xtemp = x * x - y * y + x0

mandelbrotEscapeIteration :: Double -> Double -> Maybe Int
mandelbrotEscapeIteration y0 x0 = 
  findIndex escaped $ take maxIterations iterations 
 where
  escaped (y, x) = x * x + y * y >= 4
  iterations = iterate (mandelbrotIteration y0 x0) (0, 0)

mandelbrotColor :: Maybe Int -> (Word8, Word8, Word8)
mandelbrotColor Nothing = (0, 0, 0)
mandelbrotColor (Just iteration) = (byte red, byte green, byte blue) 
 where
  fraction = (log (fromIntegral iteration)) / (log (fromIntegral maxIterations))
  hue = 360 * fraction
  saturation = 1
  lightness = fraction
  RGB red green blue = hsl hue saturation lightness
  byte x = fromIntegral $ round (x * 255)

-- From http://en.wikipedia.org/wiki/Mandelbrot_set#Escape_time_algorithm
mandelbrotEscapeTime :: Double -> Double -> (Word8, Word8, Word8)
mandelbrotEscapeTime y0 x0 = 
  mandelbrotColor $ mandelbrotEscapeIteration y0 x0


pixels :: Int -> Int -> [(Word8, Word8, Word8)]
pixels height width = do
 yPixel <- [0 .. height - 1]
 xPixel <- [0 .. width - 1]
 let yScaled = (fromIntegral yPixel) / (fromIntegral height) * 2 - 1
 let xScaled = (fromIntegral xPixel) / (fromIntegral width) * 3.5 - 2.5
 return $ mandelbrotEscapeTime yScaled xScaled 


main :: IO ()
main = do
 args <- getArgs
 putStrLn $ show args 
 let [heightString, filename] = args
 let height = read heightString :: Int
 let width = round $ (3.5 / 2.0) * (fromIntegral height)
 let arr = fromList (Z :. height :. width) (pixels height width) :: Array U DIM2 (Word8, Word8, Word8)
 writeImageToBMP filename arr
 putStrLn $ show height
 putStrLn filename
 putStrLn "Done"

