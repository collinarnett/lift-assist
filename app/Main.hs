import Data.Fixed (mod')
import Text.Pretty.Simple (pPrint)

plates :: Float -> Float -> Float
plates total weight =
  let plate' = fromInteger . floor $ total / weight
   in if mod' plate' 2 == 1 then plate' - 1 else plate'

setWeight :: Float -> [Float] -> [Float]
setWeight total (weight : weights) =
  let plate = plates total weight
   in plate : setWeight (total - weight * plate) weights

main = pPrint $ setWeight 115 [45, 35, 25, 10, 5, 2.5]
