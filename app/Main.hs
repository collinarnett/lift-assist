import Data.Fixed (mod')
import Data.Semigroup ((<>))
import Options.Applicative
import Text.Pretty.Simple (pPrint)

data Config = Config {total :: Float, plateSet :: String}

toFloats str =
  let toFloat x = read x :: Float
   in map toFloat (words str)

config :: Parser Config
config =
  Config
    <$> option
      auto
      ( long "total"
          <> help "Total weight being lifted"
          <> metavar "TOTAL"
      )
    <*> strOption
      ( long "plates"
          <> help "Plates availible for the barbell"
          <> metavar "PLATES"
      )

plates :: Float -> Float -> Float
plates total weight =
  let plate' = fromInteger . floor $ total / weight
   in if mod' plate' 2 == 1 then plate' - 1 else plate'

setWeight :: Float -> [Float] -> [Float]
setWeight total [] = []
setWeight total (weight : weights) =
  let plate = plates total weight
   in plate : setWeight (total - weight * plate) weights

main = parse =<< execParser opts
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Decompose a total weight into plates"
            <> header "lift-assist - an assistant for powerlifting"
        )

parse :: Config -> IO ()
parse (Config t p) = pPrint $ setWeight t $ toFloats p

