module Main (main) where

import Lib (
  handleArgsAndGenPng,
  defaultDummyName,
  availableGenFunctions,
  availableGenFunctionsStr,
  availableUnitsStr
  )

import Options.Applicative


data DummyArgs = DummyArgs
  { _size           :: Int
  , _units          :: String
  , _output         :: String 
  , _output_dir     :: String
  , _gen_func       :: String 
  , _dump_func_list :: Bool }


dArgs :: Parser DummyArgs
dArgs = DummyArgs
      <$> option auto
          ( long "size"
         <> short 's'
         <> metavar "SIZE"
         <> showDefault
         <> value 100
         <> help "Target size for dummy in UNITS" )
      <*> strOption
          ( long "units"
         <> short 'u'
         <> metavar availableUnitsStr
         <> help ("One of " ++ availableUnitsStr)
         <> showDefault
         <> value "kb" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> help "Output name"
         <> showDefault
         <> value defaultDummyName
         <> metavar "NAME" )
      <*> strOption
          ( long "output-dir"
         <> short 'd'
         <> help "Output dir"
         <> showDefault
         <> value "./"
         <> metavar "PATH" )
      <*> strOption
          ( long "gen-func"
         <> short 'f'
         <> help ("The function to generate. Available -> "++availableGenFunctionsStr)
         <> showDefault
         <> value (fst (head availableGenFunctions))
         <> metavar "GEN_FUNC" )
      <*> switch
          ( long "dump-func-list"
         <> help "List available functions and exit")

main :: IO ()
main = checkAndRun =<< execParser opts
  where
    opts = info (dArgs <**> helper)
      ( fullDesc
     <> header "hsDummyImage - tool for generating images with target file size" )

checkAndRun :: DummyArgs -> IO ()
checkAndRun (DummyArgs tSize tUnits tName tDir tFunc dump_func_l)
  | dump_func_l = putStr availableGenFunctionsStr
  | otherwise = handleArgsAndGenPng tSize tUnits tName tDir tFunc