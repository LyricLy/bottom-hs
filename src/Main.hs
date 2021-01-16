module Main where

import Data.Maybe
import System.IO
import Control.Monad
import Data.Semigroup ((<>))
import Control.Applicative

import Options.Applicative

import Bottom


data Options = Options
  { bottomify :: Bool
  , inputFile :: Maybe String
  , outputFile :: Maybe String
  , text :: Maybe String
  } deriving Show

parseOptions :: Parser Options
parseOptions = Options
  <$> (flag' True (long "bottomify" <> short 'b' <> help "Translate text to bottom")
   <|> flag' False (long "regress" <> short 'r' <> help "Translate bottom to human-readable text (futile)"))
  <*> optional (strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input file [Default: TEXT/stdin]"))
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file [Default: stdout]"))
  <*> ((unwords <$>) <$> (optional . some) (strArgument (metavar "TEXT")))


main :: IO ()
main = execParser opts >>= mainBottom
  where
    opts =
      info (parseOptions <**> helper)
        (fullDesc
      <> progDesc "Fantastic CLI for translating between bottom and human-readable text"
      <> header "Bottom translator 1.0.0")

mainBottom :: Options -> IO ()
mainBottom o = do
  input <- fromJust . msum $ [readFile <$> inputFile o, return <$> text o, return getContents]
  let result = if bottomify o then Just $ encodeString input else decodeString input 
  case result of
    Nothing -> hPutStrLn stderr "The input was invalid."
    Just r -> case outputFile o of
      Nothing -> putStrLn r
      Just f -> writeFile f r
