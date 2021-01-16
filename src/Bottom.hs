module Bottom (decodeString, encodeString) where

import Data.List.Split (endBy)

encodeChar :: Char -> String
encodeChar '\0' = "❤️"
encodeChar c =
  let (hug, r)           = divMod (fromEnum c) 200
      (heartSparkle, r') = divMod r            50
      (sparkle, r'')     = divMod r'           10
      (pleading, comma)  = divMod r''          5
  in replicate hug '\129730'
  ++ replicate heartSparkle '💖'
  ++ replicate sparkle '✨'
  ++ replicate pleading '🥺'
  ++ replicate comma ','
  ++ "👉👈"

encodeString :: String -> String
encodeString = concatMap encodeChar


decodeChar :: String -> Maybe Char
decodeChar = fmap (toEnum . sum) . sequence . map val
  where val '\129730' = Just 200
        val '💖' = Just 50
        val '✨' = Just 10
        val '🥺' = Just 5
        val ',' = Just 1
        val '\10084' = Just 0  -- heart
        val '\65039' = Just 0  -- variation selector 16
        val '\n' = Just 0      -- ignore newlines
        val _ = Nothing
        

decodeString :: String -> Maybe String
decodeString = sequence . map decodeChar . endBy "👉👈"
