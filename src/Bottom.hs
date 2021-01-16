module Bottom (decodeString, encodeString) where

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

decodeString :: String -> Maybe String
decodeString = Just
