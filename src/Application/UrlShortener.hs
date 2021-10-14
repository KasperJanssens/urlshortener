module Application.UrlShortener (calculateInBase62, calculateInBase10, shortenUrl, shortenedToRowId) where

import           Data.Int                   (Int64)
import           Data.List                  as List
import           Data.Maybe                 (catMaybes, isJust, isNothing)
import qualified Data.Text                  as Text
import           Domain.ShortenedUrl        (ShortenedUrl (..))

dictionary :: [Char]
dictionary =
  [ 'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z'
  ]

shortenUrl :: Int64 -> Maybe ShortenedUrl
shortenUrl i =
  let maybeChars = toChar <$> calculateInBase62 i
   in if isJust (List.findIndex isNothing maybeChars)
        then Nothing
        else Just $ ShortenedUrl $ Text.pack $ catMaybes maybeChars

shortenedToRowId :: ShortenedUrl -> Maybe Int64
shortenedToRowId (ShortenedUrl url) =
  let chars = Text.unpack url
   in let maybeNumbers = fromChar <$> chars
       in if isJust (List.findIndex isNothing maybeNumbers)
            then Nothing
            else Just $ calculateInBase10 $ catMaybes maybeNumbers

toChar :: Int64 -> Maybe Char
toChar i
  | i >= 0 && i < 62 = Just $ dictionary !! fromIntegral i
  | otherwise = Nothing

fromChar :: Char -> Maybe Int64
fromChar c = fromIntegral <$> List.elemIndex c dictionary

calculateInBase62Internal :: Int64 -> [Int64] -> [Int64]
calculateInBase62Internal orig curList =
  let (res, modulo) = divMod orig 62
   in let newList = modulo : curList
       in if res >= 62
            then calculateInBase62Internal res newList
            else res : newList

calculateInBase62 :: Int64 -> [Int64]
calculateInBase62 orig = calculateInBase62Internal orig []

calculateInBase10 :: [Int64] -> Int64
calculateInBase10 l = calculateInBase10Internal $ reverse l

calculateInBase10Internal :: [Int64] -> Int64
calculateInBase10Internal [] = 0
calculateInBase10Internal l =
  let allUp = (* 62) <$> tail l
   in head l + calculateInBase10Internal allUp
