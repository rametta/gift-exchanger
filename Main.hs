module Main where

import Control.Conditional ((<|))
import Control.Monad (forM)
import Data.Array.IO
  ( IOArray,
    newListArray,
    readArray,
    writeArray,
  )
import qualified Data.List as List
import qualified Data.Map as Map
import Relude ((!!?))
import System.Random (randomRIO)
import Text.CSV (CSV, Record, parseCSVFromFile)

data Country
  = Canada
  | USA
  | Other String
  deriving (Eq, Show)

data MatchType
  = Valid
  | Invalid String
  | NoMatch String
  deriving (Show, Eq)

data Participant = Participant
  { name :: String,
    email :: String,
    country :: Country,
    city :: String,
    address :: Maybe String,
    giftCardsOk :: Bool,
    foodOk :: Bool,
    dietaryRestrictions :: Maybe String,
    wishlist :: Maybe String
  }
  deriving (Show)

data Match = Match
  { giver :: Participant,
    receiver :: Participant
  }
  deriving (Show)

data Init = Init
  { remainingParticipants :: [Participant],
    matches :: Map.Map String Match
  }

-- To be an eligible match, one must satisfy these 3 constraints
-- 1. Must not be yourself
-- 2. Must be in the same country
-- 3. Must be in the same city
-- 4. Must not be the same person who got you
isEligibleMatch :: Map.Map String Match -> Participant -> Participant -> Bool
isEligibleMatch ms g r = email g /= email r && country g == country r && city g == city r && not alreadyTogether
  where
    alreadyTogether = False <| (== email g) . email . receiver <$> Map.lookup (email r) ms

findMatch :: Participant -> [Participant] -> Map.Map String Match -> Maybe Match
findMatch giver ps ms =
  Match giver <$> List.find (isEligibleMatch ms giver) ps

removeParticipant :: Participant -> [Participant] -> [Participant]
removeParticipant p = List.filter $ (/= email p) . email

match :: Init -> Participant -> Init
match acc p =
  case mMatch of
    Just m ->
      Init (removeParticipant (receiver m) remaining) $ Map.insert (email . giver $ m) m currentMatches
    Nothing -> acc
  where
    currentMatches = matches acc
    remaining = remainingParticipants acc
    mMatch = findMatch p remaining currentMatches

createMatches :: [Participant] -> Map.Map String Match
createMatches ps = matches . foldl match init $ ps
  where
    init = Init ps Map.empty

strToCountry :: Maybe String -> Country
strToCountry (Just "Canada") = Canada
strToCountry (Just "US") = USA
strToCountry (Just s) = Other s
strToCountry Nothing = Other "-"

strToMaybe :: String -> Maybe String
strToMaybe "" = Nothing
strToMaybe s = Just s

strToBool :: String -> Bool
strToBool "true" = True
strToBool _ = False

recordToParticipant :: Record -> Participant
recordToParticipant record =
  Participant
    { name = "-" <| record !!? 9,
      email = "-" <| record !!? 1,
      country = strToCountry $ record !!? 7,
      city = "-" <| record !!? 8,
      address = record !!? 2 >>= strToMaybe,
      giftCardsOk = maybe False strToBool $ record !!? 3,
      foodOk = maybe False strToBool $ record !!? 4,
      dietaryRestrictions = record !!? 5 >>= strToMaybe,
      wishlist = record !!? 6 >>= strToMaybe
    }

csvToParticipants :: CSV -> [Participant]
csvToParticipants = map recordToParticipant

showPerson :: Participant -> String
showPerson p = name p ++ "(" ++ c ++ ")"
  where
    c = show $ country p

matchToString :: Match -> String
matchToString m = showPerson g ++ " gives to " ++ showPerson r
  where
    g = giver m
    r = receiver m

matchesToString :: Map.Map String Match -> String
matchesToString = List.intercalate "\n" . map matchToString . Map.elems

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs

isValidMatch :: Map.Map String Match -> Match -> MatchType
isValidMatch ms m =
  if email g /= email r && country g == country r && email g /= receiverGiverEmail
    then Valid
    else Invalid $ email g
  where
    g = giver m
    r = receiver m
    receiverGiverEmail = "" <| email . giver <$> Map.lookup (email r) ms

-- Make sure everybody has a match and everybody receives a gift and all constraints are covered
testMatches :: Map.Map String Match -> [Participant] -> String
testMatches ms =
  List.concatMap ((++ ", ") . show) . notValid . verifications
  where
    verifications = List.map (\p -> NoMatch (email p ++ name p) <| isValidMatch ms <$> Map.lookup (email p) ms)
    notValid = List.filter (/= Valid)

main :: IO ()
main = do
  (Right csv) <- parseCSVFromFile "./data.csv"
  ps <- shuffle . csvToParticipants $ csv
  let ms = createMatches ps
  let res = testMatches ms ps
  writeFile "output.txt" $ matchesToString ms
  putStrLn $ if res == "" then "Success" else res