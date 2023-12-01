{-# LANGUAGE ViewPatterns #-}

module Day01 where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.IO (readFile)


input :: IO [String]
input = readFile "input.txt" >>= return . lines

updateTens :: Maybe Int -> Int -> Maybe Int
Nothing `updateTens` x = Just $ 10 * x + x
(Just n) `updateTens` x = Just $ (10 * x) + (n `mod` 10)

formNumber :: String -> Maybe Int
formNumber [] = Nothing
formNumber (x : xs) | isDigit x = formNumber xs `updateTens` x'
                      where x' = digitToInt x
formNumber (_ : xs) = formNumber xs

part1 :: IO Int
part1 = do
  lines <- input
  return . fromJust . foldr (\x y -> (+) <$> x <*> y) (Just 0) $ map formNumber lines

formNumber' :: String -> Maybe Int
formNumber' [] = Nothing
formNumber' str@(_:xs) | "one" `isPrefixOf` str = formNumber' xs `updateTens` 1
formNumber' str@(_:xs) | "two" `isPrefixOf` str = formNumber' xs `updateTens` 2
formNumber' str@(_:xs) | "three" `isPrefixOf` str = formNumber' xs `updateTens` 3
formNumber' str@(_:xs) | "four" `isPrefixOf` str = formNumber' xs `updateTens` 4
formNumber' str@(_:xs) | "five" `isPrefixOf` str = formNumber' xs `updateTens` 5
formNumber' str@(_:xs) | "six" `isPrefixOf` str = formNumber' xs `updateTens` 6
formNumber' str@(_:xs) | "seven" `isPrefixOf` str = formNumber' xs `updateTens` 7
formNumber' str@(_:xs) | "eight" `isPrefixOf` str = formNumber' xs `updateTens` 8
formNumber' str@(_:xs) | "nine" `isPrefixOf` str = formNumber' xs `updateTens` 9
formNumber' (x : xs) | isDigit x = formNumber' xs `updateTens` x'
                      where x' = digitToInt x
formNumber' (_ : xs) = formNumber' xs

part2 :: IO Int
part2 = do
  lines <- input
  return . fromJust . foldr (\x y -> (+) <$> x <*> y) (Just 0) $ map formNumber' lines
