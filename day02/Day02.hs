{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Day02 where

import Data.List (stripPrefix, isSuffixOf)
import Data.List.Split (splitOn)
import System.IO (readFile)

input :: IO [String]
input = readFile "input.txt" >>= return . lines

data Reveal = Reveal
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving Show

data Game = Game
  { id :: Int
  , revealed :: [Reveal]
  } deriving Show

parseGame :: String -> Game
parseGame (stripPrefix "Game " -> Just rest) = Game n (parseReveals rest')
  where n = read . takeWhile (/= ':') $ rest
        _:rest' = dropWhile (/= ' ') rest
        parseReveals :: String -> [Reveal]
        parseReveals str = let
          draws :: [[String]] = map (splitOn ", ") (splitOn "; " str)
          in map (foldr (\draw reveal -> case () of
                            _ | "red" `isSuffixOf` draw -> reveal{red=read . takeWhile (/=' ') $ draw}
                              | "green" `isSuffixOf` draw -> reveal{green=read . takeWhile (/=' ') $ draw}
                              | "blue" `isSuffixOf` draw -> reveal{blue=read . takeWhile (/=' ') $ draw})
                   (Reveal 0 0 0)) draws

possibleGame :: Game -> Bool
possibleGame Game{revealed=[]} = True
possibleGame g@Game{revealed=(Reveal{..} : rs)} | red <= 12 && green <= 13 && blue <= 14
  = possibleGame g{revealed=rs}
possibleGame _ = False

part1 :: IO Int
part1 = do
  lines <- input >>= return . map parseGame
  return . sum . map Day02.id . filter possibleGame $ lines

minimumReveal :: [Reveal] -> Reveal
minimumReveal = foldr (\rev acc -> Reveal { red = max (red rev) (red acc)
                                          , green = max (green rev) (green acc)
                                          , blue = max (blue rev) (blue acc)
                                          }) (Reveal 0 0 0)

power :: Reveal -> Int
power Reveal{..} = red * green * blue

part2 :: IO Int
part2 = do
  lines <- input >>= return . map parseGame
  return . sum . map (power . minimumReveal . revealed) $ lines
