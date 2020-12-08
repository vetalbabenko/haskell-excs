module CaesarCipher where

import System.Random
import Data.Char(isLetter)
import System.IO
import Control.Monad (sequence)
import Lib

main :: IO ()
main = do
       putStrLn "Please, enter a string for encryption: "
       -- need to force flash putStrLn to stdout
       hFlush stdout
       forEncryption <- getLine
       randomShift <- random'
       -- Encrypting user input
       encrypted <- encrypt forEncryption randomShift
       putStrLn ("String was encrypted by shiftig each letter by " ++ show randomShift ++ ": " ++ show encrypted)
       hFlush stdout
       putStrLn "Decripting ..."
       hFlush stdout
       -- Trying to decrypt message
       allSolutions <- brutForceDecrypt encrypted
       -- Filter all brut forced solutions by identifying common words.
       let filteredSolutions = filter containCommonWords allSolutions
       putStrLn ("All brutforce solutions: " ++ show allSolutions )
       putStrLn ("Filtered by common words solutions: " ++ show filteredSolutions )


-- Encrypting function, which iterates through given string and shift forwards each letter by given number.
encrypt :: String -> Int -> IO String
encrypt str shift = do
                    return (map (\c -> if isLetter c then shiftForwards c shift else c ) str)

-- Decryption function, which iterates through given string and shift backwards each letter by given number.
decrypt :: String -> Int -> IO String
decrypt str shift = do
                    return (map (\c -> if isLetter c then shiftBackwards c shift else c ) str)

-- Brut-force decryptor. As we only have 26 letters in English, then we can easily iterate by each shift and find out
-- our encrypted message.
brutForceDecrypt :: String -> IO [String]
brutForceDecrypt str = do
             sequence (map (\shift -> decrypt str shift ) ( take 26 [1,2..]))

-- Checks brut-forced solution if they are contains common words or not.
-- More words contains in common array, more accurate function will identify encrypted message.
containCommonWords :: String -> Bool
containCommonWords str = let splittedWords = words str
                             commonWords = ["the", "of", "and", "a", "to", "in", "is", "you", "that", "it", "at", "be", "its"]
                         in nonEmpty (filter (\w -> nonEmpty (filter (== w) splittedWords) ) commonWords )


-- Function which shifts letter backwards by give number, needed for encryption.
-- If we reach Z/z, then we back to A/a, to correctly do a shifting.
shiftForwards :: Char -> Int -> Char
shiftForwards c 0 = c
shiftForwards 'z' shift = shiftForwards 'a' (pred shift)
shiftForwards 'Z' shift = shiftForwards 'A' (pred shift)
shiftForwards c shift = shiftForwards (succ c) (pred shift)

-- Function which shifts letter backwards by give number, needed for decrypting.
-- If we reach A/a, then we back to Z/z, to correctly do a shifting.
shiftBackwards :: Char ->  Int -> Char
shiftBackwards c 0 = c
shiftBackwards 'a' shift = shiftBackwards 'z' (pred shift)
shiftBackwards 'A' shift = shiftBackwards 'Z' (pred shift)
shiftBackwards c shift = shiftBackwards (pred c) (pred shift)

-- Random number generator. As english language has only 26 letters, so we can only shift by 26 max
random' :: IO Int
random' = do randomRIO (1, 26)

-- Help functions, checks whether list empty
nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty _ = True
