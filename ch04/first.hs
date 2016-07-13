-- Exercise 3 on page 84

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = firstWords

firstWords content =  unlines $ map (\line -> safeHead (words line)) (lines content)
    where safeHead xs
            | null xs = []
            | otherwise = head xs
