{-|
Module       : Counter
Description  : Used to count words frequencies in the emails.
-}
module Counter
( count
, convertToVec
) where
    
import System.Directory
import Data.List

repeatNTimes 0 _ _ = return ()
repeatNTimes n action (x:xs) =
 do
  action x
  repeatNTimes (n-1) action xs


writeToFile file = do
    contents <- Prelude.readFile file;
    Prelude.appendFile "AllEmails" contents
    Prelude.appendFile "AllEmails" "\n"
    return ()

-- | Convert string array into the binary vector, checking if it contains words included in patterns array 
convertToVec [] words = []
convertToVec pattern words
    | ((\\) [head pattern] words) /= [] = 0 : (convertToVec (tail pattern) (words))
    | ((\\) [head pattern] words) == [] = 1 : (convertToVec (tail pattern) (words))
    
-- | Count word frequencies and shows tuples containing word and it's frequency
count = do
    files <- System.Directory.getDirectoryContents =<< System.Directory.getCurrentDirectory;
    let onlyFiles = delete "." $ delete ".." $ delete "Counter.hs"  files;
    repeatNTimes (length onlyFiles) writeToFile onlyFiles
    allContent <- Prelude.readFile "AllEmails"
    let allWords = words allContent
    let sortedWords = sort allWords
    let groupedWords = group sortedWords
    let mappedWords = map (\x -> (length x, head x)) groupedWords
    let sortedMappedWords = reverse (sort mappedWords)
    writeFile "wordsFrequency" (unwords $ fmap (\(x,y)-> y) sortedMappedWords)
    return sortedMappedWords

    

    


