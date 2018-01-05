module Counter
( writeToFile
, count
, repeatNTimes
, convertToVec
) where
    
import System.Directory
import Data.List
--import Control.Monad
--import Control.Applicative 

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


convertToVec :: Num a => [String] -> [String] -> [a]
convertToVec [] words = []
convertToVec pattern words
    | ((\\) [head pattern] words) /= [] = 0 : (convertToVec (tail pattern) (words))
    | ((\\) [head pattern] words)== [] = 1 : (convertToVec (tail pattern) (words))
    
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

    
 --putStrLn (show ((\\) (take 80 (fmap (\(x,y)-> y) spam )) (take 80 (fmap (\(x,y)-> y)ham))))

    

