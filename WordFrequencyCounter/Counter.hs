import System.Directory
import Data.List
--import Control.Monad
--import Control.Applicative 

repeatNTimes 0 _ _ = return ()
repeatNTimes n action (x:xs) =
 do
  action x
  repeatNTimes (n-1) action xs


-- writeToArrayAllFiles files  = loop (return []) files
--     where loop acc (x:[])  = (writeToArray acc x)
--           loop acc (x:xs)  = loop (writeToArray acc x) xs 
    
-- writeToArrayAllFiles files  = loop (length files) (return []) files
--     where loop 1 acc x  = writeToArray acc x
--           loop n acc (x:xs)  = loop (n-1) (writeToArray acc x) xs 

--fun array file = ((Prelude.ReadFile file) >>= words) ++ array

--repeatNTimes (length files) 

-- writeToArray array file = do
--     contents <- Prelude.readFile file;
--     let contentsWords = words contents
--     return  (contentsWords ++ array) 

writeToFile file = do
    contents <- Prelude.readFile file;
    Prelude.appendFile "AllEmails" contents
    Prelude.appendFile "AllEmails" "\n"
    return ()



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
    return sortedMappedWords

