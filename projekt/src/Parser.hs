module Parser
( substring
, prefix
, parse_all
, tooLower
, delete_punctuation_mark
, parseChar 
) where
import NLP.Snowball
import qualified Data.Text as T
import Data.Text.IO as TIO 
import Data.Char
import System.Directory
import Data.List
import Control.Exception



deleteAppendix [] = []
deleteAppendix ('\n':'\n':xs) = xs
deleteAppendix (x:xs) = deleteAppendix xs



parseChar :: String -> String
parseChar s 
                | '@' `elem` s = "email"
                | '%' `elem` s = "percent"
                | Prelude.any isDigit s = "number"
                | '$' `elem` s = "dollar"
                | substring "http" s = "URL"
                | '<' `elem` s = ""
                | '>' `elem` s = ""
                | '|' `elem` s = ""
                | otherwise = s

delete_punctuation_mark :: String -> String
delete_punctuation_mark s  
    | last s == '.' = reverse (delete (last s) (reverse s))
    | last s == ';' = reverse (delete (last s) (reverse s))
    | last s == ',' = reverse (delete (last s) (reverse s))
    | last s == ':' = reverse (delete (last s) (reverse s))
    | last s == '!' = reverse (delete (last s) (reverse s))
    | last s == '?' = reverse (delete (last s) (reverse s))
    | otherwise = s



substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

tooLower :: String -> String
tooLower "" = []
tooLower a = (toLower (head a) ) : (tooLower (tail a))


parse s = do
    ws0 <- Prelude.readFile s
    let ws1 = fmap (T.append (T.pack " ")) . fmap (stem English) . fmap (T.pack) . fmap parseChar . fmap delete_punctuation_mark . fmap tooLower $ Prelude.words (deleteAppendix ws0)
    TIO.writeFile (s++"_parsed") (foldl1 T.append ws1)
    return ()

parseCatched s = catch (parse s) handler
    where
    handler :: SomeException -> IO ()
    handler ex = Prelude.putStrLn $ "Caught exception: " ++ show ex




repeatNTimes 0 _ _ = return ()
repeatNTimes n action (x:xs) =
    do
    action x
    repeatNTimes (n-1) action xs



deleteUnParsed files = do
    repeatNTimes (length files) removeFile files


deleteEmpty s = do
    fcontents <- Prelude.readFile s;
    if ((length fcontents) == 0) then do removeFile s else return () 
    return ()

deleteAllEmpties = do
    files <- System.Directory.getDirectoryContents =<< System.Directory.getCurrentDirectory;
    let onlyFiles = delete "." $ delete ".." files
    repeatNTimes (length onlyFiles) deleteEmpty onlyFiles
    return ()


parse_all = do
    files <- System.Directory.getDirectoryContents =<< System.Directory.getCurrentDirectory;
    let onlyFiles = delete "." $ delete ".." files
    repeatNTimes (length onlyFiles) parseCatched onlyFiles
    deleteUnParsed onlyFiles
    deleteAllEmpties
    return 1;

