import NLP.Snowball
import qualified Data.Text as T
import Data.Text.IO as TIO 
import Data.Char
import System.Directory


parseChar :: String -> String
parseChar s 
                | '@' `elem` s = "email"
                | '%' `elem` s = "percent"
                | Prelude.any isDigit s = "number"
                | '$' `elem` s = "dollar"
                | otherwise = s


parse s = do 
    ws0 <- Prelude.readFile s;
    let ws1 = Prelude.words ws0;
    let ws2 = fmap parseChar ws1;
    let ws3 = fmap (T.pack) ws2;
    let ws4 = fmap (stem English) ws3;
    let ws5 = fmap (T.append (T.pack " ")) ws4
    TIO.writeFile (s++"parsed") (foldl1 T.append ws5)
    return ws5 




-- main = do dirContent <- (getCurrentDirectory >>= getDirectoryContents )
    
--     ws0 <- (dirContent >>= readFile);
--     let ws1 = Prelude.words ws0;
--     let ws2 = fmap parseChar ws1;
--     let ws3 = fmap (T.pack) ws2;
--     let ws4 = fmap (stem English) ws3;
--     T.writeFile dirContent ws4
--     return ws4



    