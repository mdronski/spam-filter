module Main where

import Lib
import Parser
import Counter
--import parser2
import System.Directory
import Data.List

 
main = do
    setCurrentDirectory "/home/bartek/Pulpit/spam-filter-mdronski/dataset/OGOLNY/spam_1";
    parse_all
    --let x <- sort [1,2,3]
    spam <- count
    putStrLn "jestes za polowa"
    --putStrLn (show (take 40 (fmap (\(x,y)-> y) spam )))
    setCurrentDirectory "/home/bartek/Pulpit/spam-filter-mdronski/dataset/OGOLNY/Ham_1"
    parse_all
    ham <- count
    putStrLn (show ((\\) (take 80 (fmap (\(x,y)-> y) spam )) (take 80 (fmap (\(x,y)-> y)ham))))
    --convertToVec pattern i_do_porownania