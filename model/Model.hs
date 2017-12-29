{-# LANGUAGE FlexibleContexts #-}
module Model (
    sigmoid,
    readToArray,
    costFunction,
    vectorByVector,
    matrixByVector,
    hypothesis,
    hvec
    ) where

import Counter
import Data.String


sigmoid x = 1/(1 + exp (-x))


readToArray = do
    spamArray' <- readFile "SpamEmails";
    let spamCount = length (lines spamArray');

    hamArray' <- readFile "HamEmails";
    let hamCount = length (lines hamArray');    

    let labels = [1 | x <- [1..spamCount]] ++ [0 | y <- [1..hamCount]];


    let emailArray' = spamArray' ++ hamArray';
    let emailArray = fmap words (lines spamArray');

    pattern' <- readFile "top500inSpam";
    let pattern = words pattern';

    let vectorsMatrix = fmap (convertToVec pattern) emailArray;

    return (vectorsMatrix, labels)

    
vectorByVector [] [] = 0
vectorByVector v1 v2 = (head v1)*(head v2) + vectorByVector (tail v1) (tail v2)


matrixByVector [] _ = []
matrixByVector m v = (vectorByVector (head m) v) : (matrixByVector (tail m) v)

hypothesis theta x = sigmoid (vectorByVector theta x)

hvec :: Floating a => [a] -> [[a]] -> [a]
hvec _ [] = []
hvec theta vectors =  (hypothesis theta (head vectors)) : (hvec theta (tail vectors))

costFunction :: Floating a => [[a]] -> [a] -> [a] -> a
costFunction vectors labels theta = 
 (vectorByVector (fmap (\x -> -x) labels) (fmap log  (hvec theta vectors)) - vectorByVector (fmap (\x -> 1-x) labels) (fmap log (fmap (\x -> 1-x) (hvec theta vectors)))) / (fromIntegral (length vectors)) 


 --gradient vectors labels thea =
