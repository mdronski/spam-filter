{-# LANGUAGE FlexibleContexts #-}
module Model (
    sigmoid,
    readToArray
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