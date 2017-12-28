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

    return ( labels)

