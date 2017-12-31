{-# LANGUAGE FlexibleContexts #-}
module Model (
    sigmoid,
    readToArray,
    costFunction,
    vectorByVector,
    matrixByVector,
    hypothesis,
    hvec,
    singleGradient,
    gradient,
    ) where

import Counter
import Data.String
import System.Random (randomRIO)
import Data.Matrix

sigmoid x = 1/(1 + exp (-x))


readToArray = do
    spamArray' <- readFile "SpamEmails";
    let spamCount = length (lines spamArray');

    hamArray' <- readFile "HamEmails";
    let hamCount = length (lines hamArray');    

    let labels = [1 | x <- [1..spamCount]] ++ [0 | y <- [1..hamCount]];


    let emailArray' = spamArray' ++ hamArray';
    let emailArray = fmap words (lines spamArray');

    pattern' <- readFile "HamSpamDiffrence";
    let pattern = take 100 $ words pattern';

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


vectorMinusvector [] _ = []
vectorMinusvector _ [] = []
vectorMinusvector (x:xs) (y:ys) = (x-y) : (vectorMinusvector xs ys)

vectorPlusvector [] _ = []
vectorPlusvector (x:xs) (y:ys) = (x+y) : (vectorPlusvector xs ys)

elementByElement [] _ = []
elementByElement (x:xs) (y:ys) = (x*y) : (elementByElement xs ys)

--gradient :: Floating a => [[a]] -> [a] -> [a] -> a -> [a]
gradient vectors labels theta n  
    | n == (length theta) = []
    | otherwise = (singleGradient vectors labels theta n) : (gradient vectors labels theta (n+1))

vectorByNElemOfVec v1 v2 n = fmap (\x -> x*(v2 !! n)) v1 

nVecFromVectors [] _ = []
nVecFromVectors (x:xs) n = (x !! n) : (nVecFromVectors xs n) 

singleGradient vectors labels theta n =
   (sum (zipWith (*) (vectorMinusvector (hvec theta vectors) labels) (nVecFromVectors vectors n))) /(fromIntegral (length vectors))


gradientDescent vectors labels theta n  
    | (n == 15) = theta
    | otherwise = gradientDescent vectors labels (vectorPlusvector theta (gradient vectors labels theta 0)) (n+1)  


randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 


-- (vectors,labels) <- readToArray 
-- let v = fmap (fmap fromIntegral) vectors;
-- let l = fmap fromIntegral labels;
-- let t = [0.001 * (exp 1) | x <- [1..(length(head v))]];

firstStep vectors labels theta  = 
    trace (multStd (Data.Matrix.transpose ((-) labels (multStd vectors theta))) ((-) labels (multStd vectors theta)) / (multStd ((-) labels (multStd vectors theta)) (multStd vectors ((-) labels (multStd vectors theta)))))

    
gradientDescent1 vectors labels theta n 
    | (n == 15 ) = theta
    | otherwise = gradientDescent1 vectors labels ((-) labels ((*) vectors ((+) theta (fmap (\x->x*(firstStep vectors labels theta )) labels) ) )) (n+1)

