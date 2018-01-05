{-# LANGUAGE FlexibleContexts #-}
module NewModel (
    sigmoid,
    readToArray,
 --   costFunction,
    vectorByVector,
    matrixByVector,
    hypothesis,
    hvec,
 --   singleGradient,
 --   gradient,
    ) where

import Counter
import Data.String
import System.Random (randomRIO)
import Data.Matrix


readToArray :: Fractional a => IO (Matrix a, Matrix a, Matrix a)
readToArray = do
    spamArray'' <- readFile "SpamEmails";
    let spamArray' = lines spamArray'';
    let spamCount = length  spamArray';

    hamArray'' <- readFile "HamEmails";
    let hamArray' = lines hamArray'';
    let hamCount = length hamArray';    

    let emailArray' = spamArray' ++ hamArray';
    let emailArray = fmap words emailArray';

    pattern' <- readFile "wordsFromHamSet1300";
    let pattern = words pattern';

    let vectorsMatrix' = fmap (convertToVec pattern) emailArray;
    let vectorsMatrix = fmap fromIntegral (fromLists vectorsMatrix');

    let theta = [0.01  | x <- [1..(ncols vectorsMatrix)]];
    let t = fromList 1 (ncols vectorsMatrix) theta

    let labels' = [1 | x <- [1..spamCount]] ++ [0 | y <- [1..hamCount]];
    let labels = fmap (fromIntegral) (fromList (length labels') 1 labels');

    return (vectorsMatrix, labels, t)

vectorByVector [] [] = 0
vectorByVector v1 v2 = (head v1)*(head v2) + vectorByVector (tail v1) (tail v2)

matrixByVector [] _ = []
matrixByVector m v = (vectorByVector (head m) v) : (matrixByVector (tail m) v)

vectorMinusvector [] _ = []
vectorMinusvector _ [] = []
vectorMinusvector (x:xs) (y:ys) = (x-y) : (vectorMinusvector xs ys)

vectorPlusvector [] _ = []
vectorPlusvector (x:xs) (y:ys) = (x+y) : (vectorPlusvector xs ys)

elementByElement [] _ = []
elementByElement (x:xs) (y:ys) = (x*y) : (elementByElement xs ys)


--repeatGradient :: Floating a1 => Matrix a1 -> Matrix a1 -> Matrix a1 -> a -> IO (Matrix a1)
repeatGradient _ _ theta 15 = return (theta)
repeatGradient matrix labels theta n =
 do
  putStrLn (show theta )
  let gradients = fromList 1 ((ncols matrix)) (gradient matrix labels theta 1)
  let newTheta = elementwise (\x y -> x - y) theta gradients
  
  repeatGradient matrix labels newTheta (n+1)


sigmoid :: Floating a => a -> a
sigmoid x = 1/(1 + exp (-x))

hypothesis :: Floating a =>  Matrix a -> Matrix a ->  a
hypothesis x theta =  sigmoid $ trace (multStd2 x (transpose theta) )

hvec matrix theta = fmap sigmoid (multStd2 matrix (transpose theta))  

singlegradient :: Floating a => Matrix a -> Matrix a -> Matrix a -> Int -> a
singlegradient matrix labels theta n =
   sum (elementwise (\x y -> x*y) (elementwise (\x y -> x-y) (hvec matrix theta) labels) (submatrix 1 (nrows matrix) n n matrix)) /(fromIntegral (nrows matrix))

gradient matrix labels theta n 
    | n == ((ncols matrix) +1) = []
    | otherwise = ((singlegradient matrix labels theta n)) : (gradient matrix labels theta (n+1))

gradientDescent matrix labels theta n 
    | n==30 = theta
    | otherwise = gradientDescent matrix labels (elementwise (\x y -> x + y) (fromList 1 (ncols theta) (gradient matrix labels theta 1)) theta) (n+1)

-- costFunction :: Floating a => [[a]] -> [a] -> [a] -> a
-- costFunction vectors labels theta = 
--  (vectorByVector (fmap (\x -> -x) labels) (fmap log  (hvec theta vectors)) - vectorByVector (fmap (\x -> 1-x) labels) (fmap log (fmap (\x -> 1-x) (hvec theta vectors)))) / (fromIntegral (length vectors)) 


-- --gradient :: Floating a => [[a]] -> [a] -> [a] -> a -> [a]
-- gradient vectors labels theta n  
--     | n == (length theta) = []
--     | otherwise = (singleGradient vectors labels theta n) : (gradient vectors labels theta (n+1))

-- vectorByNElemOfVec v1 v2 n = fmap (\x -> x*(v2 !! n)) v1 

-- nVecFromVectors [] _ = []
-- nVecFromVectors (x:xs) n = (x !! n) : (nVecFromVectors xs n) 

-- singleGradient vectors labels theta n =
--    (sum (zipWith (*) (vectorMinusvector (hvec theta vectors) labels) (nVecFromVectors vectors n))) /(fromIntegral (length vectors))


-- gradientDescent vectors labels theta n  
--     | (n == 15) = theta
--     | otherwise = gradientDescent vectors labels (vectorPlusvector theta (gradient vectors labels theta 0)) (n+1)  


-- randomList :: Int -> IO([Int])
-- randomList 0 = return []
-- randomList n = do
--   r  <- randomRIO (1,6)
--   rs <- randomList (n-1)
--   return (r:rs) 


-- -- (vectors,labels) <- readToArray 
-- -- let v = fmap (fmap fromIntegral) vectors;
-- -- let l = fmap fromIntegral labels;
-- -- let t = [0.001 * (exp 1) | x <- [1..(length(head v))]];

-- firstStep vectors labels theta  = 
--     trace (multStd (Data.Matrix.transpose ((-) labels (multStd vectors theta))) ((-) labels (multStd vectors theta)) / (multStd ((-) labels (multStd vectors theta)) (multStd vectors ((-) labels (multStd vectors theta)))))

    
-- gradientDescent1 vectors labels theta n 
--     | (n == 15 ) = theta
--     | otherwise = gradientDescent1 vectors labels ((-) labels ((*) vectors ((+) theta (fmap (\x->x*(firstStep vectors labels theta )) labels) ) )) (n+1)


