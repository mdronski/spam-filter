{-# LANGUAGE FlexibleContexts #-}
{-|
Module       : Model
Description  : Machine learnig algorithm
-}
module Model (
    hypothesis,
    sigmoid
    ) where

import Counter
import Data.String
import System.Random (randomRIO)
import Data.Matrix

-- read emails an convert to binary vectors to make them suitable for ML algorithm
readToArray :: Fractional a => IO (Matrix a, Matrix a, Matrix a)
readToArray = do
    spamArray'' <- readFile "SpamEmails";
    let spamArray' = lines spamArray'';
    let spamCount = length  spamArray';

    hamArray'' <- readFile "HamEmails";
    let hamArray' = lines hamArray'';
    let hamCount = length hamArray';    

    let emailArray = words <$> (spamArray' ++ hamArray');

    pattern' <- readFile "wordsFromHamSet1300";
    let pattern = words pattern';

    let vectorsMatrix' = fmap (convertToVec pattern) emailArray;
    let vectorsMatrix = fmap fromIntegral (fromLists vectorsMatrix');

    let theta = [0.01  | x <- [1..(ncols vectorsMatrix)]];
    let t = fromList 1 (ncols vectorsMatrix) theta

    let labels' = [1 | x <- [1..spamCount]] ++ [0 | y <- [1..hamCount]];
    let labels = fmap (fromIntegral) (fromList (length labels') 1 labels');

    return (vectorsMatrix, labels, t)


-- | Gradient descent iteration
repeatGradient _ _ theta 15 = return (theta)
repeatGradient matrix labels theta n =
 do
  putStrLn (show theta )
  let gradients = fromList 1 ((ncols matrix)) (gradient matrix labels theta 1)
  let newTheta = elementwise (\x y -> x - y) theta gradients
  
  repeatGradient matrix labels newTheta (n+1)


-- | Sigmoid function
sigmoid :: Floating a => a -> a 
sigmoid x = 1/(1 + exp (-x))

-- | Calculate hypothesis (possibility of being spam or not) for email
hypothesis :: Floating a =>  Matrix a -> Matrix a ->  a
hypothesis x theta =  sigmoid $ trace (multStd2 x (transpose theta) )

--  calculate hypothesis matrix (possibility of being spam or not) for emails matrix
hvec matrix theta = fmap sigmoid (multStd2 matrix (transpose theta))  

-- | Single gradient step
singlegradient :: Floating a => Matrix a -> Matrix a -> Matrix a -> Int -> a
singlegradient matrix labels theta n =
   sum (elementwise (\x y -> x*y) (elementwise (\x y -> x-y) (hvec matrix theta) labels) (submatrix 1 (nrows matrix) n n matrix)) /(fromIntegral (nrows matrix))

-- | Calculate a gradient 
gradient matrix labels theta n 
    | n == ((ncols matrix) +1) = []
    | otherwise = ((singlegradient matrix labels theta n)) : (gradient matrix labels theta (n+1))

-- | Gradient Descent iteration
gradientDescent matrix labels theta n 
    | n==30 = theta
    | otherwise = gradientDescent matrix labels (elementwise (\x y -> x + y) (fromList 1 (ncols theta) (gradient matrix labels theta 1)) theta) (n+1)

