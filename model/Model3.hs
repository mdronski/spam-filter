 {-# OPTIONS_GHC -Wall                    #-}
 {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
 {-# OPTIONS_GHC -fno-warn-type-defaults  #-}
 {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
 
 {-# LANGUAGE TupleSections #-}
 
 module Logistic ( betas
                 , main
                 , a
                 , b
                 , nSamples
                 ) where

import Numeric.AD
import Numeric.AD.Types
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.State
import Data.List
import Text.Printf
import Fata.Matrix

readToArray :: Fractional a => IO (Matrix a, Matrix a, Matrix a)
readToArray = do
    spamArray' <- readFile "SpamEmails";
    let spamCount = length (lines spamArray');

    hamArray' <- readFile "HamEmails";
    let hamCount = length (lines hamArray');    

    let labels' = [1 | x <- [1..spamCount]] ++ [0 | y <- [1..hamCount]];
    let labels = fmap (fromIntegral) (fromList (length labels') 1 labels');

    let emailArray' = spamArray' ++ hamArray';
    let emailArray = fmap words (lines spamArray');

    pattern' <- readFile "top1500diffrence";
    let pattern = words pattern';

    let vectorsMatrix' = fmap (convertToVec pattern) emailArray;
    let vectorsMatrix = fmap fromIntegral (fromLists vectorsMatrix');

    let theta = [0.1  | x <- [1..(ncols vectorsMatrix)]];
    let t = fromList 1 (ncols vectorsMatrix) theta

    return (vectorsMatrix, labels, t)



logit :: Floating a => a -> a
logit x = 1 / (1 + exp (negate x))

logLikelihood :: Floating a => V.Vector a -> a -> V.Vector a -> a
logLikelihood theta y x = y * log (logit z) + (1 - y) * log (1 - logit z)
   where
     z = V.sum $ V.zipWith (*) theta x

totalLogLikelihood :: Floating a =>
                       V.Vector a ->
                       V.Vector a ->
                       V.Vector (V.Vector a) ->
                       a
totalLogLikelihood theta y x = (a - delta * b) / l
   where
     l = fromIntegral $ V.length y
     a = V.sum $ V.zipWith (logLikelihood theta) y x
     b = (/2) $ V.sum $ V.map (^2) theta




delTotalLogLikelihood :: Floating a =>
                 V.Vector a ->
                 V.Vector (V.Vector a) ->
                 V.Vector a ->
                 V.Vector a
delTotalLogLikelihood y x = grad f
   where
     f theta = totalLogLikelihood theta (V.map auto y) (V.map (V.map auto) x)
 
stepOnce :: Double ->
             V.Vector Double ->
             V.Vector (V.Vector Double) ->
             V.Vector Double ->
             V.Vector Double
stepOnce gamma y x theta =
   V.zipWith (+) theta (V.map (* gamma) $ del theta)
   where
     del = delTotalLogLikelihood y x