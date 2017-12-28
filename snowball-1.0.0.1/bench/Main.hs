{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-------------------------------------------------------------------------------
import           Control.Applicative      ((<$>))
-------------------------------------------------------------------------------
import           Criterion.Main           (Pure, bench, bgroup, defaultMain,
                                           nf)
-------------------------------------------------------------------------------
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
-------------------------------------------------------------------------------
import           Language.Haskell.Extract (functionExtractorMap)
-------------------------------------------------------------------------------
import           NLP.Snowball
-------------------------------------------------------------------------------

main :: IO ()
main = do
    ws <- take 50000 . Text.lines <$> Text.readFile "/usr/share/dict/words"
    defaultMain
      [ bgroup "100 words"
          $(functionExtractorMap "^bench_"
             [| \name benchmark -> bench (drop 6 name) (benchmark (take 100 ws)) |])
      , bgroup "50k words"
          $(functionExtractorMap "^bench_"
             [| \name benchmark -> bench (drop 6 name) (benchmark ws) |])
      ]

bench_stem :: [Text] -> Pure
bench_stem = nf $ foldr ((:) . stem English) []

bench_map_stem :: [Text] -> Pure
bench_map_stem = nf $ map (stem English)

bench_stems :: [Text] -> Pure
bench_stems = nf $ stems English

bench_stemWith :: [Text] -> IO [Text]
bench_stemWith ws = do
    english <- newStemmer English
    mapM (stemWith english) ws

bench_stemsWith :: [Text] -> IO [Text]
bench_stemsWith ws = do
    english <- newStemmer English
    stemsWith english ws
