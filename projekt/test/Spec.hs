import Test.HUnit
import Counter
import Parser
import Control.Monad
import Test.QuickCheck


delete_punctuation_mark_test :: String -> Bool
delete_punctuation_mark_test s = delete_punctuation_mark (s ++ ".") == s

parseChar_test :: String -> String -> Bool
parseChar_test s c = parseChar (s ++ "@" ++ c) == "email"


main = do
    let test1 = TestCase (assertEqual "substring test" True (substring "ala" "ma ala kota")) 
    let test2 = TestCase (assertEqual "prefix test" True (prefix "http" "http://clam.com")) 
    let test3 = TestCase (assertEqual "tooLower test" "ala ma kota" (tooLower "Ala ma Kota"))
    let test4 = TestCase (assertEqual "convertToVec test" [1,1,0] (convertToVec ["ala","ma","kota"] ["ma","ala"]))
    let test5 = TestCase (assertEqual "bad list" [3] (tail [1,3]))
    let tests = TestList [TestLabel "test1" test1, 
                      TestLabel "test2" test2,
                      TestLabel "test3" test3,
                      TestLabel "test4" test4,
                      TestLabel "test5" test5]
    runTestTT tests
    quickCheck delete_punctuation_mark_test 
    quickCheck parseChar_test
    