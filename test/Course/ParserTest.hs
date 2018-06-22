{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ParserTest where

import           Data.Ratio         ((%))
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Course.Applicative (pure, (*>), (<*>))
import           Course.Core
import           Course.Functor     ((<$>))
import           Course.Monad       ((=<<))

import           Course.List        (List (..), length)
import           Course.Parser      (ParseResult (..), Parser (..), character,
                                     constantParser, isErrorResult, list, list1,
                                     parse, satisfy, valueParser, (|||))

test_Parser :: TestTree
test_Parser =
  testGroup "Parser"
  [ isErrorResultTest
  , constantParserTest
  , characterTest
  , parserFunctorTest
  , valueParserTest
  , alternativeTest
  ]

isErrorResultTest :: TestTree
isErrorResultTest =
  testGroup "isErrorResult"
  [ testCase "True when UnexpectedEof" $ isErrorResult (UnexpectedEof) @?= True
  , testCase "True when ExpectedEof" $ isErrorResult (ExpectedEof "a") @?= True
  , testCase "True when UnexpectedChar" $ isErrorResult (UnexpectedChar 'a') @?= True
  , testCase "True when UnexpectedString" $ isErrorResult (UnexpectedString "abc") @?= True
  , testCase "False when Result" $ isErrorResult (Result "abc" "a") @?= False
  ]

constantParserTest :: TestTree
constantParserTest =
  testGroup "constantParser"
  [ testCase "always return given result" $ parse (constantParser $ Result "abc" 'z') "Lorem" @?= Result "abc" 'z' ]

characterTest :: TestTree
characterTest =
  testGroup "character"
  [ testCase "read character in input" $ parse character "abc" @?= Result "bc" 'a'
  , testCase "fail when input is empty" $ parse character "" @?= UnexpectedEof
  ]

parserFunctorTest :: TestTree
parserFunctorTest =
  testGroup "parserFunctor"
  [ testCase "parser can map" $ parse (toUpper <$> character) "amz" @?= Result "mz" 'A' ]

valueParserTest :: TestTree
valueParserTest =
  testGroup "valueParser"
  [ testCase "always succeed with the given value" $
    parse (valueParser 3) "abc" @?= Result "abc" 3 ]

alternativeTest :: TestTree
alternativeTest =
  testGroup "alternative"
  [ testCase "use first parser if it succeeds" $
    parse (character ||| valueParser 'v') "abc" @?= Result "bc" 'a'
  , testCase "try second parser if first failed" $
    parse (character ||| valueParser 'v') "" @?= Result "" 'v'
  ]

bindParserTest :: TestTree
bindParserTest =
  testGroup "bindParser"
  [ testCase "(=<<) success" $
    parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "abc" @?= Result "bc" 'v'
  , testCase "(=<<) fail" $
    parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "" @?= UnexpectedEof
  ]

pureParserTest :: TestTree
pureParserTest =
  testGroup "pureParser"
  [ testCase "pure" $ parse (pure 'a') "abc" @?= Result "abc" 'a'
  , testCase " " $ parse (pure 'a') "abc" @?= Result "abc" 'a'
  ]

applicativeParserTest :: TestTree
applicativeParserTest =
  testGroup "applicativeParserTest"
  [
    testCase "applicativeParser" $
    parse ((P $ \i -> Result i (1+)) <*> (pure 1)) "abc" @?= Result "abc" 2
  ]

listTest :: TestTree
listTest =
  testGroup "listTest"
  [
    testCase "produce a list of values" $
    parse (list (character)) "abc" @?= Result Nil "abc"
  ]

list1Test :: TestTree
list1Test =
  testGroup "list1Test"
  [ testCase "produce a list of value 1" $
    parse (list1 (character)) "abc" @?= Result Nil "abc"
  , testCase "produce a list of values 2" $
    parse (list1 (character *> valueParser 'v')) "abc" @?= Result Nil "vvv"
  , testCase "fail on empty input" $
    parse (list1 (character *> valueParser 'v')) "" @?= UnexpectedEof
  ]

satisfyTest :: TestTree
satisfyTest =
  testGroup "satisfyTest"
  [ testCase "produce a character when predicate validates" $
    parse (satisfy isUpper) "Abc" @?= Result "bc" 'A'
  , testCase "fail when input is empty" $
    parse (satisfy isUpper) "" @?= UnexpectedEof
  , testCase "fail when predicate does not valide" $
    parse (satisfy isUpper) "abc" @?= UnexpectedChar 'a'
  ]

