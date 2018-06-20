{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ParserTest where

import           Data.Ratio        ((%))
import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.Functor        ((<$>))
import           Course.Monad          ((=<<))
import           Course.Applicative    ((<*>), pure)

import           Course.List       (List (..))
import           Course.Parser     (ParseResult (..), isErrorResult, constantParser,
                                    character, Parser (..), valueParser, (|||),
                                    parse)

test_Parser :: TestTree
test_Parser =
  testGroup "Parser"
  [ isErrorResultTest
  , constantParserTest
  , characterTest
  , parserFunctorTest
  , valueParserTest
  , alternativeTest
  --   jsonStringTest
  -- , jsonNumberTest
  -- , jsonTrueTest
  -- , jsonFalseTest
  -- , jsonNullTest
  -- , jsonArrayTest
  -- , jsonObjectTest
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
  , testCase "fail when input is empty" $ isErrorResult (parse character "") @?= True
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
    isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "") @?= True
  ]

pureParserTest :: TestTree
pureParserTest =
  testGroup "pureParser"
  [ testCase "pure" $ parse (pure 'a') "abc" @?= Result "abc" 'a'
  , testCase " " $ parse (pure 'a') "abc" @?= Result "abc" 'a'
  ]

