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
import           Course.Parser      (ParseResult (..), Parser (..), ageParser,
                                     alpha, character, constantParser, digit,
                                     firstNameParser, is, isErrorResult, list,
                                     list1, lower, parse, personParser,
                                     phoneBodyParser, phoneParser, satisfy,
                                     sequenceParser, smokerParser, space,
                                     spaces1, surnameParser, thisMany, upper,
                                     valueParser, (|||))
import           Course.Person      (Person (..))

import           Course.MoreParser  (charTok, commaTok, quote, spaces, string,
                                     tok, (<.>), stringTok, option, digits1)

test_MoreParser :: TestTree
test_MoreParser =
  testGroup "MoreParser"
  [ spacesTest
  , tokTest
  , charTokTest
  , commaTokTest
  , quoteTest
  , stringTest
  , stringTokTest
  , optionTest
  , digits1Test
  ]

spacesTest :: TestTree
spacesTest =
  testGroup "spaces"
  [ testCase "consume 0 spaces" $
    parse spaces "abc" @?= Result "abc" ""
  , testCase "consume more spaces" $
    parse spaces "  abc" @?= Result "abc" "  "
  ]

tokTest :: TestTree
tokTest =
  testGroup "tok"
  [ testCase "run the parser and consume spaces" $
    parse (tok (is 'a')) "a bc" @?= Result "bc" 'a'
  ]

charTokTest :: TestTree
charTokTest =
  testGroup "charTok"
  [ testCase "parse a char and consume spaces" $
    parse (charTok 'a') "a bc" @?= Result "bc" 'a',
    testCase "fail when the char is not the given on" $
    parse (charTok 'a') "dabc" @?= UnexpectedChar 'd'
  ]


commaTokTest :: TestTree
commaTokTest =
  testGroup "commaTok"
  [ testCase  "parse a comma and consume spaces" $
    parse commaTok ",123" @?= Result "123" ','
  , testCase  "fail when not a ','" $
    parse commaTok "1,23" @?= UnexpectedChar '1'
  ]

quoteTest :: TestTree
quoteTest =
  testGroup "quote"
  [ testCase "parse single quote" $
    parse quote "'abc" @?= Result "abc" '\''
  , testCase "parse double quotes" $
    parse quote "\"abc" @?= Result "abc" '\"'
  , testCase "fail when not a single/double quote" $
    parse quote "abc" @?= UnexpectedChar 'a'
  ]

stringTest :: TestTree
stringTest =
  testGroup "string"
  [ testCase "parse the given string" $
    parse (string "") "" @?= Result "" ""
  , testCase "parse the given string" $
    parse (string "a") "a" @?= Result "" "a"
  ,  testCase "parse the given string" $
    parse (string "abc") "abc" @?= Result "" "abc"
  , testCase "fail otherwise" $
    parse (string "abc") "bcdef" @?= UnexpectedChar 'b'
  ]

stringTokTest :: TestTree
stringTokTest =
  testGroup "stringTok"
  [ testCase "parse the given string and consume spaces" $
    parse (stringTok "abc") "abc  " @?= Result "" "abc"
  , testCase "fail otherwise" $
    parse (stringTok "abc") "bc  " @?= UnexpectedChar 'b'
  ]

optionTest :: TestTree
optionTest =
  testGroup "option"
  [ testCase "run the given parser" $
    parse (option 'x' character) "abc" @?= Result "bc" 'a'
  , testCase "produce the given value if parser fails" $
    parse (option 'x' character) "" @?= Result "" 'x'
  ]

digits1Test :: TestTree
digits1Test =
  testGroup "digits1"
  [ testCase "parse 1 or more digits" $
    parse digits1 "123" @?= Result "" "123"
  , testCase "fail if no digits" $
    parse digits1 "abc123" @?= UnexpectedChar 'a'
  ]

