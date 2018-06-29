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

import           Course.MoreParser  (between, betweenCharTok, charTok, commaTok,
                                     digits1, hex, hexu, noneof, oneof, option,
                                     quote, spaces, string, stringTok, tok,
                                     (<.>))

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
  , oneofTest
  , noneofTest
  , betweenTest
  , betweenCharTokTest
  , hexTest
  , hexuTest
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

oneofTest :: TestTree
oneofTest =
  testGroup "oneof"
  [ testCase "parses one of the characters in the given string" $
    parse (oneof "abc") "bcdef" @?= Result "cdef" 'b'
  , testCase "fail if none of characters is in the given string" $
    parse (oneof "abc") "def" @?= UnexpectedChar 'd'
  ]

noneofTest :: TestTree
noneofTest =
  testGroup "noneof"
  [ testCase "parses any characters not in the given string" $
    parse (noneof "bcd") "abc" @?= Result "bc" 'a'
  , testCase "fail if one characters is in the given string" $
    parse (noneof "abcd") "abc" @?= UnexpectedChar 'a'
  ]

betweenTest :: TestTree
betweenTest =
  testGroup "between"
  [ testCase "run the first parser, then third keeping the result, then second" $
    parse (between (is '[') (is ']') character) "[a]" @?= Result "" 'a'
  , testCase "fail if first parser fails" $
    parse (between (is '[') (is ']') character) "a]" @?= UnexpectedChar 'a'
  , testCase "fail if second parser fails" $
    parse (between (is '[') (is ']') character) "[abc]" @?= UnexpectedChar 'b'
  , testCase "fail if third parser fails" $
    parse (between (is '[') (is ']') character) "[a" @?= UnexpectedEof
  ]

betweenCharTokTest :: TestTree
betweenCharTokTest =
  testGroup "betweenCharTok"
  [
    testCase "run the parser between two delimiters" $
    parse (betweenCharTok '[' ']' character) "[a]" @?= Result "" 'a'
  , testCase "fail if the parser fails" $
    parse (betweenCharTok '[' ']' character) "[abc]" @?= UnexpectedChar 'b'
   , testCase "fail if the left delimiter is not found" $
   parse (betweenCharTok '[' ']' character) "a]" @?= UnexpectedChar 'a'
   , testCase "fail if the right delimiter is not found" $
   parse (betweenCharTok '[' ']' character) "[a" @?= UnexpectedEof
  ]

hexTest :: TestTree
hexTest =
  testGroup "hex"
  [
    testCase "parse de character 4 hex digits and return de value 1" $
    parse hex "0010" @?= Result "" '\DLE'
  , testCase "parse de character 4 hex digits and return de value 2" $
    parse hex "0a1f" @?= Result "" '\2591'
  , testCase "fail if not a hex number 1" $
    parse hex "001" @?= UnexpectedEof
  , testCase "fail if not a hex number 2" $
    parse hex "0axf" @?= UnexpectedChar 'x'
  ]

hexuTest :: TestTree
hexuTest =
  testGroup "hexu"
  [ testCase "parse the character 'u' followed by 4 hex digits 1" $
    parse hexu "u0010" @?= Result "" '\DLE'
  , testCase "parse the character 'u' followed by 4 hex digits 2" $
    parse hexu "u0a1f" @?= Result " " '\2591'
  , testCase "fail if not starting with 'u'" $
    parse hexu "0010" @?= UnexpectedChar '0'
  , testCase "fail if not a hex number 1" $
    parse hexu "u001" @?= UnexpectedEof
  , testCase "fail if not a hex number 2" $
    parse hexu "u0axf" @?= UnexpectedChar 'x'
  ]
