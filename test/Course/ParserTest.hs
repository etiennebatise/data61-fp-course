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
  [ testCase "True when UnexpectedEof" $
    isErrorResult (UnexpectedEof) @?= True
  , testCase "True when ExpectedEof" $
    isErrorResult (ExpectedEof "a") @?= True
  , testCase "True when UnexpectedChar" $
    isErrorResult (UnexpectedChar 'a') @?= True
  , testCase "True when UnexpectedString" $
    isErrorResult (UnexpectedString "abc") @?= True
  , testCase "False when Result" $
    isErrorResult (Result "abc" "a") @?= False
  ]

constantParserTest :: TestTree
constantParserTest =
  testGroup "constantParser"
  [ testCase "always return given result" $
    parse (constantParser $ Result "abc" 'z') "Lorem" @?= Result "abc" 'z'
  ]

characterTest :: TestTree
characterTest =
  testGroup "character"
  [ testCase "read character in input" $
    parse character "abc" @?= Result "bc" 'a'
  , testCase "fail when input is empty" $
    parse character "" @?= UnexpectedEof
  ]

parserFunctorTest :: TestTree
parserFunctorTest =
  testGroup "parserFunctor"
  [ testCase "parser can map"
    $ parse (toUpper <$> character) "amz" @?= Result "mz" 'A'
  ]

valueParserTest :: TestTree
valueParserTest =
  testGroup "valueParser"
  [ testCase "always succeed with the given value" $
    parse (valueParser 3) "abc" @?= Result "abc" 3
  ]

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
  [ testCase "applicativeParser" $
    parse ((P $ \i -> Result i (1+)) <*> (pure 1)) "abc" @?= Result "abc" 2
  ]

listTest :: TestTree
listTest =
  testGroup "listTest"
  [ testCase "produce a list of values" $
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

isTest :: TestTree
isTest =
  testGroup "isTest"
  [ testCase "return a parser that produce the given character" $
    parse (is 'a') "abc" @?= Result "bc" 'a'
  , testCase "return a parser that fails if input is empty" $
    parse (is 'a') "" @?= UnexpectedEof
  , testCase "return a parser that fails if the characters aren't equal" $
    parse (is 'b') "abc" @?= UnexpectedChar 'a'
  ]

digitTest :: TestTree
digitTest =
  testGroup "digitTest"
  [ testCase "return a parser that produce a character between 0 and 9" $
    parse (digit) "1abc" @?= Result "abc" '1'
  , testCase "return a parser that fails if input is empty" $
    parse (digit) "" @?= UnexpectedEof
  , testCase "return a parser that fails if the character is not a digit" $
    parse (digit) "a" @?= UnexpectedChar 'a'
  ]

spaceTest :: TestTree
spaceTest =
  testGroup "spaceTest"
  [ testCase "return a parser that produce a space character" $
    parse (space) " abc" @?= Result "abc" ' '
  , testCase "return a parser that fails if input is empty" $
    parse (space) "" @?= UnexpectedEof
  , testCase "return a parser that fails if the character is not a space" $
    parse (space) "a" @?= UnexpectedChar 'a'
  ]

spaces1Test :: TestTree
spaces1Test =
  testGroup "spaces1Test"
  [ testCase "return a parser that produces one or more spaces" $
    parse (spaces1) "     abc" @?= Result "abc" "     "
  , testCase "return a parser that fails if input is empty" $
    parse (spaces1) "" @?= UnexpectedEof
  , testCase "return a parser that fails if the first character is not a space" $
    parse (space) "a" @?= UnexpectedChar 'a'
  ]

lowerTest :: TestTree
lowerTest =
  testGroup "lowerTest"
  [ testCase "return a parser that produce a lower-case character" $
    parse (lower) "abc" @?= Result "bc" 'a'
  , testCase "return a parser that fails if input is empty" $
    parse (lower) "" @?= UnexpectedEof
  , testCase "return a parser that fails if the character is not lower-case" $
    parse (lower) "A" @?= UnexpectedChar 'A'
  ]

upperTest :: TestTree
upperTest =
  testGroup "upperTest"
  [ testCase "return a parser that produce a upper-case character" $
    parse (upper) "Abc" @?= Result "bc" 'A'
  , testCase "return a parser that fails if input is empty" $
    parse (upper) "" @?= UnexpectedEof
  , testCase "return a parser that fails if the character is not upper-case" $
    parse (upper) "a" @?= UnexpectedChar 'a'
  ]

alphaTest :: TestTree
alphaTest =
  testGroup "alphaTest"
  [
    -- testCase "return a parser that produce a alpha character" $
    -- parse (alpha) "alpha" @?= Result "bc" 'A'
    testCase "return a parser that fails if input is empty" $
    parse (alpha) "" @?= UnexpectedEof
  , testCase "return a parser that fails if the character is not alpha" $
    parse (alpha) "a" @?= UnexpectedChar 'a'
  ]

sequenceParserTest :: TestTree
sequenceParserTest =
  testGroup "sequenceParserTest"
  [ testCase "return a parser that sequences the given list of parsers by producing all their results" $
    parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" @?= Result "def" "axC"
  , testCase "return a parser that fails on the first failing parser" $
    parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef" @?= UnexpectedChar 'b']

thisManyTest :: TestTree
thisManyTest =
  testGroup "thisManyTest"
  [ testCase "return a parser that produces the given number of values" $
    parse (thisMany 4 upper) "ABCDef" @?= Result "ef" "ABCD"
  , testCase "return a parser that fails if it can't produce the given number of values" $
    parse (thisMany 4 upper) "ABcDef" @?= UnexpectedChar 'c'
  ]

ageParserTest :: TestTree
ageParserTest =
  testGroup "ageParserTest"
  [ testCase "return a parser that produce an age" $
    parse ageParser "120" @?= Result "" 120
  , testCase "return a parser that fails when no age can be produced 1" $
    parse ageParser "abc" @?= UnexpectedChar 'a'
  , (testCase) "return a parser that fails when no age can be produced 2" $
    parse ageParser "-120" @?= UnexpectedChar '-'
  ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup "firstNameParserTest"
  [ testCase "return a parser for Person.firstName" $
    parse firstNameParser "Abc" @?= Result "" "Abc"
  , testCase "return a parser that fails if not a first name" $
    parse firstNameParser "abc" @?= UnexpectedChar 'a'
  ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup "surnameParserTest"
  [ testCase "return a parser for Person.surname" $
    parse surnameParser "Abcdefg" @?= Result "" "Abcdefg"
  , testCase "return a parser that fails if not a surname 1" $
    parse surnameParser "Abc" @?= UnexpectedEof
  , testCase "return a parser that fails if not a surname 2" $
    parse surnameParser "abc" @?= UnexpectedChar 'a'
  ]

smokerParserTest :: TestTree
smokerParserTest =
  testGroup "smokerParserTest"
  [ testCase "return a parser for Person.smoker 1" $
    parse smokerParser "yabc" @?= Result "abc" True
  , testCase "return a parser for Person.smoker 2" $
    parse smokerParser "nabc" @?= Result "abc" False
  , testCase "return a parser that fails if not 'y' or 'n'" $
    parse smokerParser "abc" @?= UnexpectedChar 'a'
  ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup "phoneGroupParserTest"
  [ testCase "return parser for Person.phoneBody 1" $
    parse phoneBodyParser "123-456" @?= Result "" "123-456"
  , testCase "return parser for Person.phoneBody 2" $
    parse phoneBodyParser "123-4a56" @?= Result "a56" "123-4"
  , testCase "return parser for Person.phoneBody 3" $
    parse phoneBodyParser "a123-456" @?= Result "a123-456" ""
  ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup "phoneParserTest"
  [ testCase "return parser for Person.phoneBody 1" $
    parse phoneParser "123-456#" @?= Result "" "123-456"
  , testCase "return parser for Person.phoneBody 2" $
    parse phoneParser "123-456#abc" @?= Result "abc" "123-456"
  , testCase "return parser for Person.phoneBody 3" $
    parse phoneParser "123-456" @?= UnexpectedEof
  , testCase "return parser for Person.phoneBody 4" $
    parse phoneParser "a123-456" @?= UnexpectedChar 'a'
  ]

personParserTest :: TestTree
personParserTest =
  testGroup "personParserTest"
  [
    testCase "return a parser for Person 1" $
    parse personParser "" @?= UnexpectedEof
  , testCase "return a parser for Person 2" $
    parse personParser "12x Fred Clarkson y 123-456.789#" @?= UnexpectedChar 'a'
  , testCase "return a parser for Person 3" $
    parse personParser "123 fred Clarkson y 123-456.789#" @?= UnexpectedChar 'f'
  , testCase "return a parser for Person 4" $
    parse personParser "123 Fred Cla y 123-456.789#" @?= UnexpectedChar ' '
  , testCase "return a parser for Person 5" $
    parse personParser "123 Fred clarkson y 123-456.789#" @?= UnexpectedChar 'c'
  , testCase "return a parser for Person 6" $
    parse personParser "123 Fred Clarkson x 123-456.789#" @?= UnexpectedChar 'x'
  , testCase "return a parser for Person 6" $
    parse personParser "123 Fred Clarkson y 1x3-456.789#" @?= UnexpectedChar 'x'
  , testCase "return a parser for Person 7" $
    parse personParser "123 Fred Clarkson y -123-456.789#" @?= UnexpectedChar '-'
  , testCase "return a parser for Person 8" $
    parse personParser "123 Fred Clarkson y 123-456.789" @?= UnexpectedEof
  , testCase "return a parser for Person 9" $
    parse personParser "123 Fred Clarkson y 123-456.789# rest" @?= Result " rest" (Person 123 "Fred" "Clarkson" True "123-456.789")
  , testCase "return a parser for Person 9" $
    parse personParser "123  Fred   Clarkson    y     123-456.789#" @?= Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
  ]
