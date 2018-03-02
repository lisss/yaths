module ParserMain where

import Text.Trifecta
import Control.Applicative
import ParserExamples
import ParserPhone
import ParserSemVer
import ParserInteger
import ParserIP

pNL s =
  putStrLn ('\n' : s)

-- TODO: write spec tests?
main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneEOF':"
  oneEOF
  pNL "oneTwoEOF':"
  oneTwoEOF
  pNL "eitherOfThree - 1:"
  print $ parseString eitherOfThree mempty "1"
  pNL "eitherOfThree - 12:"
  print $ parseString eitherOfThree mempty "12"
  pNL "eitherOfThree - 123:"
  print $ parseString eitherOfThree mempty "123"
  pNL "eitherOfThree - 1234 (err):"
  print $ parseString eitherOfThree mempty "1234"
  pNL "eitherOfThree - 666 (err):"
  print $ parseString eitherOfThree mempty "666"
  pNL "eitherOfThreeStop:"
  print $ parseString eitherOfThreeStop mempty "12"
  pNL "eitherOfThreeChar: a"
  print $ parseString eitherOfThreeChar mempty "a"
  pNL "eitherOfThreeChar: b"
  print $ parseString eitherOfThreeChar mempty "b"
  pNL "eitherOfThreeChar: c"
  print $ parseString eitherOfThreeChar mempty "c"
  pNL "eitherOfThreeChar: d (err)"
  print $ parseString eitherOfThreeChar mempty "d"
  pNL "eitherOfThreeChar: ab (err)"
  print $ parseString eitherOfThreeChar mempty "ab"
  pNL "parseInt: 123"
  print $ parseString parseInt mempty "123"
  pNL "parseInt: 12"
  print $ parseString parseInt mempty "12"
  pNL "parseInt: 123ab (err)"
  print $ parseString parseInt mempty "123ab"
  pNL "Example: Alternative"
  let p f i = parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
  pNL "EitherOr (err - is it expected by authors to fail?)"
  print $ p parseNos eitherOr
  pNL "Example: Fraction parser"
  let parseRatio' = parseString parseRatio mempty
  print $ parseRatio' badFraction
  print $ parseRatio' alsoBad
  print $ parseRatio' shouldWork
  print $ parseRatio' shouldAlsoWork
  pNL ">>> Parse decimal or fractional"
  pNL "Parse decimal: 1.9"
  print $ parseString parseDecOrFract mempty "1.9"
  pNL "Parse decimal: 43.00479"
  print $ parseString parseDecOrFract mempty "43.00479a"
  pNL "Parse ratio: shouldWork"
  print $ parseString parseDecOrFract mempty shouldWork
  pNL "Parse ratio: bad"
  print $ parseString parseDecOrFract mempty badFraction
  pNL ">>> Chapter Exercises"
  pNL ">>>>> SemVer"
  pNL "SemVer: just a ver"
  print $ parseString parseSemVer mempty "2.1.1"
  pNL "SemVer: ver with release"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  pNL "SemVer: ver with release and meta"
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92+some.666.meta"
  pNL "SemVer: ver with meta"
  print $ parseString parseSemVer mempty "1.0.0+some.666.meta"
  pNL "SemVer: bad ver (err)"
  print $ parseString parseSemVer mempty "1.0."
  pNL "SemVer: bad ver - 2 (err)"
  print $ parseString parseSemVer mempty "1.0.xxx"
  pNL "SemVer: bad rel (err)"
  print $ parseString parseSemVer mempty "1.0.3-3."
  pNL "SemVer: bad meta (err)"
  print $ parseString parseSemVer mempty "1.0.3+3."
  pNL ">>>>> Integers"
  pNL "Parse digit"
  print $ parseString parseDigit mempty "123"
  pNL "Parse digit (err)"
  print $ parseString parseDigit mempty "abc"
  pNL "Parse positive integer"
  print $ parseString base10Integer mempty "123abc"
  pNL "Parse positive integer (err)"
  print $ parseString base10Integer mempty "abc"
  pNL "Parse negative integer"
  print $ parseString base10Integer' mempty "-123abc"
  pNL ">>>>> Phone"
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
  pNL ">>>>> IPV4"
  print $ parseString getIpV4 mempty "172.16.254.1"
  print $ parseString getIpV4 mempty "204.120.0.15"
