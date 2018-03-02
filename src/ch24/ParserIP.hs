-- 6. Write a parser for IPv4 addresses
module ParserIP where

import Text.Trifecta
import Data.Word
import Control.Applicative
import ParserUtils

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

base = 256

parseOctet :: Parser Int
parseOctet = try parseThree <|> try parseTwo <|> fromInteger <$> integer

parseIp :: Parser Word32
parseIp = do
  one <- parseOctet
  char '.'
  two <- parseOctet
  char '.'
  three <- parseOctet
  char '.'
  four <- parseOctet
  return $ fromIntegral $ one * base ^ 3 + two * base ^ 2 + three * base + four

getIpV4 :: Parser IPAddress
getIpV4 = do
  parsed <- parseIp
  return $ IPAddress parsed
