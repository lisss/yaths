module Print3 where

hello :: String
hello = "hello"

myGr :: String
myGr = "hello," ++ " dude"

world :: String
world = "Dude"

main :: IO ()
main = do
  putStrLn myGr
  putStrLn secondGr
  where secondGr = concat [hello, " ", world]

area d = pi * (r * r) where
  r = d / 2
