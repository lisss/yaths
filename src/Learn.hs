module Learn where

fn1 x = x * 3 + y where
    y = 1000

fn2 x y = 10 * 5 + y where
  y = x * 5

fn3 x = z / x + y where
  y = negate x
  z = y * 10

z = 7
waxOn = x * 5 where
  x = y ^ 2
  y = z + 8

tripple x = x * 3

waxOff = tripple
