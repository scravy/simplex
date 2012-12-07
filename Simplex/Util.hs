module Simplex.Util where


when True x = x
when False _ = []

ifElse True x _ = x
ifElse _    _ y = y

tail' [] = []
tail' xs = tail xs

tail'' [] = []
tail'' xs = tail' $ tail' xs

skipOneSpace (' ':xs) = xs
skipOneSpace s = s

