module Tests where

import Grammar
import Algo

type Alpha = [Symbol Char Char]

alpha1 :: Alpha
alpha1 = []

beta1 :: Alpha
beta1 = []

test1 = findMatch [] alpha1 beta1

alpha2 :: Alpha
alpha2 = [Right 'b']
beta2 = [Right 'd']

test2a = findMatch [] alpha2 alpha2
test2b = findMatch [] alpha1 alpha2
test2c = findMatch [] alpha2 beta2
test2d = findMatch [] alpha2 beta1

alpha3 :: Alpha
alpha3 = [Left 'B']
beta3 = [Left 'D']

test3a = findMatch [] alpha3 alpha3
test3b = findMatch [] alpha3 beta2
test3c = findMatch [] alpha3 beta3
test3d = findMatch [('D','B')] alpha3 beta3
test3e = findMatch [('D','A')] alpha3 beta3
