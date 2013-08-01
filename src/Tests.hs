module Tests where

import Grammar
import Algo
import Examples

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

-------

prods_o_1 = [Production 'A' alpha1]
prod_n_1 = Production 'N' beta1
prod_n_2 = Production 'N' beta2
prod_n_3 = Production 'N' alpha2

test_p_1a = findMatchProd emptysub prods_o_1 prod_n_1 
test_p_1b = findMatchProd emptysub prods_o_1 prod_n_2

prods_o_2 = [Production 'A' alpha2] 

test_p_2a = findMatchProd emptysub prods_o_2 prod_n_1 
test_p_2b = findMatchProd emptysub prods_o_2 prod_n_2
test_p_2c = findMatchProd emptysub prods_o_2 prod_n_3

--------

CFG _ _ p_ex1left _ = ex1left
CFG _ _ p_ex1right _ = ex1right

test_ex1_1 = findMatchProd emptysub p_ex1left (p_ex1right !! 0)
test_ex1_2 = findMatchProd emptysub p_ex1left (p_ex1right !! 1)
test_ex1_3 = findMatchProd emptysub p_ex1left (p_ex1right !! 2)
test_ex1_4 = findMatchProd emptysub p_ex1left (Production 'G' [Right 'x'])

--------

rex1l = mkRCFG (liftG ex1left)
rex1r = mkRCFG (liftG ex1right)

rex1l_x_p = derivativeProductions rex1l 'x' 
rex1r_x_p = derivativeProductions rex1r 'x' 

rex1l_x = derivative rex1l 'x'
rex1r_x = derivative rex1r 'x'
rex1l_plus = derivative rex1l '+'
rex1r_plus = derivative rex1r '+'

rex1l_x_x = derivative rex1l_x 'x'
rex1r_x_x = derivative rex1r_x 'x'
rex1l_x_plus = derivative rex1l_x '+'
rex1r_x_plus = derivative rex1r_x '+'

--------

test_contained_1 = isContained ex1left ex1right
test_contained_2 = isContained ex1right ex1left

test_contained_3 = isContained ex3 ex4
test_contained_4 = isContained ex4 ex3
