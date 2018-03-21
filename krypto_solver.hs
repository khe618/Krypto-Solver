import Data.List
import Numeric
import Data.Char

operators_list = operator_combinations
    where   operator_combinations = map convertDigits base4'
            convertDigits = map ( (operators !!) . digitToInt)
            operators = ["+", "-", "*", "/"]
            base4' = [concat (take (4 - (length x) ) (repeat "0") ) ++ x | x <- base4 ]
            base4 = [showIntAtBase 4 intToDigit x "" | x <- [0..255] ]

merge_operators (n1:n2:n3:n4:n5:_) (op1:op2:op3:op4:_) = [l1, l2, l3, l4, l5, l6, l7]
    where   l1 = n1:n2:n3:n4:n5:op1:op2:op3:op4:[]
            l2 = n1:n2:n3:n4:op1:n5:op2:op3:op4:[]
            l3 = n1:n2:n3:n4:op1:op2:n5:op3:op4:[]
            l4 = n1:n2:n3:n4:op1:op2:op3:n5:op4:[]
            l5 = n1:n2:n3:op1:n4:op2:op3:n5:op4:[]
            l6 = n1:n2:n3:op1:op2:n4:op3:n5:op4:[]
            l7 = n1:n2:op1:n3:op2:n4:op3:n5:op4:[]

solveRPN expression = head (foldl foldingFunction [] expression) 
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = if x == 0 then (0/0):ys else (y / x):ys
            foldingFunction xs numberString = read numberString:xs

solve_krypto nums target = if length solutions > 0 then rpn_to_infix $ snd $ head solutions else "No solutions"
    where   solutions = dropWhile (\(val,exp) -> val /= target) rpn_solve
            rpn_solve = [(solveRPN x, x) | x <- expression_list]
            expression_list = concat [merge_operators x y | x <- permute, y <- o_list]
            o_list = operators_list
            permute = permutations strings
            strings = map show nums 

insertAt index element xs = as ++ (element:bs)
                  where (as,bs) = splitAt index xs

rpn_to_infix arr = tail $ tail $ init $ init $ head (foldl foldingFunction [] arr)
    where   foldingFunction (x:y:ys) "*" = ("( " ++ y ++ " * " ++ x ++ " )" ):ys
            foldingFunction (x:y:ys) "+" = ("( " ++ y ++ " + " ++ x ++ " )" ):ys
            foldingFunction (x:y:ys) "-" = ("( " ++ y ++ " - " ++ x ++ " )" ):ys
            foldingFunction (x:y:ys) "/" = ("( " ++ y ++ " / " ++ x ++ " )" ):ys
            foldingFunction xs numberString = numberString:xs
