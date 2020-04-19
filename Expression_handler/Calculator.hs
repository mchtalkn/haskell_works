module Mini1 (
    gridMap,
    gridMapIf,
    evalExpr,
    getVars,
    evalDeriv,
    parse -- reexported to allow use
    ) where

import Expression
import Parser
import Data.List

-- Do not modify the module declaration and imports above!
-- Also do not change the function signatures and do not
-- remove the dummy implementations of the functions if
-- you want your code to compile.

-- Feel free to import anything else here

-- gridMap (20 points), map function over grid elements
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap a b = [map a x|x<-b]
gridMap _ _ = undefined

-- gridMapIf (20 points), map functions over grid elements
-- that satisfy the predicate provided as the first arg.
gridMapIf :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
gridMapIf c a b = [[if(c y) then (a y) else y|y<-x]|x<-b]
gridMapIf _ _ _ = undefined

-- evalExpr (20 points), evaluate the expression by
-- substituting (var, value) pairs given in the first arg.
evalExpr :: [(String, Int)] -> ExprV -> Int
evalExpr _ (Leaf (Constant a))=a
evalExpr l (Leaf (Variable a))=  case (find (\x ->(fst x) ==a ) l ) of
                                  Just n ->snd n
                                  Nothing ->0
evalExpr l (UnaryOperation Minus a)= (-1)*(evalExpr l a)
evalExpr l (BinaryOperation Plus a b)=(evalExpr l a)+(evalExpr l b)
evalExpr l (BinaryOperation Times a b)=(evalExpr l a)*(evalExpr l b)
evalExpr _ _ = undefined


-- getVars (20 points), return the variables contained
-- in the expression in a list (ordered, no duplicates)

getVars :: ExprV -> [String]
getVars ( Leaf (Constant a))= []
getVars (Leaf (Variable a))=[a]
getVars (UnaryOperation _ a)=getVars a
getVars (BinaryOperation Plus a b)= clear_vars((getVars a)++(getVars b)) []
getVars (BinaryOperation Times a b)= clear_vars((getVars a)++(getVars b)) []
getVars _ = undefined

clear_vars :: [String]->[String]->[String]
clear_vars [] l=sort l
clear_vars (x:y) l=if (elem (x) l) then clear_vars y l else (clear_vars y (x:l))

-- evalDeriv (20 points), evaluate the first derivative
-- with respect to the variable given in the second
-- arg. using (var, value) pairs given in the first arg.
evalDeriv :: [(String, Int)] -> String -> ExprV -> Int
evalDeriv a b c=evalExpr a (derivHelper b c)
evalDeriv _ _ _ = undefined

derivHelper :: String->ExprV->ExprV
derivHelper v (Leaf (Constant _))=Leaf (Constant 0)
derivHelper v (Leaf (Variable a))= if(a==v) then Leaf (Constant 1) else Leaf (Constant 0)
derivHelper v (UnaryOperation Minus a)=UnaryOperation Minus (derivHelper v a)
derivHelper v (BinaryOperation Plus a b)=BinaryOperation Plus (derivHelper v a) (derivHelper v b)
derivHelper v (BinaryOperation Times a b)=BinaryOperation Plus (BinaryOperation Times (derivHelper v a) b) (BinaryOperation Times  a (derivHelper v b))
-- Looks like that's all!
