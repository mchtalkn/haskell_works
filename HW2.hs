module HW2 (
    parse, -- reexport for easy terminal use
    foldAndPropagateConstants,
    assignCommonSubexprs,
    reducePoly
) where

import Expression
import Parser

-- Do not change the module definition and imports above! Feel free
-- to modify the parser, but please remember that it is just a helper
-- module and is not related to your grade. You should only submit
-- this file. Feel free to import other modules, especially Data.List!

foldAndPropagateConstants :: [(String, ExprV)] -> [(String, ExprV)]
foldAndPropagateConstants a = myFNP (reverse a) []
foldAndPropagateConstants _ = []

assignCommonSubexprs :: ExprV -> ([(String, ExprV)], ExprV)
assignCommonSubexprs _ = ([], notImpl)

reducePoly :: ExprV -> ExprV
reducePoly _ = notImpl

-- an extra dummy variable, so as to not crash the GUI
notImpl :: ExprV
notImpl = Leaf $ Variable "Not Implemented"

replace :: ((String,ExprV),ExprV)->ExprV
replace ((x,val),Leaf  (Variable y)) = if x==y then val else Leaf (Variable y)
replace ((x,val),Leaf y) = Leaf y
replace ((x,val),UnaryOperation Minus a)=replace((x,val), a)
replace ((x,val),BinaryOperation Plus a b)= BinaryOperation Plus (replace((x,val),a)) (replace((x,val),b))
replace ((x,val),BinaryOperation Times a b)= BinaryOperation Times (replace((x,val),a)) (replace((x,val),b))

isle :: ExprV->ExprV
isle (Leaf (Constant a)) =Leaf (Constant a)
isle (UnaryOperation Minus (UnaryOperation Minus (Leaf (Constant a))))=Leaf (Constant a)
isle (BinaryOperation Plus (Leaf (Constant a)) (Leaf (Constant b))) =Leaf (Constant (a+b))
isle (BinaryOperation Times (Leaf (Constant a)) (Leaf (Constant b)))=Leaf (Constant (a*b))
isle a=a

recReplace :: [(String,ExprV)]->ExprV->ExprV
recReplace [] a=a
recReplace (x:y) a=recReplace (y) (replace (x,a))

recIsle :: ExprV->ExprV
recIsle (UnaryOperation Minus a)=isle(UnaryOperation Minus (recIsle a))
recIsle (BinaryOperation Plus a b)=isle(BinaryOperation Plus (recIsle a) (recIsle b))
recIsle (BinaryOperation Times a b)=isle(BinaryOperation Times (recIsle a) (recIsle b))
recIsle a= a

myFNP :: [(String, ExprV)] -> [(String, ExprV)]-> [(String, ExprV)]
myFNP [] b = b
myFNP ((s,e):y) b = myFNP y ((s,(recIsle (recReplace b e))):b)

isIncludes :: Eq a=> [a]->a->Bool
isIncludes [] _ =False
isIncludes (x:y) a= if x==a then True else isIncludes y a

findCommonLeaves ExprV->[(String,ExprV)]->[(String,ExprV)]->[(String,ExprV)]
