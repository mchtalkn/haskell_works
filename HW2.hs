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
assignCommonSubexprs a= assignHelper [] a 0
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

findCommons :: Eq a=> [a]->[a]->[a]
findCommons []  b = b
findCommons (x:y) b = if( isIncludes y x) then findCommons (filter (/=x) y) (x:b)  else findCommons y b

isLeaf :: ExprV->Bool
isLeaf (Leaf ( Constant _))=True
isLeaf (Leaf (Variable _))=True
isLeaf _ = False

findBigLeaves :: ExprV->[ExprV]->[ExprV]
findBigLeaves (UnaryOperation Minus a) l =if (isLeaf a) then ((UnaryOperation Minus a):l) else findBigLeaves a l
findBigLeaves (BinaryOperation Plus a b) l =if ((isLeaf a)&& (isLeaf b)) then ((BinaryOperation Plus a b):l) else ((findBigLeaves a l)++(findBigLeaves b l))++l
findBigLeaves (BinaryOperation Times a b) l =if ((isLeaf a)&& (isLeaf b)) then ((BinaryOperation Times a b):l) else ((findBigLeaves a l)++(findBigLeaves b l))++l
findBigLeaves _ l=l

namer :: [ExprV]->Int->Int->[(String,ExprV)]
namer [] _ _ = []
namer (x:l) m n= (((("&"++(show m))++(show n)),x) : (namer l m (n+1)))

assignHelper :: [(String,ExprV)]-> ExprV -> Int-> ([(String, ExprV)], ExprV)
assignHelper l a m =if ((length (findCommons (findBigLeaves a []) []))==0) then (l,a) else let k=(namer (findCommons (findBigLeaves a []) []) m 0) in assignHelper (l++k) (recAssign k a) (m+1)

assign ::(String,ExprV)->ExprV->ExprV
assign (s,v) (UnaryOperation Minus a)= if((UnaryOperation Minus a)==v) then Leaf(Variable s) else UnaryOperation Minus (assign (s,v) a)
assign (s,v) (BinaryOperation Plus a b)=if((BinaryOperation Plus a b)==v) then Leaf (Variable s) else BinaryOperation Plus (assign (s,v) a) (assign (s,v) b)
assign (s,v) (BinaryOperation Times a b)=if((BinaryOperation Times a b)==v) then Leaf (Variable s) else BinaryOperation Times (assign (s,v) a) (assign (s,v) b)
assign _ a=a

recAssign:: [(String,ExprV)]->ExprV->ExprV
recAssign [] a=a
recAssign (x:y) a=recAssign y (assign x a)
