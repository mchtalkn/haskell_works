import Data.Char
import Data.List.Split
import Data.List
import HW2
--contents =deMicrosoftifyString getContents
--mlines = splitOn "\n" contents
main = do
    contents <- getContents
    mapM  putStr $filter (/= "True") $ test $ splitOn "\n" $deMicrosoftifyString contents
deMicrosoftifyString = filter (/= '\r')
test :: [String]->[String]
test [""]=[]
test []=[]
test (inp:y)=[(test2 $ splitOn "|" inp)]++(test y)

test2 :: [String]->String
test2 ("test":a)="test\n"
test2 ("assignCommonSubexprs":(inp:(expected:y)))=let a=show $ assignCommonSubexprs $ parse inp in if(a==expected) then "True" else ("Error in assignCommonSubexprs \ninp:"++inp++"\nexpected:"++expected++"\ngiven:"++a++"\n")
test2 ("reducePoly":(inp:(expected:y)))=let a=show $ reducePoly $ parse inp in if(a==expected) then "True" else ("Error in reducePoly \ninp:"++inp++"\nexpected:"++expected++"\ngiven:"++a++"\n")

test2 _ = []
