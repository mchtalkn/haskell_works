module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where

-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]]
form list (0,y) = []
form list (y,x) = (first list x:form (mdrop list x) (y-1,x))

constGrid :: a -> (Int, Int) -> [[a]]
constGrid _ (0,_) = []
constGrid a (m,n)=(createOneGrid a n:constGrid a (m-1, n))

flatten :: [[a]] -> [a]
flatten list = [y |x<-list , y<-x]

access :: [[a]] -> (Int, Int) -> a
access a (m,n) = (a!!m)!!n
----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice list a b = [el | subList<-getSubList list a,let el=getSubList subList b]

vcat :: [[a]] -> [[a]] -> [[a]]
vcat a b = a++b

hcat :: [[a]] -> [[a]] -> [[a]]
hcat _ [] = []
hcat[] _ = []
hcat (x:y) (m:n)=((x++m):hcat y n)

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without list (x,y) (m,n) = [el | subList<-except list (x,y-x),let el=except subList (m,n-m)]
--without _ _ _=[]
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d g pattern = windower g g pattern 0 0 (pickWeight pattern)
--matches2d grid pattern = helper2d grid pattern (pickHeight grid) (pickWeight grid) (pickHeight pattern) (pickWeight pattern)
--matches2d _ _ = undefined
--[res | row<-[0..(((pickHeight grid)-(pickHeight pattern))-1)],column<-[0..(((pickWeight grid)-(pickWeight pattern))-1)], let res=(slice grid (row,row+pickHeight pattern) (column,column+ pickWeight pattern))]
----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.
first :: [a]->Int->[a]
first [] _ = []
first _ 0 = []
first (x:y) z = if (z<0) then [] else (x:first y (z-1))
mdrop :: [a]->Int->[a]
mdrop [] _ =[]
mdrop list 0= list
mdrop (x:y) z = mdrop y (z-1)

createOneGrid:: a->Int->[a]
createOneGrid _ 0=[]
createOneGrid a m=(a:createOneGrid a (m-1))
except :: [a]->(Int,Int)->[a]
except [] _ = []
except list  (0,0)= list
except (x:y) (0,b)= except y (0,(b-1))
except (x:y) (a,b)= (x:except y ((a-1),b))
getSubList :: [a]->(Int,Int)->[a]
getSubList list (x,y)=first (mdrop list x) (y-x)
getFirstElement :: [a]->a
getFirstElement []= undefined
getFirstElement (x:y)= x
pickHeight :: [[a]]-> Int
pickHeight list= length list
pickWeight ::  [[a]]-> Int
pickWeight (x:y)=length x
helper2d :: Eq a => [[a]]->[[a]]->Int->Int->Int->Int->[(Int,Int)]
helper2d grid pattern h1 w1 h2 w2 = [(row,column) | row<-[0..(h1-h2)],column<-[0..(w1 -w2)],  (slice grid (row,row+h2) (column,column + w2))==pattern ]
gridEquality :: Eq a => [[a]]->[[a]]->Bool
gridEquality [] [] =True
gridEquality _ [] = False
gridEquality [] _ = False
gridEquality (x:y) (m:n) = if(listEquality x m) then gridEquality y n else False
listEquality ::Eq a=> [a]->[a]->Bool
listEquality [] [] = True
listEquality _ []=False
listEquality [] _=False
listEquality (x:y) (m:n)= if(x==m) then  listEquality y n else False
test2d ::  [[Int]]->[[Int]]->Int->Int->Int->Int->[Int]
test2d grid pattern h1 w1 h2 w2 = [res | row<-[0..(h1-h2)],column<-[0..(w1 -w2)], let res =1 ]
mtest2d ::  [[Int]] -> [[Int]] -> [Int]
mtest2d grid pattern =test2d grid pattern (pickHeight grid) (pickWeight grid) (pickHeight pattern) (pickWeight pattern)
--g = form [1..1000000] (1000, 10000) :: [[Int]]
--windower g g pattern  0 0 2
--pattern = [[300, 301], [10300, 10301]] ::[[Int]]


--a=matches2d g [[300, 301], [10300, 10301]]
dropFirsts :: Int->[[a]]->[[a]]
dropFirsts 0 _ = []
dropFirsts _ [] = []
dropFirsts m (x:y) = ((drop 1 x):(dropFirsts (m-1) y))
main= print "a"
equalFL ::Eq a=> [a]->[a]->Bool
equalFL [] _ =True
equalFL _ [] =False
equalFL (x:y) (z:t) = if (x==z) then equalFL y t else False
equalFW :: Eq a=> [[a]]->[[a]]->Bool
equalFW [] _ =True
equalFW _ [] =False
equalFW (x:y) (z:t) = if(equalFL x z) then equalFW y t else False
windower :: Eq a=>[[a]]-> [[a]]-> [[a]]-> Int->Int->Int->[(Int,Int)]
windower [] _ _ _ _ _  = []
windower (_:ols) ([]:ls) pattern  x y m = windower ols ols pattern 0 (y+1) m
windower oL tmpL pattern x y m= if (equalFW pattern tmpL) then ((y,x):windower oL (dropFirsts m tmpL) pattern (x+1) y m) else windower oL (dropFirsts m tmpL) pattern (x+1) y m
