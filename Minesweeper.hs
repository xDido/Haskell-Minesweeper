type Cell=(Int,Int)
data Mystate= Null | S Cell [Cell] String Mystate deriving (Show,Eq)

up::Mystate -> Mystate
up(S (x,y) xs s m)=
 if x > 0 then S ((x-1),y) xs "up" (S (x,y) xs s m)
 else Null 
 
down:: Mystate-> Mystate
down(S (x,y) xs s m)=
 if x < 3 then S((x+1),y) xs "down" (S (x,y) xs s m)
 else Null
 
left:: Mystate-> Mystate
left(S (x,y) xs s m)=
 if y > 0 then S(x,(y-1)) xs "left" (S (x,y) xs s m)
 else Null
 
right:: Mystate->Mystate
right(S (x,y) xs s m)=
 if y < 3 then S(x,(y+1)) xs "right" (S (x,y) xs s m)
 else Null
 
find a []=False
find a (x:xs)=
 if a == x then True
 else find a xs

remove a []=[]
remove a (x:xs)=
 if a == x then xs
 else  (x:remove a xs)
 
collect:: Mystate-> Mystate
collect(S (x,y) xs s m)=
 if find (x,y) xs ==True then (S (x,y) (remove (x,y) xs) "collect" (S (x,y) xs s m))
 else Null

clean[]=[]
clean(x:xs)=
 if x==Null then clean xs
 else (x:clean xs)
 
nextMyStates::Mystate->[Mystate]
nextMyStates(S (x,y) xs s m)=
 clean([up(S (x,y) xs s m),down(S (x,y) xs s m),left(S (x,y) xs s m),right(S (x,y) xs s m),collect(S (x,y) xs s m)])

isEmpty []=True

isGoal:: Mystate->Bool
isGoal Null=False
isGoal(S (x,y) xs s m)=
 if xs==[] then True
 else False
 
search::[Mystate]->Mystate
search(x:xs)=
 if isGoal x then x
 else search(xs++nextMyStates x)
 
constructSolution:: Mystate ->[String]
constructSolution (S (x,y) xs "" m)=[]
constructSolution Null=[]
constructSolution(S (x,y) xs s m)= constructSolution m ++ [s]

solve :: Cell->[Cell]->[String]
solve c y = constructSolution(search([S c y "" Null]))

