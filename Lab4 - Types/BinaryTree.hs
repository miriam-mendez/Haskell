data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2

size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + height right `max` height left

equal :: Eq a => Tree a -> Tree a -> Bool 
equal Empty Empty = True
equal (Node a1 left1 right1) (Node a2 left2 right2) =
    a1 == a2 && equal left1 left2 && equal right1 right2
equal _ _ = False

isomorphic :: Eq a => Tree a -> Tree a -> Bool 
isomorphic Empty Empty = True
isomorphic (Node a1 left1 right1) (Node a2 left2 right2) =
     a1 == a2 && ((isomorphic left1 left2 && isomorphic right1 right2) || 
                (isomorphic left1 right2 && isomorphic left2 right1))
isomorphic _ _ = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a left right) = [a] ++ preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a left right) = inOrder left ++ [a] ++ inOrder right

breadthFirst :: Tree a -> [a]
breadthFirst t = bf [t]
    where 
        bf :: [Tree a] -> [a]
        bf [] = []
        bf (Empty:ts) = bf ts
        bf ((Node x t1 t2):ts) = [x] ++ bf (ts ++ [t1,t2])

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build pre@(px:pxs) post = Node px (build preL postL) (build preR postR)
    where
        (postL,_:postR) = span (/= px) post
        (preL, preR) = splitAt (length postL) pxs

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ Empty tree = tree
overlap _ tree Empty = tree
overlap f (Node a1 left1 right1) (Node a2 left2 right2) =
    Node (f a1 a2) (overlap f left1 left2) (overlap f right1 right2)

{-
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right -}

{-ghci> let nums = [8,6,4,1,7,3,5]
ghci> let numsTree = foldr treeInsert EmptyTree nums
ghci> numsTree
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree)) -}
