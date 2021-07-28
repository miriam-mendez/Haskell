data Queue a = Queue [a] [a] deriving (Show)

-- c = push 3 (push 2 (push 1 create))

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue xs ys) = Queue xs (x:ys)

pop :: Queue a -> Queue a
pop (Queue (x:xs) ys) = Queue xs ys
pop (Queue [] ys) = pop $ Queue (reverse ys) []

top :: Queue a -> a
top (Queue (x:xs) ys) = x
top (Queue [] ys) = top $ Queue (reverse ys) []

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a) where 
        (==) :: Queue a -> Queue a -> Bool
        (/=) :: Queue a -> Queue a -> Bool
        p == q = reverse' p == reverse' q

reverse' :: Queue a -> [a]
reverse' (Queue x y) = x ++ reverse y