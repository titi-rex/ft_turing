

-- b-tree implementation
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- utility to create node without branches
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

-- insert elem
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)  
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

-- check elem existance
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 

-- duplicate a tree
treeDup EmptyTree = EmptyTree
treeDup (Node a left right) = Node a (treeDup left) (treeDup right)

-- remove node in a tree (wip)
treeRemove x EmptyTree = EmptyTree
treeRemove x (Node a left@(Node lv lleft lright) right)  
    | x < a  = if x == lv then Node a lright right
        else Node a (treeRemove x left) right
    | x > a  = Node a left (treeRemove x right)
    | x == a = Node a left right


instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


