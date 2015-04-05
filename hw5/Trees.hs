module Trees where


data BinarySearchTree = INil | ITreeNode { int_value :: Int
										 , left_int_node :: BinarySearchTree
										 , right_int_node :: BinarySearchTree
										 } deriving (Show, Eq, Ord)
-- interp. binary search tree for int elements

nilTree, simpleTree, bTree :: BinarySearchTree
nilTree = INil
simpleTree = ITreeNode 5 INil INil
bTree = ITreeNode 4 (ITreeNode 2 (ITreeNode 1 INil INil) (ITreeNode 3 INil INil)) (ITreeNode 6 (ITreeNode 5 INil INil) (ITreeNode 7 INil INil))

hight :: BinarySearchTree -> Int
hight tree = case tree of
				INil -> 0
				ITreeNode val lNode rNode -> 1 + (max (hight lNode) (hight rNode))

sum_elem tree = case tree of
					INil -> 0
					ITreeNode val lNode rNode -> val + (sum_elem lNode) + (sum_elem rNode)

find_elem_node :: Int -> BinarySearchTree -> BinarySearchTree
find_elem_node el tree = case tree of 
							INil -> INil
							ITreeNode val lNode rNode -> if val == el
															then tree
															else if val > el
																	then find_elem_node el lNode
																	else find_elem_node el rNode

contains_elem :: Int -> BinarySearchTree -> Bool
contains_elem el tree = case find_elem_node el tree of 
							INil -> False
							ITreeNode val lNode rNode -> True


data BinaryTree a = Nil | TreeNode { value :: a
								   , left_node :: BinaryTree a
								   , right_node :: BinaryTree a
								   } deriving (Show, Eq, Ord)
-- interp. binary search tree for any type elements

nTr, sTr, bTr :: BinaryTree Int
nTr = Nil
sTr = TreeNode 5 Nil Nil
bTr = TreeNode 4 (TreeNode 2 (TreeNode 1 Nil Nil) (TreeNode 3 Nil Nil)) (TreeNode 6 (TreeNode 5 Nil Nil) (TreeNode 7 Nil Nil))

tr_hight :: BinaryTree a -> Int
tr_hight tree = case tree of
					Nil -> 0
					TreeNode val lNode rNode -> 1 + (max (tr_hight lNode) (tr_hight rNode))

tmap :: BinaryTree a -> (a -> b) -> BinaryTree b
tmap tree fun = case tree of
					Nil -> Nil
					TreeNode val lNode rNode -> TreeNode (fun val) (tmap lNode fun) (tmap rNode fun)


data AltList a b = LNil | LCons a (AltList b a) deriving (Show, Eq, Ord)
-- interp. list for two type of elements

nList, sList, bList :: AltList String Int
nList = LNil
sList = LCons "a" (LCons 1 LNil)
bList = LCons "a" (LCons 1 (LCons "b" (LCons 2 (LCons "c" (LCons 3 LNil)))))

list_len :: AltList a b -> Int
list_len ls = case ls of
				LNil -> 0
				LCons x xs -> 1 + (list_len xs)

dmap :: AltList a b -> (a -> c) -> (b -> d) -> AltList c d
dmap ls fun1 fun2 = case ls of 
						LNil -> LNil
						LCons x xs -> LCons (fun1 x) (dmap xs fun2 fun1)