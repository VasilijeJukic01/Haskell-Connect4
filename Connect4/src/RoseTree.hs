module RoseTree (
    size,
    height,
    leavesCount,
    leaves,
    elemsOnDepth,
    foldRose,
    genereteRose
)where


data Rose a = Node a [Rose a] deriving Show


-- sample for testing
rt :: Rose Int
rt = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]

size :: Rose a -> Int
size (Node _ childs) = 1 + foldl (\acc x -> acc + size x) 0 childs


height :: Rose a -> Int
height (Node _ childs) = 1 + foldl (\acc x -> max acc $ height x) 0 childs


leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ childs) = sum (map leavesCount childs) -- 3 [2 1 10] 2 [1 3] ----------> [2, 1, 1] ----- [1,1]


leaves :: Rose a -> [a]
leaves (Node root []) = [root]
leaves (Node _ childs) = concatMap leaves childs


elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node root _) = [root] 
elemsOnDepth depth (Node _ childs) = concatMap (elemsOnDepth (depth - 1)) childs


instance Functor Rose where
    fmap f (Node root childs) = Node (f root) (map (fmap f) childs)


foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node root childs) = foldl (foldRose f) (f acc root) childs


genereteRose :: (a -> [a]) -> Int -> a -> Rose a
genereteRose f 0 root = Node root [] 
genereteRose f depth root = Node root (map (genereteRose f (depth - 1)) (f root))


