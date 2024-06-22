module RoseTree (
    size,
    height,
    leavesCount,
    leaves,
    elemsOnDepth,
    foldRose,
    genereteRose,
    Rose
)where


-- Structures --
data Rose a = Node a [Rose a] deriving Show


-- Sample for testing
{-
      Node 1
     /   |
 Node 2 Node 3
        /   \
    Node 4 Node 5
-}
rt :: Rose Int
rt = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]


-- Functions --
{- 
1. size (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])      => Add 1 for Node 1, call size for children
2. size (Node 2 [])                                              => Add 1 for Node 2, no children
3. size (Node 3 [Node 4 [], Node 5 []])                          => Add 1 for Node 3, call size for children
4. size (Node 4 []) & size (Node 5 [])                           => Add 1 for each, no children

1 (Node 1) + 1 (Node 2) + 1 (Node 3) + 1 (Node 4) + 1 (Node 5) = 5
-}
size :: Rose a -> Int
size (Node _ children) = 1 + foldl (\acc x -> acc + size x) 0 children


{-
1. height (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])    => Call height for children
2. height (Node 2 [])                                            => Leaf node, return 1
3. height (Node 3 [Node 4 [], Node 5 []])                        => Call height for children
4. height (Node 4 []) & height (Node 5 [])                       => Leaf nodes, return 1 for each

height (Node 1) = 1 + max (height (Node 2)) (height (Node 3))
                = 1 + max (1) (1 + max (height (Node 4), height (Node 5)))
                = 1 + max (1) (1 + max (1, 1))
                = 1 + max (1) (2)
                = 1 + 2 = 3
-}
height :: Rose a -> Int
height (Node _ children) = 1 + foldl (\acc x -> max acc $ height x) 0 children


{-
1. leavesCount (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])  => Call leavesCount for children
2. leavesCount (Node 2 [])                                          => Leaf node, return 1
3. leavesCount (Node 3 [Node 4 [], Node 5 []])                      => Call leavesCount for children
4. leavesCount (Node 4 []) & leavesCount (Node 5 [])                => Leaf nodes, return 1 for each

leavesCount (Node 1) = leavesCount (Node 2) + leavesCount (Node 3) 
                     = 1 + (leavesCount (Node 4) + leavesCount (Node 5)) 
                     = 1 + (1 + 1) = 3
-}
leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ children) = sum (map leavesCount children) -- 3 [2 1 10] 2 [1 3] ----------> [2, 1, 1] ----- [1,1]


{-
1. leaves (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])      => Call leaves for children
2. leaves (Node 2 [])                                              => Leaf node, return [2]
3. leaves (Node 3 [Node 4 [], Node 5 []])                          => Call leaves for children
4. leaves (Node 4 []) & leaves (Node 5 [])                         => Leaf nodes, return [4] and [5]

leaves (Node 1) = leaves (Node 2) ++ leaves (Node 3) 
                = [2] ++ (leaves (Node 4) ++ leaves (Node 5)) 
                = [2] ++ ([4] ++ [5]) = [2, 4, 5]
-}
leaves :: Rose a -> [a]
leaves (Node root []) = [root]
leaves (Node _ children) = concatMap leaves children


{-
1. elemsOnDepth 0 (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])  => Depth is 0, return [1]
2. elemsOnDepth 1 (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]])  => Depth is 1, call elemsOnDepth for children
3. elemsOnDepth 0 (Node 2 [])                                          => Depth is 0, return [2]
4. elemsOnDepth 0 (Node 3 [Node 4 [], Node 5 []])                      => Depth is 0, return [3]

elemsOnDepth 1 (Node 1) = elemsOnDepth 0 (Node 2) ++ elemsOnDepth 0 (Node 3) 
                        = [2] ++ [3] = [2, 3]

elemsOnDepth 2 (Node 1) = elemsOnDepth 1 (Node 2) ++ elemsOnDepth 1 (Node 3)
                        = [] ++ (elemsOnDepth 0 (Node 4) ++ elemsOnDepth 0 (Node 5))
                        = [] ++ ([4] ++ [5]) = [4, 5]
-}
elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node root _) = [root] 
elemsOnDepth depth (Node _ children) = concatMap (elemsOnDepth (depth - 1)) children


instance Functor Rose where
    fmap f (Node root children) = Node (f root) (map (fmap f) children)


{-
foldRose (+) 0 rt

1. foldRose (+) 0 (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) => Initial acc = 0, apply (+) with 1
2. foldRose (+) 1 (Node 2 [])                                         => Apply (+) with 2
3. foldRose (+) 3 (Node 3 [Node 4 [], Node 5 []])                     => Apply (+) with 3
4. foldRose (+) 6 (Node 4 []) & foldRose (+) 6 (Node 5 [])            => Apply (+) with 4 and 5

Result = 0 + 1 + 2 + 3 + 4 + 5 = 15
-}
foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node root children) = foldl (foldRose f) (f acc root) children


{-
Let's assume the function is: genereteRose (\x -> [x*2, x*2+1]) 2 1

1. genereteRose (\x -> [x*2, x*2+1]) 2 1 
2. Node 1 [genereteRose (\x -> [x*2, x*2+1]) 1 2, genereteRose (\x -> [x*2, x*2+1]) 1 3]
3. Node 2 [genereteRose (\x -> [x*2, x*2+1]) 0 4, genereteRose (\x -> [x*2, x*2+1]) 0 5]
4. Node 3 [genereteRose (\x -> [x*2, x*2+1]) 0 6, genereteRose (\x -> [x*2, x*2+1]) 0 7]
5. Node 4 [], Node 5 [], Node 6 [], Node 7 []

Result = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 [Node 6 [], Node 7 []]]
-}
genereteRose :: (a -> [a]) -> Int -> a -> Rose a
genereteRose f 0 root = Node root [] 
genereteRose f depth root = Node root (map (genereteRose f (depth - 1)) (f root))
