import Test.Hspec 

-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq) 

-- HELPER FUNCTIONS AND DEFINTIONS

findSubPair :: Int -> [Int] -> Int -> [[Int]]
findSubPair _ [] _ = []
findSubPair x (y:ys) target
    | sumEqualsTarget && xGreaterOrEqual = [pairMachine x y] `listFixer` findSubPair x ys target
    | sumEqualsTarget && not xGreaterOrEqual = [pairMachine y x] `listFixer` findSubPair x ys target
    | otherwise = findSubPair x ys target
  where
    sumEqualsTarget = x + y == target
    xGreaterOrEqual = x >= y

-- Helper function to creater pair
pairMachine :: Int -> Int -> [Int]
pairMachine a b = [a, b]

-- append function for lists
listFixer :: [[Int]] -> [[Int]] -> [[Int]]
listFixer [] ys = ys
listFixer (x:xs) ys = x : listFixer xs ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

removeDuplicates :: Eq a => [[a]] -> [[a]]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:y:xs)
  | x == y    = removeDuplicates (x:xs)
  | otherwise = x : removeDuplicates (y:xs)

findPairs :: Int -> [Int] -> [[Int]]
findPairs target nums = removeDuplicates $ sortPairs [ [x, y] | x <- nums, y <- nums, x < y, x + y == target ]



isMirror :: Eq a => Tree a -> Tree a -> Bool
isMirror Nil Nil = True
isMirror (TreeNode left1 val1 right1) (TreeNode left2 val2 right2) = val1 == val2 && isMirror left1 right2 && isMirror right1 left2
isMirror _ _ = False


reverserFunct :: [[a]] -> [a]
reverserFunct xs = revHelper xs True
  where
    revHelper [] _ = []
    revHelper (x:xs) flag
      | flag = x ++ revHelper xs (not flag)
      | otherwise = reverse x ++ revHelper xs (not flag)

collectLevels :: [Tree a] -> [[a]]
collectLevels [] = []
collectLevels nodes = currentLevel : collectLevels (mergeAll nextLevels)
  where
    currentLevel = [x | TreeNode _ x _ <- nodes]
    nextLevels = [(left, right) | TreeNode left _ right <- nodes]

    mergeAll :: [(Tree a, Tree a)] -> [Tree a]
    mergeAll [] = []
    mergeAll ((left, right):rest) = left : right : mergeAll rest




reverseList :: LinkedList a -> LinkedList a
reverseList = reverseListHelper Null
  where
    reverseListHelper acc Null = acc
    reverseListHelper acc (ListNode x xs) = reverseListHelper (ListNode x acc) xs

------------------------------------------------------------------------------------------------
linkedListToString :: LinkedList Char -> String
linkedListToString Null = ""
linkedListToString (ListNode c rest) = c : linkedListToString rest

stringToLinkedList :: String -> LinkedList Char
stringToLinkedList "" = Null
stringToLinkedList (c:cs) = ListNode c (stringToLinkedList cs)

-- Helper function to find the longest common prefix of two strings
longestCommonPrefix :: String -> String -> String
longestCommonPrefix (x:xs) (y:ys) = if x == y then x : longestCommonPrefix xs ys else []
longestCommonPrefix _ _ = []

-- Updated function to find the longest common substring
longestCommonSubstr :: String -> String -> String
longestCommonSubstr s1 s2 =
    let substrings1 = [drop i s1 | i <- [0..length s1]]
        substrings2 = [drop j s2 | j <- [0..length s2]]
        commonSubstrings = [longestCommonPrefix s1' s2' | s1' <- substrings1, s2' <- substrings2]
    in findLongest commonSubstrings ""
    where
      findLongest [] longest = longest
      findLongest (x:xs) longest = findLongest xs (if length x > length longest then x else longest)

      longestCommonPrefix (x:xs) (y:ys) | x == y = x : longestCommonPrefix xs ys
      longestCommonPrefix _ _ = ""

-----------------------------------------------------------------------------------------------------


constructTree :: String -> Tree Char -> (Tree Char, String)
constructTree [] tree = (tree, [])
constructTree (x:xs) Nil
  | x == '^' = (Nil, xs)
  | otherwise = constructTree xs (TreeNode Nil x Nil)
constructTree str@(x:xs) tree@(TreeNode left val right)
  | x == '^' = case right of
                 Nil -> let (newRight, restStr) = constructTree xs Nil 
                        in (TreeNode left val newRight, restStr)
                 _ -> let (parentUpdated, restStr) = constructTree xs tree 
                      in (parentUpdated, restStr)
  | otherwise = let (newLeft, restStr) = insertLeft str left 
                in (TreeNode newLeft val right, restStr)

insertLeft :: String -> Tree Char -> (Tree Char, String)
insertLeft [] tree = (tree, [])
insertLeft (x:xs) Nil
  | x == '^' = (Nil, xs)
  | otherwise = insertLeft xs (TreeNode Nil x Nil)
insertLeft str@(x:xs) tree@(TreeNode left val right)
  | x == '^' = (tree, xs)
  | otherwise = let (newLeft, restStr) = insertLeft str left
                in (TreeNode newLeft val right, restStr)
--maze question
mazeHelper :: Int -> Int -> Int -> Int -> [[Int]] -> Int
mazeHelper x y xaxis yaxis m
  | x == xaxis - 1 && y == yaxis - 1 = m !! x !! y
  | x == xaxis - 1 = m !! x !! y + mazeHelper x (y + 1) xaxis yaxis m
  | y == yaxis - 1 = m !! x !! y + mazeHelper (x + 1) y xaxis yaxis m
  | otherwise = m !! x !! y + myMin (mazeHelper (x + 1) y xaxis yaxis m) (mazeHelper x (y + 1) xaxis yaxis m)
    where
      myMin a b = if a < b then a else b


--Category: Easy

-- Question 1
targetSum :: [Int] -> Int ->[[Int]]
compareLists [] y = False
compareLists x [] = True
compareLists (a:as) (b:bs)
    | a > b = False
    | a < b = True
    | otherwise = True

sortPairs [] = []
sortPairs [x] = [x]
sortPairs (x:y:xs)
    | compareLists x y = x : sortPairs (y:xs)
    | otherwise = y : sortPairs (x:xs)

targetSum [] target = []
targetSum [x] target = []
targetSum (x:xs) target = sortPairs(findSubPair x xs target ++ targetSum xs target)

-- Question 2
symmetricTree :: Eq a => Tree a -> Bool
symmetricTree Nil = True
symmetricTree (TreeNode left val right) = isMirror left right


-- Question 3
palindromList :: Eq a => LinkedList a -> Bool
appendToEnd value Null = ListNode value Null
appendToEnd value (ListNode y ys) = ListNode y (appendToEnd value ys)

reversedList Null = Null
reversedList (ListNode x xs) = appendToEnd x (reversedList xs)

palindromList Null = True
palindromList list = list == reversedList list



-- Question 4
snakeTraversal :: Tree a -> [a]
snakeTraversal Nil = []
snakeTraversal root = reverserFunct (collectLevels [root])

-- Question 5
treeConstruction :: String -> Tree Char
treeConstruction str = fst $ constructTree str Nil


-- Category: Medium

-- Attempy any 4 questions from this category

-- Question 1.1: Overload the (+) operator for Tree. You only need to overload (+). Keep the rest of the operators as undefined.   
instance Num (Tree Int) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

-- Question 1.2

longestCommonString :: LinkedList Char -> LinkedList Char -> LinkedList Char
longestCommonString ll1 ll2 = stringToLinkedList (longestCommonSubstr (linkedListToString ll1) (linkedListToString ll2))


-- Question 2
commonAncestor :: Ord a => Eq a => Tree a -> a -> a -> Maybe a
commonAncestor Nil _ _ = Nothing
commonAncestor (TreeNode left val right) a b
  | val == a || val == b = Just val
  | otherwise = case (commonAncestor left a b, commonAncestor right a b) of
    (Just x, Just y) -> Just val
    (Just x, Nothing) -> Just x
    (Nothing, Just y) -> Just y
    (Nothing, Nothing) -> Nothing

-- Question 3
gameofLife :: [[Int]] -> [[Int]]
gameofLife board
  | null board || null (board !! 0) = [] 
  | otherwise = [[nextState (x, y) | y <- [0..m-1]] | x <- [0..n-1]]
  where
    n = length board -- Number of rows
    m = if not (null board) then length (board !! 0) else 0 
    nextState (x, y) = case (getCell x y, liveNeighbors x y) of
      (1, neighbors) | neighbors < 2 -> 0 -- rule1
                     | neighbors == 2 || neighbors == 3 -> 1 --rule2
                     | neighbors > 3 -> 0 --rule3
      (0, 3) -> 1 --rule4
      (_, _) -> getCell x y
    getCell x y = if x >= 0 && x < n && y >= 0 && y < m then board !! x !! y else 0

    liveNeighbors x y = countLiveNeighbors x y [-1..1] [-1..1] 0
    countLiveNeighbors _ _ [] _ acc = acc
    countLiveNeighbors x y (_:dxs) [] acc = countLiveNeighbors x y dxs [-1..1] acc
    countLiveNeighbors x y (dx:dxs) (dy:dys) acc
      | dx == 0 && dy == 0 = countLiveNeighbors x y (dx:dxs) dys acc -
      | otherwise = countLiveNeighbors x y (dx:dxs) dys (acc + getCell (x + dx) (y + dy))



-- Question 4
waterCollection :: [Int] -> Int
waterCollection heights = sum [max 0 (min (leftMax !! i) (rightMax !! i) - heights !! i) | i <- [0..length heights - 1]]
  where
    leftMax = scanl max 0 heights 
    rightMax = scanr max 0 heights 


-- Question 5
minPathMaze :: [[Int]] -> Int
minPathMaze m = mazeHelper 0 0 (length m) (length (m !! 0)) m








-- Main Function
main :: IO ()
main =
   hspec $ do

    -- Test List Target Sum
        describe "targetSum" $ do
            it "should return pairs whose sum is equal to the target" $ do
                targetSum [1,2,3,4,5] 5 `shouldBe` [[3,2], [4,1]]
                targetSum [1,2,3,4,5,6] 10 `shouldBe` [[6,4]]
                targetSum [1,2,3,4,5] 0 `shouldBe` []
                targetSum [1,10,8,7,6,2,3,4,5,-1,9] 10 `shouldBe` [[6,4],[7,3],[8,2],[9,1]]
    
    -- Test Symmetric Tree
        describe "symmetricTree" $ do
            it "should return True if the tree is symmetric" $ do
                symmetricTree (Nil :: Tree Int) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 1 Nil)) `shouldBe` True
                symmetricTree (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 2 Nil)) `shouldBe` False
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 1 Nil))) `shouldBe` True
                symmetricTree (TreeNode (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 4 (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 4 Nil))) `shouldBe` False
    
    -- Test Palindrom List
        describe "palindromList" $ do
            it "should return True if the list is a palindrome" $ do
                palindromList (Null :: LinkedList Int) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 Null))))) `shouldBe` True
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 3 (ListNode 1 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 2 Null))))) `shouldBe` False
                palindromList (ListNode 1 (ListNode 2 (ListNode 3 (ListNode 2 (ListNode 1 (ListNode 1 Null)))))) `shouldBe` False
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'b' (ListNode 'a' Null))))) `shouldBe` True
                palindromList (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'c' (ListNode 'a' Null))))) `shouldBe` False
    
    -- Test Snake Traversal
        describe "snakeTraversal" $ do
            it "should return the snake traversal of the tree" $ do
                snakeTraversal (Nil:: Tree Int) `shouldBe` []
                snakeTraversal (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) `shouldBe` [2,3,1]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil))) `shouldBe` [4,2,3,1,6,5,7]
                snakeTraversal (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode (TreeNode Nil 9 Nil) 7 Nil))) `shouldBe` [4,2,3,1,6,5,7,9]
    
    -- Test Tree Construction
        describe "treeConstruction" $ do
            it "should return the tree constructed from the string" $ do
                treeConstruction "" `shouldBe` Nil
                treeConstruction "a" `shouldBe` TreeNode Nil 'a' Nil
                treeConstruction "^a" `shouldBe` Nil
                treeConstruction "ab^c" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^c^" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode Nil 'c' Nil)
                treeConstruction "ab^cde^f" `shouldBe` TreeNode (TreeNode Nil 'b' Nil) 'a' (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil)
                treeConstruction "abcde^f" `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode (TreeNode Nil 'e' Nil) 'd' (TreeNode Nil 'f' Nil)) 'c' Nil) 'b' Nil) 'a' Nil
    
    -- Test (+) operator for Tree
        describe "(+)" $ do
            it "should return the sum of the two trees" $ do
                let result1 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) :: Tree Int) 
                result1  `shouldBe` TreeNode (TreeNode Nil 2 Nil) 4 (TreeNode Nil 6 Nil) 
                let result2 = (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil) + TreeNode Nil 2 (TreeNode Nil 3 Nil) :: Tree Int)
                result2 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 4 (TreeNode Nil 6 Nil)
                let result3 = (Nil + Nil :: Tree Int) 
                result3 `shouldBe` Nil
                let result4 = (Nil + TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil):: Tree Int)
                result4 `shouldBe` TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)
                let result5 = (TreeNode (TreeNode (TreeNode Nil 1 (TreeNode Nil (-2) Nil)) 3 Nil) 4 (TreeNode Nil 2 (TreeNode Nil 7 (TreeNode Nil (-7) Nil))) + TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 1 Nil) 3 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 4 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 2 (TreeNode (TreeNode Nil (-5) Nil) 7 Nil)) :: Tree Int) 
                result5 `shouldBe` TreeNode (TreeNode (TreeNode (TreeNode Nil 0 Nil) 2 (TreeNode Nil (-2) Nil)) 6 (TreeNode (TreeNode Nil 1 Nil) 6 (TreeNode Nil (-2) Nil))) 8 (TreeNode (TreeNode (TreeNode Nil 9 Nil) 5 (TreeNode Nil 4 Nil)) 4 (TreeNode (TreeNode Nil (-5) Nil) 14 (TreeNode Nil (-7) Nil)))
                let result6 = (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) + TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 6 Nil)) 4 (TreeNode (TreeNode Nil 5 Nil) 2 (TreeNode Nil 7 Nil)) :: Tree Int) 
                result6 `shouldBe` TreeNode (TreeNode (TreeNode Nil 2 Nil) 6 (TreeNode Nil 12 Nil)) 8 (TreeNode (TreeNode Nil 10 Nil) 4 (TreeNode Nil 14 Nil))
    
    -- Test Longest Common String
        describe "longestCommonString" $ do
            it "should return the longest common string" $ do
                longestCommonString Null Null `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) Null `shouldBe` Null
                longestCommonString Null (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` Null
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'f' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' Null)))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'f' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' (ListNode 'c' Null))
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'b' (ListNode 'f' (ListNode 'g' (ListNode 'e' Null))))) `shouldBe` ListNode 'a' (ListNode 'b' Null)
                longestCommonString (ListNode 'a' (ListNode 'b' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) (ListNode 'a' (ListNode 'f' (ListNode 'c' (ListNode 'd' (ListNode 'e' Null))))) `shouldBe` ListNode 'c' (ListNode 'd' (ListNode 'e' Null))
    
    -- Test Common Ancestor
        describe "commonAncestor" $ do
            it "should return the lowest common ancestor of the two nodes" $ do
                commonAncestor Nil 1 2 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 3 `shouldBe` Just 2
                commonAncestor (TreeNode (TreeNode Nil 1 Nil) 2 (TreeNode Nil 3 Nil)) 1 4 `shouldBe` Nothing
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 6 `shouldBe` Just 5
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 8 9 `shouldBe` Just 8
                commonAncestor (TreeNode (TreeNode (TreeNode Nil 1 Nil) 3 (TreeNode Nil 4 Nil)) 5 (TreeNode (TreeNode Nil 6 Nil) 8 (TreeNode Nil 9 Nil))) 1 3 `shouldBe` Just 3
                
    
    -- Test Game of Life
        describe "gameofLife" $ do
            it "should return the next state" $ do
                gameofLife [[0,1,0],[0,0,1],[1,1,1],[0,0,0]] `shouldBe` [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
                gameofLife [[1,1],[1,0]] `shouldBe` [[1,1],[1,1]]
                gameofLife [[1,1],[1,1]] `shouldBe` [[1,1],[1,1]]
                gameofLife [[1,0],[0,1]] `shouldBe` [[0,0],[0,0]]
                gameofLife [[0,1,0,0],[0,1,1,1],[1,0,1,1]] `shouldBe` [[0,1,0,0],[1,0,0, 1],[0,0,0,1]]
    
    -- Test Water Collection
        describe "waterCollection" $ do
            it "should return the amount of water that can be trapped" $ do
                waterCollection [0,1,0,2,1,0,1,3,2,1,2,1] `shouldBe` 12
                waterCollection [4,2,0,3,2,5] `shouldBe` 18
                waterCollection [1,2,3,4,5] `shouldBe` 0
                waterCollection [5,4,3,2,1] `shouldBe` 0
                waterCollection [5,4,3,2,1,2,3,4,5] `shouldBe` 32  
                waterCollection [1, 0, 2, 3, 1, 4] `shouldBe` 6
                waterCollection [0, 4, 1, 2, 0, 1, 3] `shouldBe` 16
    
    -- Test Min Path Maze
        describe "minPathMaze" $ do
            it "should return the minimum cost to reach the bottom right cell" $ do
                minPathMaze [[1,3,1],[1,5,1],[4,2,1]] `shouldBe` 7
                minPathMaze [[1,2,3],[4,5,6],[7,8,9]] `shouldBe` 21
                minPathMaze [[1,2,3,4],[4,5,6,7],[7,8,9,9],[10,11,1,13]] `shouldBe` 35
                minPathMaze [[1,2,3,4,5],[4,5,6,7,8],[7,8,9,9,10],[10,11,1,13,14],[15,16,17,18,19]] `shouldBe` 66
                minPathMaze [[1,2,3,4,5,6],[4,1,2,7,8,9],[7,8,1,2,10,11],[10,11,1,2,22,15],[15,16,17,1,2,20],[21,22,23,24,2,26]] `shouldBe` 41
