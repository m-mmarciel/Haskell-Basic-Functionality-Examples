--The majority of names are self-explanatory.
-------------------------------------------------------------------   
--median3
median3 :: Eq a => [a] -> a
median3 list =
    if len == 3 then
        --If they are not similar
        if ((list !! 0) /= (list !! 1)) && 
           ((list !! 0) /= (list !! 2)) &&
           ((list !! 1) /= (list !! 2)) then
           list !! 1

        --If they are similar (This is sloppy)
        else
            if (list !! 0) == (list !! 1)      then
                list !! 0
            else if (list !! 1) == (list !! 2) then
                list !! 1
            else if (list !! 0) == (list !! 2) then
                list !! 2
            else list !! 2
    else
        error "List must be of size 3"
    where len = length list
-------------------------------------------------------------------    
--midpoint
midpoint :: (Fractional a, Fractional b) => (a,b) -> (a,b) -> (a,b)
midpoint (x1,y1) (x2,y2) =
    --midpoint is the average of the x's and y's, combined into a tuple
    (((f1 + f2) / 2), ((s1 + s2) / 2))
    where
        f1 = (fst(x1,y1))
        f2 = (fst(x2,y2))
        s1 = (snd(x1,y1))
        s2 = (snd(x2,y2))
-------------------------------------------------------------------   
--allbutlast
allbutlast:: [a] -> [a]
allbutlast list =
    --If empty then error
    if null list then
        error "List is empty"
    else
        take (len - 1) (list)
    where
        len = (length list)
-------------------------------------------------------------------
--lastElem
lastElem:: [a] -> a
lastElem list =
    --If empty give error
    if null list then
        error "List is empty"
    else
        last list
-------------------------------------------------------------------
--elemAt
elemAt:: Int -> [a] -> a
elemAt index list =
    --Get the head of the list that drops everything before it
    head x
    where
        x = drop index list
-------------------------------------------------------------------
--replaceInList
replaceInList:: [a] -> Int -> a -> [a]
replaceInList list index x =
    --If out of range give an error
    if (index > (len + 1)) || (index < 0) then
        error "Index out of range"
    --Else the list is now what is before the index, the new element,
    --then what is after the index.
    else
        take index list ++ [x] ++ drop (index + 1) list
    where
        len = length list
-------------------------------------------------------------------
--replaceMax
replaceMax :: (Ord a) => [a] -> [a]
replaceMax list =
    --Replicate the list with element max for length of list
    replicate len max
    where
        max = maximum list
        len = length  list
-------------------------------------------------------------------
--largeToSmallPairs
largeToSmallPairs :: Ord a => [a] -> [a] -> [(a,a)]
largeToSmallPairs x y =
        --Return the zipped list but with only firsts being larger than 2nds
        filter (\x -> fst(x) > snd(x)) zippedList
    where
        zippedList = zip x y
-------------------------------------------------------------------
--containsElem
containsElem :: Eq a => a -> [a] -> Bool
containsElem elem list = 
    --If it's empty then nothing
    if null filteredList then
        False
    --Otherwise, it must be in the list
    else
        True
    --Filter the list with x being the item. 
    where
        filteredList = filter (\x -> x == elem) list
-------------------------------------------------------------------
--combine
combine :: Eq a => a -> [(a,b)] -> [(a,b)] -> [(a,b)]
combine a list1 list2 =
       --It is the concatenation of 2 filtered list where the
       --filter is just filtering which ones have "a" first.
       l1 ++ l2
    where
       l1 = filter (\x -> fst x == a) list1
       l2 = filter (\x -> fst x == a) list2
    
-------------------------------------------------------------------

--main
main = putStrLn $
    --show (median3 [1,1,99])
    --show (median3 [1,1,99,1])
    --show (median3 [1,2,3])
    --show (midpoint (0,0) (2,0))
    --show (allbutlast [1,2,3])
    --show (allbutlast [] :: [Int])
    --show (lastElem [1,2,3])
    --show (lastElem [] :: [Int])
    --show (elemAt 2 [1,2,3])
    --show (replaceInList [1,2,3,4] 3 99)
    --show (replaceInList [1,2,3,4] 9 99)
    --show (replaceInList [1,2,3,4] (-5) 99)
    --show (replaceMax [1,2,3,4])
    --show (replaceMax "test")
    --show (largeToSmallPairs [5,4,2,3,1] [5,2,3,1,4])
    --show (containsElem 3 [1,4,2,5])
    --show (containsElem 3 [1,4,2,5,3])
    show (combine 'a' [('b',10),('a',30)] [('c',10),('a',40),('a',50)])
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    