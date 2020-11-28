module Board where

    -- Access player, assigned to 1.
    mkPlayer :: (Integer, Char)
    mkPlayer = (1, 'X')
    -- Access opponent, assign to 2.
    mkOpponent :: (Integer, Char)
    mkOpponent = (2, 'O')

    -- Creates a board of m by n.
    mkBoard :: Int -> Int -> [[Int]]
    mkBoard m n = replicate m (replicate n 0 )

    --Finds which slot to drop the piece
    dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
    dropInSlot [] _ _ = [[]]
    dropInSlot bd i p 
        | not (isSlotOpen bd i) = []
        | otherwise = replaceSlot bd p (numRows bd) i

    --Replaces the empty slot with player number
    replaceSlot :: [[Int]] -> Int -> Int -> Int -> [[Int]]
    replaceSlot bd p r c
        | bd!!r!!c == 0 = (take r bd ++ [take c (bd!!r) ++ [p] ++ drop (c+1) (bd!!r)] ++ drop (r+1) bd) -- If empty slot, replace and update board
        | otherwise = replaceSlot bd p (r-1) c -- Slot Taken
    
    -- returns num of rows - 1
    numRows :: [[Int]] -> Int
    numRows bd = length (bd) - 1

    -- returns num of columns
    numSlot :: [[Int]] -> Int
    numSlot bd = length (bd!!0)

    -- Checks to see if slot i is open on board bd.
    isSlotOpen :: [[Int]] -> Int -> Bool
    isSlotOpen [[]] _ = False
    isSlotOpen bd i = if (bd !! 0) !! i == 0 then True else False

    -- Checks if the board is full.
    isFull :: [[Int]] -> Bool
    isFull bd = checkTopZero (head bd)

    -- Helper for isFull. Checks first row for 0.
    checkTopZero :: [Int] -> Bool
    checkTopZero (head : tail)
        | head == 0 = False
        | otherwise = checkTopZero tail


