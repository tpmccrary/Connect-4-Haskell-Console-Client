module Board where

    -- Access player, assigned to 1.
    mkPlayer :: Int
    mkPlayer = 1
    -- Access opponent, assign to 2.
    mkOpponent :: Int
    mkOpponent = 2

    -- Creates a board of m by n.
    mkBoard :: Int -> Int -> [[Int]]
    mkBoard m n = replicate m (replicate n 0 )

    testBoard :: [[Int]]
    testBoard = [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15], [16, 17, 18, 19, 20]]

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

    -- isWonBy :: [[Int]] -> Int -> Bool
    -- isWonBy bd p = checkHorizontal bd p 0 0

    -- checkHorizontal :: [[Int]] -> Int -> Int -> Int -> Bool
    -- checkHorizontal (head : tail) p count col
    --     | count >= 4 = True
    --     | length tail == 0 = False
    --     | col >= 
    --     | head !! col == p = checkHorizontal head p (count + 1) (col + 1)


    -- EXAMPLES of using (head : tail) with 2D array.
    -- Here we are looking at the rest of the 2d array. So excluding the first array. 
    test :: [[Int]] -> [Int]
    test (head : tail) = testRow tail
    -- Here we look at the first thing in the 2d array given. Which was the tail of the last 2d array.
    testRow :: [[Int]] -> [Int]
    testRow (head : tail) = head
    -- Test function. Outputs evertything in given 2d array.
    returnArr :: [[Int]] -> [[Int]]
    returnArr [] = []
    returnArr (head : tail) = head : returnArr tail
