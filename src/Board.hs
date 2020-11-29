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
    testBoard = [[0, 0, 0, 0, 0, 0],
                 [0, 0, 0, 0, 0, 0],
                 [0, 0, 0, 0, 0, 0],
                 [0, 0, 0, 0, 0, 0],
                 [0, 0, 0, 0, 0, 0],
                 [0, 0, 0, 0, 0, 0]]

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
    
    -- returns num of rows
    numRows :: [[Int]] -> Int
    numRows bd = length (bd)

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

    -- Checks to see if given player won on given board.
    isWonBy :: [[Int]] -> Int -> Bool
    isWonBy bd p = checkHorizontals bd p || checkVerticals bd p 0 || checkDiagonals bd p

    -- Goes through every row and calls checkHorizontal to get them.
    checkHorizontals :: [[Int]] -> Int -> Bool
    checkHorizontals [] _ = False
    checkHorizontals (head : tail) p = checkHorizontal head p 0  || checkHorizontals tail p

    -- Goes through a list to see if there are 4 in a row of the given players pieces.
    checkHorizontal :: [Int] -> Int -> Int -> Bool
    checkHorizontal [] _ count = count >= 4
    checkHorizontal (head : tail) p count
        | count >= 4 = True
        | head == p = checkHorizontal tail p (count + 1)
        | head /= p = checkHorizontal tail p 0
    
    -- Goes through each column and calls on checkVertical to check for 4.
    checkVerticals :: [[Int]] -> Int -> Int -> Bool
    checkVerticals bd _ col 
        | col >= (numSlot bd) = False 
    checkVerticals bd p col = checkVertical bd p 0 0 col || checkVerticals bd p (col + 1)

    -- Checks for 4 in a row in a column
    checkVertical :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkVertical bd p count row col
        | count >= 4 = True
        | row >= numRows bd = False
        | getElem bd row col == p = checkVertical bd p (count + 1) (row + 1) col
        | getElem bd row col /= p = checkVertical bd p 0 (row + 1) col

    -- Checks every diagnoal. Top left to bottom right, top right to bottom left. 
    checkDiagonals :: [[Int]] -> Int -> Bool
    checkDiagonals bd p = checkDiagonalsPos bd p 0 0 || checkDiagonalsNeg bd p 0 ((numSlot bd) - 1)

    checkDiagonalsPos :: [[Int]] -> Int -> Int -> Int -> Bool
    checkDiagonalsPos bd p row col
        | col >= numSlot bd || row >= numRows bd = False
    checkDiagonalsPos bd p row col = checkDiagTopPos bd p 0 row col || checkDiagonalsPos bd p row (col + 1)

    checkDiagonalsNeg :: [[Int]] -> Int -> Int -> Int -> Bool
    checkDiagonalsNeg bd p row col
        | row >= numRows bd || col < 0 = False
    checkDiagonalsNeg bd p row col = checkDiagTopNeg bd p 0 row col || checkDiagonalsNeg bd p row (col - 1)


    checkDiagTopPos :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkDiagTopPos bd p count row col 
        | count >= 4 = True
        | row >= numRows bd || col >= numSlot bd = False
        | getElem bd row col == p = checkDiagTopPos bd p (count + 1) (row + 1) (col + 1)
        | getElem bd row col /= p = checkDiagTopPos bd p 0 (row + 1) (col + 1)

    checkDiagTopNeg :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkDiagTopNeg bd p count row col
        | count >= 4 = True
        | row >= numRows bd || col < 0 = False
        | getElem bd row col == p = checkDiagTopNeg bd p (count + 1) (row + 1) (col - 1)
        | getElem bd row col /= p = checkDiagTopNeg bd p 0 (row + 1) (col - 1)

    

    -- [[0, 0, 0, 1, 0, 0],
    --  [0, 0, 1, 0, 0, 0],
    --  [0, 1, 0, 0, 0, 0],
    --  [1, 0, 0, 0, 0, 0],
    --  [0, 0, 0, 0, 0, 0],
    --  [0, 0, 0, 0, 0, 0]]
    
    -- Gets the element on given board and row col.
    getElem :: [[Int]] -> Int -> Int -> Int
    getElem bd row col = (bd !! row) !! col


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
