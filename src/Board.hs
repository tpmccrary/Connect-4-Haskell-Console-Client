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

    -- Board for manual testing.
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
        | otherwise = replaceSlot bd p (numRows bd - 1) i

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
        | length (tail) == 0 = True
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

    -- Checks every diagnoal. Top left to bottom right, top right to bottom left, bottom left to top right, and bottom right to top left. 
    checkDiagonals :: [[Int]] -> Int -> Bool
    checkDiagonals bd p = checkDiagonalsPos bd p 0 0 || checkDiagonalsNeg bd p 0 ((numSlot bd) - 1) || checkDiagonalsBotPos bd p ((numRows bd) - 1) 0  || checkDiagonalsBotNeg bd p ((numRows bd) - 1) ((numSlot bd) - 1)

    -- Checks all diagonals from top left to bottom right.
    checkDiagonalsPos :: [[Int]] -> Int -> Int -> Int -> Bool
    checkDiagonalsPos bd p row col
        | col >= numSlot bd || row >= numRows bd = False
    checkDiagonalsPos bd p row col = checkDiagTopPos bd p 0 row col || checkDiagonalsPos bd p row (col + 1)

    -- Check diagonal from top left to bottom right.
    checkDiagTopPos :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkDiagTopPos bd p count row col 
        | count >= 4 = True
        | row >= numRows bd || col >= numSlot bd = False
        | getElem bd row col == p = checkDiagTopPos bd p (count + 1) (row + 1) (col + 1)
        | getElem bd row col /= p = checkDiagTopPos bd p 0 (row + 1) (col + 1)

    -- Checks all diagonals from top right to bottom left.
    checkDiagonalsNeg :: [[Int]] -> Int -> Int -> Int -> Bool
    checkDiagonalsNeg bd p row col
        | row >= numRows bd || col < 0 = False
    checkDiagonalsNeg bd p row col = checkDiagTopNeg bd p 0 row col || checkDiagonalsNeg bd p row (col - 1)

    -- Check diagonal from top right to bottom left.
    checkDiagTopNeg :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkDiagTopNeg bd p count row col
        | count >= 4 = True
        | row >= numRows bd || col < 0 = False
        | getElem bd row col == p = checkDiagTopNeg bd p (count + 1) (row + 1) (col - 1)
        | getElem bd row col /= p = checkDiagTopNeg bd p 0 (row + 1) (col - 1)

    -- Checks all diagonals from bottom left to top right.
    checkDiagonalsBotPos :: [[Int]] -> Int -> Int -> Int -> Bool
    checkDiagonalsBotPos bd p row col
        | row < 0 || col >= numSlot bd = False
    checkDiagonalsBotPos bd p row col = checkDiagBotPos bd p 0 row col || checkDiagonalsBotPos bd p row (col + 1)

    -- Checks diagonal from bottom left to top right.
    checkDiagBotPos :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkDiagBotPos bd p count row col
        | count >= 4 = True
        | row < 0 || col >= numSlot bd = False
        | getElem bd row col == p = checkDiagBotPos bd p (count + 1) (row - 1) (col + 1)
        | getElem bd row col /= p = checkDiagBotPos bd p 0 (row - 1) (col + 1)

    -- Checks all diagonals from bottom right to top left.
    checkDiagonalsBotNeg :: [[Int]] -> Int -> Int -> Int -> Bool
    checkDiagonalsBotNeg bd p row col
        | row < 0 || col < 0 = False
    checkDiagonalsBotNeg bd p row col = checkDiagBotNeg bd p 0 row col || checkDiagonalsBotNeg bd p row (col - 1)

    -- Checks diagonal from bottom right to top left.
    checkDiagBotNeg :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    checkDiagBotNeg bd p count row col
        | count >= 4 = True
        | row < 0 || col < 0 = False
        | getElem bd row col == p = checkDiagBotNeg bd p (count + 1) (row - 1) (col - 1)
        | getElem bd row col /= p = checkDiagBotNeg bd p 0 (row - 1) (count - 1)

    -- [[0, 0, 0, 1, 0, 0],
    --  [0, 0, 1, 0, 0, 0],
    --  [0, 1, 0, 0, 0, 0],
    --  [1, 0, 0, 0, 0, 0],
    --  [0, 0, 0, 0, 0, 0],
    --  [0, 0, 0, 0, 0, 0]]
    
    -- Gets the element on given board and row col.
    getElem :: [[Int]] -> Int -> Int -> Int
    getElem bd row col = (bd !! row) !! col

    --Turns board to string
    boardToStr playerToChar bd = changeRow playerToChar (concat bd) 1

    -- Creates indexed row and converts each row to string
    changeRow playerToChar [] i = " 1  2  3  4  5  6  7"
    changeRow playerToChar (h:t) i
        | (i == 7) = (playerToChar h)++"\n"++(changeRow playerToChar t 1)
        | otherwise = (playerToChar h)++(changeRow playerToChar t (i+1))

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
