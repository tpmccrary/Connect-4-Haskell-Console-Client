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

    -- Checks to see if slot i is open on board bd.
    isSlotOpen :: [[Int]] -> Int -> Bool
    isSlotOpen [[]] _ = False
    isSlotOpen bd i = if bd !! 0 !! i == 0 then True else False

    -- Checks if the board is full.
    isFull :: [[Int]] -> Int -> Bool
    isFull bd i = if i >= numSlot bd then True
    else if isSlotOpen bd i then False
    else isFull bd (i + 1) 