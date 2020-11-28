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
