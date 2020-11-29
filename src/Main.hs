module Main where
    import System.IO
    import Board

    -- Global gameBoard to print
    gameBoard = []

    readSlot :: [[Int]] -> Int -> IO()
    readSlot [[]] _ = putStrLn "Invalid Input, exiting"
    readSlot bd p = do 
        putStrLn "Enter Slot Position"
        slotPos <- getX
        if slotPos == 8999 then putStrLn "I'm sorry Dave, I'm afraid I can't do that. Shutting down"
        else do
            if isSlotOpen bd slotPos then do 
                updatedBoard <- return (dropInSlot bd slotPos p) 
                gameBoard <- return (boardToStr playerToChar updatedBoard)
                putStrLn(gameBoard)
                game updatedBoard p
            else do 
                putStrLn("Slot not open fren")
                readSlot bd p

    -- Checks for valid input
    getX = do
        line <- getLine
        let parsed = reads line :: [(Int, String)] in 
            if length parsed == 0
            then getX'
            else let (x, _) = head parsed in
            if x > 0 && x < 8 || x == 9000
                then return (x - 1)
                else getX'
            where
            getX' = do
                putStrLn "Invalid Input"
                getX

    playerToChar p 
        | p == 1 = " X "
        | p == 2 = " O "
        | otherwise = " . "

    main = do 
        putStrLn "Welcome to connect four"
        -- Enters a recursive loop until player wins
        game (mkBoard 6 7) 2

    -- Checks for a winning board
    game :: [[Int]] -> Int -> IO()
    game bd p = do 
        if isWonBy bd p then do 
            putStrLn(gameBoard)
            putStrLn("Game Won by Player " ++ "1")
        else do
            playerSwitch bd p

    -- Switches player
    playerSwitch :: [[Int]] -> Int -> IO()
    playerSwitch bd p = do
        if p == 1 then do
            readSlot bd 2
        else do
            readSlot bd 1 
    
