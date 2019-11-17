--primitive getCh :: IO Char

a :: IO(Char,Char)
a = do  x <- getChar
        getChar
        y <- getChar
        return (x,y)
getLine' :: IO String
getLine' = do   x <- getChar  
                if x == '\n' then
                    return []
                else
                    do  xs <- getLine
                        return (x:xs)   


putStr' :: String -> IO()
putStr' []      = return ()
putStr' (x:xs)  = do putChar x
                     putStr' xs
                    
putStrFoldr :: String -> IO()
putStrFoldr = foldr (\x y -> do putChar x 
                                y ) 
                    (return () )
strlenInteractive :: IO()
strlenInteractive = do  putStr' "Enter a string: "
                        xs <- getLine'
                        putStrFoldr "The string has "
                        putStrFoldr (show (length xs))
                        putStrFoldr " characters\n"



sgetLine :: IO String
sgetLine = do   x <- getChar
                if x == '\n' then
                    do  putChar x
                        return []
                else
                    do  putChar '-'
                        xs <- sgetLine
                        return (x:xs)

diff :: String -> String -> String
diff _ [] = []
diff [] _ = [] 
diff (x:xs) ys = if elem x ys then
                        (x: (diff xs ys))
                     else
                        ('-': (diff xs ys))

guess :: String -> IO()
guess []        = return ()
guess word    = do  ys <- getLine'
                    if ys == word then
                        do  putStrFoldr " you guessed correctly"
                    else
                        do  putStrFoldr (diff word ys)
                            guess word

hangman :: IO() 
hangman = do putStrFoldr "Think of a word:\n"
             word <- sgetLine
             putStrLn "Try to guess it: "
             guess word

main:: IO()
main = do
    print("lecture 9")
    