import Data.Char (isDigit)

-- Task 01
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem val (a:ab)
    | val == a = True
    | otherwise = myElem val ab

-- Task 02
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (div a b)

-- Task 03
safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (a:_) 0 = Just a
safeNth (a:ap) i
    | i < 0 = Nothing
    | otherwise = safeNth ap (i - 1)

-- Task 04
safeSucc :: Maybe Int -> Maybe Int
safeSucc Just val = Just $ succ val
safeSucc _ = Nothing

-- Task 05
myLookup :: Eq a => a -> [(a , b)] -> Maybe b
myLookup _ [] = Nothing
myLookup val (a:ab)
    | val == fst a = Just (snd a)
    | otherwise = myLookup val ab

-- Task 06
maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo func (Just a) (Just b) = Just $ func a b
maybeDo _ _ _ = Nothing

-- Task 07
readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':xs:xss)
    | all isDigit (xs:xss) = Just (read (xs:xss))
    | otherwise = Nothing
readInt a
    | all isDigit a = Just (read a :: Int)
    | otherwise = Nothing

-- Task 08
getLineLength :: IO Int
getLineLength = length <$> getLine

-- Task 09
printAndGetLength :: String -> IO Int
printAndGetLength str = putStrLn str >> (return $ length str)

-- Task 10
printBox :: Int -> IO ()
printBox 1 = putStrLn "++"
printBox size
    | size <= 0 = return ()
    | otherwise = putStrLn ('+' : lineBox (size * 2)) >>
    midleBox (size * 2) (size - 2) >>
    putStrLn ('+' : lineBox (size * 2))

lineBox :: Int -> String
lineBox 1 = []
lineBox 2 = ['+']
lineBox size = '-' : lineBox (size - 1)

midleLineBox :: Int -> String
midleLineBox 1 = []
midleLineBox 2 = ['|']
midleLineBox size = ' ' : midleLineBox (size - 1)

midleBox :: Int -> Int -> IO ()
midleBox _ 0 = return ()
midleBox size h = putStrLn ('|' : midleLineBox size) >>
    midleBox size (h - 1)

-- Task 11
recLines :: Int -> IO String -> IO String
recLines 0 a = a
recLines n a = recLines (n - 1) (mappend a getLine)

concatLines :: Int -> IO String
concatLines n
    | n <= 0 = return ""
    | otherwise = recLines (n - 1) getLine

-- Task 12
getInt :: IO (Maybe Int)
getInt = readInt <$> getLine
