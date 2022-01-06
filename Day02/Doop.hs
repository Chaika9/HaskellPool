import System.Environment ( getArgs )
import System.Directory.Internal.Prelude (exitFailure)
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    args <- getArgs
    case length args /= 3 of
        True -> exitWith (ExitFailure 84)
        False -> return ()
    case isError (myNth args 0) (myNth args 2) of
        True -> exitWith (ExitFailure 84)
        False -> return ()

    let nb1 = getValue args 0
    let nb2 = getValue args 2
    let sign = myNth args 1
    case calc nb1 nb2 sign of
        Just x -> print x
        Nothing -> exitWith (ExitFailure 84)

isError :: String -> String -> Bool
isError x y
    | not (all myIsDigit x) = True
    | not (all myIsDigit y) = True
    | otherwise = False

calc :: Maybe Int -> Maybe Int -> String -> Maybe Int
calc (Just x) (Just y) "+" = Just (x + y)
calc (Just x) (Just y) "-" = Just (x - y)
calc (Just x) (Just y) "*" = Just (x * y)
calc (Just x) (Just y) "/" = myDiv x y
calc (Just x) (Just y) "%" = myMod x y
calc (Just x) (Just y) _ = Nothing

myDiv :: Int -> Int -> Maybe Int
myDiv _ 0 = Nothing
myDiv x y = Just (div x y)

myMod :: Int -> Int -> Maybe Int
myMod _ 0 = Nothing
myMod x y = Just (mod x y)

getValue :: [String] -> Int -> Maybe Int
getValue a i = readInt (myNth a i)

myNth :: [a] -> Int -> a
myNth [] i = error "Empty List"
myNth (a:ap) 0 = a
myNth (a:ap) i
    | i < 0 = error "Invalid Index"
    | i > length (a:ap) = error "Invalid Index"
    | otherwise = myNth ap (i - 1)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt a
    | all myIsDigit a = Just (read a :: Int)
    | otherwise = Nothing

myIsDigit :: Char -> Bool
myIsDigit val = val `elem` "-0123456789"