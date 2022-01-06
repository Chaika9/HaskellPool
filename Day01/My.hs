-- Task 01
mySucc :: Int -> Int
mySucc x = x + 1

-- Task 02
myIsNeg :: Int -> Bool
myIsNeg x = x < 0

--Task 03
myAbs :: Int -> Int
myAbs x
    | x < 0 = (-x)
    | otherwise = x

--Task 04
myMin :: Int -> Int -> Int
myMin x y
    | x < y = x
    | otherwise = y

--Task 05
myMax :: Int -> Int -> Int
myMax x y
    | x > y = x
    | otherwise = y

--Task 06
myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

--Task 07
myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

-- Task 08
myFst :: (a , b) -> a
myFst (a , b) = a

-- Task 09
mySnd :: (a , b) -> b
mySnd (a , b) = b

-- Task 10
mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

-- Task 11
myHead :: [a] -> a
myHead [] = error "Empty List"
myHead (a:ap) = a

-- Task 12
myTail :: [a] -> [a]
myTail [] = error "Empty List"
myTail (a:ap) = ap

-- Task 13
myLength :: [a] -> Int
myLength [] = 0
myLength (a:ap) = 1 + myLength ap

-- Task 14
myNth :: [a] -> Int -> a
myNth [] i = error "Empty List"
myNth (a:ap) 0 = a
myNth (a:ap) i
    | i < 0 = error "Invalid Index"
    | i > myLength (a:ap) = error "Invalid Index"
    | otherwise = myNth ap (i - 1)

-- Task 15
myTake :: Int -> [a] -> [a]
myTake i [] = []
myTake 0 _ = []
myTake i (a:ap)
    | i <= 0 = []
    | otherwise = a : myTake (i - 1) ap

-- Task 16
myDrop :: Int -> [a] -> [a]
myDrop i [] =  []
myDrop 0 a =  a
myDrop i (a:ap)
    | i < 0 = a:ap
    | otherwise = myDrop (i - 1) ap

-- Task 17
myAppend :: [a] -> [a] -> [a]
myAppend (a:ap) b = a : myAppend ap b
myAppend [] a = a

-- Task 18
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:ap) = myAppend (myReverse ap) [a]

-- Task 19
myInit :: [a] -> [a]
myInit [] = error "Empty List"
myInit a = myReverse (myTail (myReverse a))

-- Task 20
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast (a:ap)
    | myLength ap == 0 = a
    | otherwise = myHead (myReverse ap)

-- Task 21
myZip :: [a] -> [b] -> [(a, b)]
myZip [] b = []
myZip a [] = []
myZip (a:ap) (b:y) = (a, b) : myZip ap y

-- Task 22
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip xs = (myMap myFst xs, myMap mySnd xs)

-- Task 23
myMap :: (a -> b) -> [a] -> [b]
myMap func [] = []
myMap func (a:ap) = func a : myMap func ap

-- Task 24
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter func [] = []
myFilter func (a:ap)
    | func a = a : myFilter func ap
    | otherwise = myFilter func ap

-- Task 25
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl func val [] = val
myFoldl func val (a:ap) = myFoldl func (func val a) ap

-- Task 26
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr func val [] = val
myFoldr func val a = myFoldr func (func (myLast a) val) (myInit a)

myInvFilter :: (a -> Bool) -> [a] -> [a]
myInvFilter func [] = []
myInvFilter func (a:ap)
    | func a = myInvFilter func ap
    | otherwise = a : myInvFilter func ap

-- Task 27
myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition func a = (myFilter func a, myInvFilter func a)

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort func [] = []
myQuickSort func (a:ap) = myAppend b (myAppend [a] s)
    where
        s = myQuickSort func (myFilter (func a) ap)
        b = myQuickSort func (myInvFilter (func a) ap)