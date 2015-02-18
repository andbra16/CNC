data Bit = B1 | B0
	deriving (Eq, Show)

bitList1 = [B1,B0,B1,B1]

bin2int :: [Bit] -> Int
bin2int [] = 0
bin2int [x] 
	| x == B1 = 1
	| otherwise = 0
bin2int xs
	| head xs == B1 = 2^(length xs - 1) + bin2int (tail xs)
	| otherwise = bin2int (tail xs)

int2bin :: Int -> [Bit]
int2bin 0 = [B0]
int2bin 1 = [B1]
int2bin x 
	| x `mod` 2 == 0 = B0 : reverse (int2bin (x `div` 2))
	| otherwise = B1 : reverse (int2bin (x `div` 2))

bin2int' = foldr func 0
	where 
		func x acc
			| x == B1 = 2^(acc) + acc
			| otherwise = acc 
			
