size :: Integer
size = 12 + 13

square :: Integer -> Integer
square n = n * n

double :: Integer -> Integer
double n = 2*n

example :: Integer
example = double (size - square (2+2))

dubSquare :: Integer -> Integer
dubSquare x  = square (double x)

squareDub :: Integer -> Integer
squareDub x = double (square x) 
