def setup(n):
	board = []
	for i in range(n):
		board.append(["X"] * n)
	return board

def printBoard(board):
	for row in range(len(board)):
		for column in range(len(board)):
			print board[column]
		print

def addQueens(board,x,y):
	board[x][y] = "Q"

def checkRow(board,x,y):
	for i in range(len(board)):
		print board[x]


board = setup(8)
addQueens(board,0,1)
addQueens(board,1,2)
printBoard(board)
#checkRow(board,0,1)
