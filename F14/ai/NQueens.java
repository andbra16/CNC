import java.util.*;

public class NQueens {

    public int[] queenPlaceInRow;
    int size;

    /**
     * 
     */
    public NQueens(int size) {
        queenPlaceInRow = new int[size];
        this.size = size;
        for (int i = 0; i < size; i++) {
            queenPlaceInRow[i] = Integer.MAX_VALUE;
        }
    }

    public static void main(String[] args) {
		Scanner scanner = new Scanner(System.in);
		System.out.println("Enter the size of the board.");
		int size = Integer.parseInt(scanner.nextLine());
        NQueens nQueens = new NQueens(size);
        nQueens.placeQueen(0);
    }

    
    private void placeQueen(int row) {
  		//placed all queens
        if (row == this.size) {
            printBoard();
            return;
        }
		//place queen
        for (int i = 0; i < this.size; i++) {
            queenPlaceInRow[row] = i;
			//check if queens conflict 
            if (check(row)) {
                placeQueen(row + 1);
            }
        }

    }

    private boolean check(int row) {
        for (int i = 0; i < row; i++) {
            int diffX = Math.abs(queenPlaceInRow[row] - queenPlaceInRow[i]);
            int diffY = row - i;
            if (diffX == 0 || diffX == diffY) {
                return false;
            }
        }
        return true;
    }

    /**
     * 
     */
    private void printBoard() {
        System.out.println("------------------------");
        for (int i = 0; i < this.size; i++) {
            for (int j = 0; j < this.size; j++) {
                if (queenPlaceInRow[i] == j) {
                    System.out.print(" Q ");
                } else {
                    System.out.print(" . ");
                }

            }
            System.out.println();

        }
        System.out.println("------------------------");

    }
}
