 /*used rows instead of cols because it was easier to 
print the board
 */

import java.util.Scanner;
import java.util.Random;

public class NQueens2{
    static int size = 8;
    // state
    int[] state = new int[size];  // rows 0 to size - 1
    Random rand = new Random();


    public NQueens2(int s) {
	size = s;
	// initialize board
	for (int i=0; i<size; i++) {
	    state[i] = rand.nextInt(size);  // position of Q in row i
	}
    }

    public void printBoard() {
	for (int row=0; row<size; row++) {
	    for (int col=0; col<size; col++) {
		if (state[row] == col) {
		    System.out.print("Q ");
		} else {
		    System.out.print(". ");
		}
	    }
	    System.out.println("");
	}
	
    }
    
    /**
       Compute the number of pairs that don't conflict
     */
    public int fitness() {
	// fill in
	return 0;
    }

    /**
       Each queen is specified by its row, e.g. queen 0 is in row 0
       The method countDiagonal returns a count of the number of pairs of queens
       along a diagonal that attack each other.
     */
    public int countDiagonal() {
		int count = 0;
		for (int i =0; i<size; i++) {
			for (int j=i+1; j<size; j++) {
				if (state[i] == state[j]) {
					count++;
				} 
			}
		}
		return count;
    }



    /**
       Each queen is specified by its row, e.g. queen 0 is in row 0
       The method countCol(i) returns a count of the number of queens
       in the same column as the queen in row i.
       This is just the condition that state[j] == state[i]
     */
    public int countCol() {
	int count = 0;
	for (int i=0; i<size; i++) {
	    for (int j=i+1; j<size; j++) {
		if (state[i] == state[j]) {
		    count++;
		}
	    }
	}
	return count;
    }


    /**
       Compute the number of pairs of queens that are attacking each other
     */
    public int cost() {
	// fill in
	
	return 0;
    }

    /**
       Neighborhood of a state is all states that differ in exactly one row
       for local search.
     */



    public static void main(String[] args) {
	NQueens2 board = new NQueens2(size);
	board.printBoard();
	System.out.println("number of pairs of attacking Queens via cols: " + board.countCol());
	System.out.println("number of pairs of attacking Queens via diags: " + board.countDiagonal());


	/*
	 Random rand = new Random();
	 for (int i=0; i<10; i++) {
	     System.out.println(rand.nextInt(5) + 1);
	 }

	 /*
        String msg;
        Scanner scan = new Scanner(System.in);
        System.out.println("Enter a line of text");
        msg = scan.nextLine();
        System.out.println(msg);
	 */
     }
  }
