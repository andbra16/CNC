package main

import "fmt"

func collatz(n int) int {
	count := -1

	for {
		count++
		if n == 1 {
			break
		} else if n%2 == 0 {
			n /= 2
		} else {
			n *= 3
			n++
		}
	}
	return count
}

func collatzToOne(n int) {
	x := 1
	for {
		if n == collatz(x) {
			fmt.Println("Sequence:", x);
			break;
		}
		x = x + 1;
	}
}

func main() {
	fmt.Println(collatz(6))
	collatzToOne(45)
}


