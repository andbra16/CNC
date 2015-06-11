fn main() {
    //patternmatch();
    //rprime_sum(1,2,3);
    //fizzbuzz();
    //ifStatment();
    let i = 6;
    println!("{} has {} Collatz steps", i, collatz(i));
    
    
    //collatz final excerise
    let mut x = 1;
	loop {
	     if i==collatz(x){
	     	println!("Sequence: {}",x);
	     	break;
	     }
	     x=x+1;
	}
    
}

fn patternmatch() {
    let x = 4i32;
    match x {
	    0 => {println!("It's 0")} // Do nothing
	    4 => {println!("It's 4")} 
	    _ => {println!("Not 0 or 4")} // Matches every integer value
    }
}

fn rprime_sum(x: int, y: int, m: int) {
	match (x+y)%m {
		0 => println!("Multiple"),
		_ => println!("Relatively prime")
	}
}

fn collatz(n: int) -> int {
	if n == 1 { return 0; }
	match n % 2 {
		0 => { 1 + collatz(n/2) }
		_ => { 1 + collatz(n*3+1) }
	}
}

fn fizzbuzz() {
    for i in range(1,101){
        if i%15==0 {
       	    println!("FizzBuzz");
	    }
	    else if i%3==0 {
	        println!("Fizz");
	    }
	    else if i%5==0 {
	        println!("Buzz");
	    }
	   else {
	        println!("{}", i);
	    }
   }
}


fn ifStatement() {
    let x = 5;
    let foo = if x == 5 {
                "five"
              }
              else if x == 6 {
                "six"
              }
              else {
                "neither"
              }
    println!("{}", foo);
} 
