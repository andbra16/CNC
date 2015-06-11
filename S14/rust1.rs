fn main() {
    //println!("Hello, world!");
    //println!("{}" , ifStatement(0.0, "salad"));
 	//println!("{}" , is_four(5));
	//matchPattern(3);  
	//looper();
	structEx(); 
}

fn ifStatement(mut price: f32, item: &str) -> f32 {
    price =
    if item == "salad" {
        3.50
    } else if item == "muffin" {
        2.25
    } else {
        2.00
    };
    return price;
}

fn is_four(x: int) -> bool {
   // No need for a return statement. The result of the expression
   // is used as the return value.
   x == 4
}

fn matchPattern(x: int) {
    match x {
        0     => println!("zero"),
        1 | 2 => println!("one or two"),
        3 => println!("three"),
        _     => println!("something else")
    }
}

fn looper() {
    let mut x = 5u;
    loop {
        x += x - 3;
        if x % 5 == 0 { break; }
        println!("{}", x);
    }
}

fn structEx() {
    // the struct owns the objects contained in the `x` and `y` fields
    struct Foo { x: int, y: int }

    {
    // `a` is the owner of the struct, and thus the owner of the struct's fields
        let a = Foo { x: 5, y: 10 };
        println!("{}", a.x);
    }
    // when `a` goes out of scope, the destructor for the `~int` in the struct's
    // field is called

    // `b` is mutable, and the mutability is inherited by the objects it owns
    let mut b = Foo { x: 5, y: 10 };
    b.x = 10;
    
    println!("{}", b.x);
}


