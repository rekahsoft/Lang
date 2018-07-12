fn square (x : i32) -> i32 {
    x * x
}

fn factorial (x : isize) -> isize {
    if x <= 0 {
        1
    } else {
        x * factorial(x - 1)
    }
}

fn main () {
    for x in 0..100 {
        println!("The factorial of {} is {}.", x, factorial(x));
    }
}
