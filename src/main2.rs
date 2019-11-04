use std::fmt;
use std::fmt::{Formatter, Error};
use std::time::SystemTime;

enum E {
    Number(f64),
    Func(Box<dyn FnOnce(Vec<E>) -> Result<E, String>>)
}

impl fmt::Display for E {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            E::Number(n) => write!(f, "{}", n),
            E::Func(_) => write!(f, "function")
        }
    }
}

fn runtime(start: SystemTime, s: Vec<E>) -> Result<E, String> {
    let now = SystemTime::now();
    println!("{}", now.duration_since(start).unwrap().as_micros());
    Ok(s.first().unwrap().into())
}

fn main() {
    let me = E::Number(1.0);
    let now = SystemTime::now();
    let e = E::Func(Box::new(|x| runtime(now, x)));

    match e {
        E::Func(f) => println!("{}", f(vec![me]).unwrap()),
        _ => panic!("Invalid output")
    };
}
