
use self::Exp::{Const, Var, Product, Sum};

enum Exp{
    Const(f32),
    Var(char),
    Product(Box<Exp>, Box<Exp>),
    Sum(Box<Exp>, Box<Exp>),
}

impl Into<Exp> for f32{
    fn into(self) -> Exp{Const(self)}
}

impl Into<Exp> for char{
    fn into(self) -> Exp{Var(self)}
}

impl Exp{
    fn derive(&self, v:char)->Exp{
        match self {
            Const(_)=> Const(0f32),
            Var(c) if v == *c => Const(1f32),
            Var(c) => Var(*c),
            Product(e1,e2) => Product(Box::new(e1.derive(v)), Box::new(e2.derive(v))),
            Sum(e1,e2) => Sum(Box::new(e1.derive(v)), Box::new(e2.derive(v))),
        }
    }
}


use std::fmt;

impl fmt::Display for Exp{
    fn fmt(&self, f:&mut fmt::Formatter<'_>)->fmt::Result{
        match self {
            Const(v)=> write!(f, "{}", v),
            Var(v)=> write!(f, "{}", v),
            Product(e1, e2)=> write!(f, "{}{}", e1, e2),
            Sum(e1,e2)=> write!(f, "{} + {}", e1, e2)
        }
    }
}
#[macro_export]
macro_rules! e {
    ($x:literal) => {
        Const($x)
    };
    ($x:tt) => {
        Var(stringify!($x).chars().nth(0).expect("invalid")) 
    };
    ($x:tt + $y:tt) => {
        Sum(Box::new(e!($x)), Box::new(e!($y)))
    }
}
fn main(){
    let e = e!{3.0 + x};
    println!( "exp: {}, deriv: {}",e, e.derive('x') );
}
