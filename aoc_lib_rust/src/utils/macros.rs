#[macro_export]
macro_rules! next(
    ($expr:expr, $ty:ty) => (
        $expr.next().unwrap()
    );
);

#[macro_export]
macro_rules! next_parse(
    ($expr:expr, $ty:ty) => (
        $expr.next().unwrap().parse::<$ty>().unwrap()
    );
);
