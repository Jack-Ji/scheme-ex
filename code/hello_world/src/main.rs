fn main() {
    println!("Hello, world!");

    trait Foo {
        fn print(&self);
    }

    struct Bar {
        x : i32
    }

    impl Foo for Bar {
        fn print(&self) {
            println!("x: {}", self.x)
        }
    }

    fn printt(x: Foo) {
        x.print()
    }

    let x = Bar{x: 33};
    printt(x);
}
