% Traits

trait类似于Java中的interface，它用于告诉Rust编译器一个类型须实现哪些功能。

使用trait时首先需要声明一个trait，其内部是方法声明。然后我们就可以为某个类型添加
trait支持了。下面的例子中，我们为`Circle`实现了`HasArea` trait。

```rust
struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

trait HasArea {
    fn area(&self) -> f64;
}

impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}
```

如你所见，`trait`的定义结构与`impl`很类似，区别是没有函数体的定义，只有函数原型
声明。此外在实现某个trait时，我们使用`impl Trait for Item`这样的语法。

`Self`可用于在trait的函数声明中表示符合本trait的类型，可根据需要使用`Self`、
`&Self`和`&mut Self`中的任意一种。

```rust
struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

trait HasArea {
    fn area(&self) -> f64;

    fn is_larger(&self, &Self) -> bool;
}

impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }

    fn is_larger(&self, other: &Self) -> bool {
        self.area() > other.area()
    }
}
```

## 泛型参数的trait限制

trait的用处主要在于允许对类型的行为做出假设。泛型函数可以利用这一点对其泛型参数
进行限制。首先考虑下面的例子：

```rust
fn print_area<T>(shape: T) {
    println!("This shape has an area of {}", shape.area());
}
```

以上代码编译会报错：

```text
error: no method named `area` found for type `T` in the current scope
```

上面的代码出错的原因是泛型参数`T`可以是任意类型，因此我们无法确定其是否一定实现
了`area`方法。我们可以通过为泛型参数`T`添加类型限定达到该目的：

```rust
# trait HasArea {
#     fn area(&self) -> f64;
# }
fn print_area<T: HasArea>(shape: T) {
    println!("This shape has an area of {}", shape.area());
}
```

`<T: HasArea>`表示`T`是任何实现了`HasArea` trait的类型。因为`HasArea`定义了方法原型，
`area()`方法必定是`T`类型支持的。

以下是扩展示例：

```rust
trait HasArea {
    fn area(&self) -> f64;
}

struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}

struct Square {
    x: f64,
    y: f64,
    side: f64,
}

impl HasArea for Square {
    fn area(&self) -> f64 {
        self.side * self.side
    }
}

fn print_area<T: HasArea>(shape: T) {
    println!("This shape has an area of {}", shape.area());
}

fn main() {
    let c = Circle {
        x: 0.0f64,
        y: 0.0f64,
        radius: 1.0f64,
    };

    let s = Square {
        x: 0.0f64,
        y: 0.0f64,
        side: 1.0f64,
    };

    print_area(c);
    print_area(s);
}
```

以上代码输出：

```text
This shape has an area of 3.141593
This shape has an area of 1
```

正如你所见，`print_area`对函数参数做了类型限制，因此当我们传入一个错误参数时，
Rust会报错如下：

```rust
print_area(5);
```

```text
error: the trait bound `_ : HasArea` is not satisfied [E0277]
```

## 泛型结构体的trait限制

Your generic structs can also benefit from trait bounds. All you need to
do is append the bound when you declare type parameters. Here is a new
type `Rectangle<T>` and its operation `is_square()`:

```rust
struct Rectangle<T> {
    x: T,
    y: T,
    width: T,
    height: T,
}

impl<T: PartialEq> Rectangle<T> {
    fn is_square(&self) -> bool {
        self.width == self.height
    }
}

fn main() {
    let mut r = Rectangle {
        x: 0,
        y: 0,
        width: 47,
        height: 47,
    };

    assert!(r.is_square());

    r.height = 42;
    assert!(!r.is_square());
}
```

`is_square()` needs to check that the sides are equal, so the sides must be of
a type that implements the [`core::cmp::PartialEq`][PartialEq] trait:

```rust
impl<T: PartialEq> Rectangle<T> { ... }
```

Now, a rectangle can be defined in terms of any type that can be compared for
equality.

[PartialEq]: ../core/cmp/trait.PartialEq.html

Here we defined a new struct `Rectangle` that accepts numbers of any
precision—really, objects of pretty much any type—as long as they can be
compared for equality. Could we do the same for our `HasArea` structs, `Square`
and `Circle`? Yes, but they need multiplication, and to work with that we need
to know more about [operator traits][operators-and-overloading].

[operators-and-overloading]: 操作符重载.md

# Rules for implementing traits

So far, we’ve only added trait implementations to structs, but you can
implement a trait for any type. So technically, we _could_ implement `HasArea`
for `i32`:

```rust
trait HasArea {
    fn area(&self) -> f64;
}

impl HasArea for i32 {
    fn area(&self) -> f64 {
        println!("this is silly");

        *self as f64
    }
}

5.area();
```

It is considered poor style to implement methods on such primitive types, even
though it is possible.

This may seem like the Wild West, but there are two restrictions around
implementing traits that prevent this from getting out of hand. The first is
that if the trait isn’t defined in your scope, it doesn’t apply. Here’s an
example: the standard library provides a [`Write`][write] trait which adds
extra functionality to `File`s, for doing file I/O. By default, a `File`
won’t have its methods:

[write]: https://doc.rust-lang.org/std/io/trait.Write.html

```rust
let mut f = std::fs::File::create("foo.txt").expect("Couldn’t create foo.txt");
let buf = b"whatever"; // byte string literal. buf: &[u8; 8]
let result = f.write(buf);
# result.unwrap(); // ignore the error
```

Here’s the error:

```text
error: type `std::fs::File` does not implement any method in scope named `write`
let result = f.write(buf);
               ^~~~~~~~~~
```

We need to `use` the `Write` trait first:

```rust
use std::io::Write;

let mut f = std::fs::File::create("foo.txt").expect("Couldn’t create foo.txt");
let buf = b"whatever";
let result = f.write(buf);
# result.unwrap(); // ignore the error
```

This will compile without error.

This means that even if someone does something bad like add methods to `i32`,
it won’t affect you, unless you `use` that trait.

There’s one more restriction on implementing traits: either the trait
or the type you’re implementing it for must be defined by you. Or more
precisely, one of them must be defined in the same crate as the `impl`
you're writing. For more on Rust's module and package system, see the
chapter on [crates and modules][cm].

So, we could implement the `HasArea` type for `i32`, because we defined
`HasArea` in our code. But if we tried to implement `ToString`, a trait
provided by Rust, for `i32`, we could not, because neither the trait nor
the type are defined in our crate.

One last thing about traits: generic functions with a trait bound use
‘monomorphization’ (mono: one, morph: form), so they are statically dispatched.
What’s that mean? Check out the chapter on [trait objects][to] for more details.

[cm]: crates-and-modules.md
[to]: trait-objects.md

# Multiple trait bounds

You’ve seen that you can bound a generic type parameter with a trait:

```rust
fn foo<T: Clone>(x: T) {
    x.clone();
}
```

If you need more than one bound, you can use `+`:

```rust
use std::fmt::Debug;

fn foo<T: Clone + Debug>(x: T) {
    x.clone();
    println!("{:?}", x);
}
```

`T` now needs to be both `Clone` as well as `Debug`.

# Where clause

Writing functions with only a few generic types and a small number of trait
bounds isn’t too bad, but as the number increases, the syntax gets increasingly
awkward:

```rust
use std::fmt::Debug;

fn foo<T: Clone, K: Clone + Debug>(x: T, y: K) {
    x.clone();
    y.clone();
    println!("{:?}", y);
}
```

The name of the function is on the far left, and the parameter list is on the
far right. The bounds are getting in the way.

Rust has a solution, and it’s called a ‘`where` clause’:

```rust
use std::fmt::Debug;

fn foo<T: Clone, K: Clone + Debug>(x: T, y: K) {
    x.clone();
    y.clone();
    println!("{:?}", y);
}

fn bar<T, K>(x: T, y: K) where T: Clone, K: Clone + Debug {
    x.clone();
    y.clone();
    println!("{:?}", y);
}

fn main() {
    foo("Hello", "world");
    bar("Hello", "world");
}
```

`foo()` uses the syntax we showed earlier, and `bar()` uses a `where` clause.
All you need to do is leave off the bounds when defining your type parameters,
and then add `where` after the parameter list. For longer lists, whitespace can
be added:

```rust
use std::fmt::Debug;

fn bar<T, K>(x: T, y: K)
    where T: Clone,
          K: Clone + Debug {

    x.clone();
    y.clone();
    println!("{:?}", y);
}
```

This flexibility can add clarity in complex situations.

`where` is also more powerful than the simpler syntax. For example:

```rust
trait ConvertTo<Output> {
    fn convert(&self) -> Output;
}

impl ConvertTo<i64> for i32 {
    fn convert(&self) -> i64 { *self as i64 }
}

// can be called with T == i32
fn normal<T: ConvertTo<i64>>(x: &T) -> i64 {
    x.convert()
}

// can be called with T == i64
fn inverse<T>(x: i32) -> T
        // this is using ConvertTo as if it were "ConvertTo<i64>"
        where i32: ConvertTo<T> {
    x.convert()
}
```

This shows off the additional feature of `where` clauses: they allow bounds
on the left-hand side not only of type parameters `T`, but also of types (`i32` in this case). In this example, `i32` must implement
`ConvertTo<T>`. Rather than defining what `i32` is (since that's obvious), the
`where` clause here constrains `T`.

# Default methods

A default method can be added to a trait definition if it is already known how a typical implementor will define a method. For example, `is_invalid()` is defined as the opposite of `is_valid()`:

```rust
trait Foo {
    fn is_valid(&self) -> bool;

    fn is_invalid(&self) -> bool { !self.is_valid() }
}
```

Implementors of the `Foo` trait need to implement `is_valid()` but not `is_invalid()` due to the added default behavior. This default behavior can still be overridden as in:

```rust
# trait Foo {
#     fn is_valid(&self) -> bool;
#
#     fn is_invalid(&self) -> bool { !self.is_valid() }
# }
struct UseDefault;

impl Foo for UseDefault {
    fn is_valid(&self) -> bool {
        println!("Called UseDefault.is_valid.");
        true
    }
}

struct OverrideDefault;

impl Foo for OverrideDefault {
    fn is_valid(&self) -> bool {
        println!("Called OverrideDefault.is_valid.");
        true
    }

    fn is_invalid(&self) -> bool {
        println!("Called OverrideDefault.is_invalid!");
        true // overrides the expected value of is_invalid()
    }
}

let default = UseDefault;
assert!(!default.is_invalid()); // prints "Called UseDefault.is_valid."

let over = OverrideDefault;
assert!(over.is_invalid()); // prints "Called OverrideDefault.is_invalid!"
```

# Inheritance

Sometimes, implementing a trait requires implementing another trait:

```rust
trait Foo {
    fn foo(&self);
}

trait FooBar : Foo {
    fn foobar(&self);
}
```

Implementors of `FooBar` must also implement `Foo`, like this:

```rust
# trait Foo {
#     fn foo(&self);
# }
# trait FooBar : Foo {
#     fn foobar(&self);
# }
struct Baz;

impl Foo for Baz {
    fn foo(&self) { println!("foo"); }
}

impl FooBar for Baz {
    fn foobar(&self) { println!("foobar"); }
}
```

If we forget to implement `Foo`, Rust will tell us:

```text
error: the trait bound `main::Baz : main::Foo` is not satisfied [E0277]
```

# Deriving

Implementing traits like `Debug` and `Default` repeatedly can become
quite tedious. For that reason, Rust provides an [attribute][attributes] that
allows you to let Rust automatically implement traits for you:

```rust
#[derive(Debug)]
struct Foo;

fn main() {
    println!("{:?}", Foo);
}
```

[attributes]: attributes.md

However, deriving is limited to a certain set of traits:

- [`Clone`](../core/clone/trait.Clone.html)
- [`Copy`](../core/marker/trait.Copy.html)
- [`Debug`](../core/fmt/trait.Debug.html)
- [`Default`](../core/default/trait.Default.html)
- [`Eq`](../core/cmp/trait.Eq.html)
- [`Hash`](../core/hash/trait.Hash.html)
- [`Ord`](../core/cmp/trait.Ord.html)
- [`PartialEq`](../core/cmp/trait.PartialEq.html)
- [`PartialOrd`](../core/cmp/trait.PartialOrd.html)
