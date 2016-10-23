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

泛型结构体同样可使用trait对类型进行限定。下面是示例：

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

由于方法`is_square()`需要对两个`T`类型变量进行比较，类型`T`必须实现了
`core::cmp::PartialEq` trait。

```rust
impl<T: PartialEq> Rectangle<T> { ... }
```

因此，上面定义的`Rectangle`可由任何能够进行比较的类型构成。

上面的`Circle`也可以用同样的方式实现成泛型结构体，这回其成员需要支持乘法操作，
关于操作符的支持参看后续的[操作符traits][operators-and-overloading]。

[operators-and-overloading]: 操作符重载.md

# 实现traits的准则

上面我们主要为struct添加了trait支持，实际上你可以为任意类型添加trait支持，比如
`i32`类型：

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

尽管可以这样做，我们并不推荐为基础类型添加新方法，因为这可能会使代码相当晦涩难懂。

Rust对trait的使用施加了两条限制，以避免代码失控。首先，你准备使用的trait的定义必
须在当前范围可见。以下示例代码中的`File`类型支持[`Write`][write] trait，然而在没
有将`Write`的定义引入当前范围的情况下编译不会成功：

[write]: https://doc.rust-lang.org/std/io/trait.Write.html

```rust
let mut f = std::fs::File::create("foo.txt").expect("Couldn’t create foo.txt");
let buf = b"whatever"; // byte string literal. buf: &[u8; 8]
let result = f.write(buf);
# result.unwrap(); // ignore the error
```

编译错误信息如下：

```text
error: type `std::fs::File` does not implement any method in scope named `write`
let result = f.write(buf);
               ^~~~~~~~~~
```

因此我们需要通过`use`加载`Write` trait的定义：

```rust
use std::io::Write;

let mut f = std::fs::File::create("foo.txt").expect("Couldn’t create foo.txt");
let buf = b"whatever";
let result = f.write(buf);
# result.unwrap(); // ignore the error
```

这下就没问题了。

这意味着即便某个人故意为基本类型`i32`添加了一些方法，只要你没有`use`那些trait，
你就不会受到影响。

Rust对trait的使用施加的最后一条限制是类型和trait二者至少有一个要属于当前crate，
否则不允许为类型添加trait实现。关于crate可参见后续的[crates and modules章节][cm]。

因此，我们可以为`i32`添加`HasArea`支持，因为`HasArea`是我们自己定义的。然而如果
我们想为`i32`添加`ToString`的支持，Rust会禁止这样做，因为无论`i32`还是`ToString`
都不是我们自己定义的。

最后一件需要了解的关于trait的事实是：添加了trait限制的泛型函数是静态派发的，这一
点在后面的[trait objects][to]中会详细解释。

[cm]: crates-and-modules.md
[to]: trait-objects.md

# 多trait限制

之前介绍过如何为泛型参数添加trait限制：

```rust
fn foo<T: Clone>(x: T) {
    x.clone();
}
```

如果你希望添加多个trait限制，可以使用`+`：

```rust
use std::fmt::Debug;

fn foo<T: Clone + Debug>(x: T) {
    x.clone();
    println!("{:?}", x);
}
```

上面代码中的类型`T`需要同时支持`Clone`和`Debug`。

# `where`字句

随着泛型参数的增加，为参数添加trait限定会导致整个函数原型的可读性大幅下降：

```rust
use std::fmt::Debug;

fn foo<T: Clone, K: Clone + Debug>(x: T, y: K) {
    x.clone();
    y.clone();
    println!("{:?}", y);
}
```

这是因为泛型参数列表过长，导致函数名和函数参数相隔得太远了。为解决这个问题，Rust
引入了`where`字句：

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

如上所示，我们可以将泛型参数的类型限定放在函数参数列表后面的`where`字句中，必要
时也可以添加换行，这样一来函数原型的可读性就大大改善了：

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

`where`字句不止可以用于限制泛型参数，还可以用于限制数据类型，例如：

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

上例中，函数`inverse()`通过`where`字句限制了i32必须实现`ConvertTo<T>`。

# 默认方法

在定义trait时可为方法提供默认实现，例如：

```rust
trait Foo {
    fn is_valid(&self) -> bool;

    fn is_invalid(&self) -> bool { !self.is_valid() }
}
```

实现该trait时一般只需实现`is_valid()`就足够了，当然你仍然可以自己实现`is_invalid()`。
下面的例子只实现了`is_invalid()`：

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

下面的例子则实现了所有方法：

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

# 继承

trait之间是可以继承：

```rust
trait Foo {
    fn foo(&self);
}

trait FooBar : Foo {
    fn foobar(&self);
}
```

`FooBar`继承了`Foo`，因此`FooBar`的实现者必须实现`Foo`中的方法：

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

如果我们忘记了实现`Foo`中的接口，Rust会告诉我们：

```text
error: the trait bound `main::Baz : main::Foo` is not satisfied [E0277]
```

# trait自动实现

有些trait不光经常用到，其实现通常也有其固定的规律，Rust为这些trait提供了自动实现
的手段：[属性][attributes]：

```rust
#[derive(Debug)]
struct Foo;

fn main() {
    println!("{:?}", Foo);
}
```

[attributes]: attributes.md

可被自动实现的trait如下：

- `Clone`
- `Copy`
- `Debug`
- `Default`
- `Eq`
- `Hash`
- `Ord`
- `PartialEq`
- `PartialOrd`
