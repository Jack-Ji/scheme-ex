% Match

当判断条件的分支过多时，[`if`][if]/`else`语句就是显得有些力不从心了，此外判断条
件本身也可能会更加复杂化，这时我们可以使用`match`语句：

```rust
let x = 5;

match x {
    1 => println!("one"),
    2 => println!("two"),
    3 => println!("three"),
    4 => println!("four"),
    5 => println!("five"),
    _ => println!("something else"),
}
```

[if]: 条件判断.md

`match`接受一个表达式，根据表达式返回的值可创建多个分支，分支遵循`val => expression`
这样的写法。当某个分支的值能够与match接受的表达式返回的值相匹配时，与其关联的表达
式会被执行，执行得到的值就是整个match语句的值。

[patterns]: 模式.md

`match`所具备的其中一个很独特的优势是“完整匹配检查”，加入我们将上面的match语句的
最后一个分支去掉，编译器会报错如下：

```text
error: non-exhaustive patterns: `_` not covered
```

Rust通过该错误信息告知我们匹配分支是不完整的，因为整数`x`除了值1、2、3、4、5以外
还可能是很多其它值，而最后的`_`分支表示默认分支，它会匹配所有前面未被匹配到的值。

如前所述，`match`同样是个表达式，它可在任何表达式能出现的地方被使用：

```rust
let x = 5;

let number = match x {
    1 => "one",
    2 => "two",
    3 => "three",
    4 => "four",
    5 => "five",
    _ => "something else",
};
```

使用`match`可以很方便的对某个数据进行类型转换，上面的代码就将整数转换为了`String`。

# 枚举变量的匹配

match语句经常被用于对枚举变量进行判断匹配：

```rust
enum Message {
    Quit,
    ChangeColor(i32, i32, i32),
    Move { x: i32, y: i32 },
    Write(String),
}

fn quit() { /* ... */ }
fn change_color(r: i32, g: i32, b: i32) { /* ... */ }
fn move_cursor(x: i32, y: i32) { /* ... */ }

fn process_message(msg: Message) {
    match msg {
        Message::Quit => quit(),
        Message::ChangeColor(r, g, b) => change_color(r, g, b),
        Message::Move { x: x, y: y } => move_cursor(x, y),
        Message::Write(s) => println!("{}", s),
    };
}
```

如上面提到的，Rust会确保匹配是完整的，因此你需要根据枚举的定义给出所有的匹配分支，
当然你也可以只匹配感兴趣的枚举类型并提供`_`作为默认匹配分支。

与之前的简单匹配代码不同，以上对枚举变量进行匹配的代码是无法使用`if`语句完成的，
你可以使用后面将会介绍的[`if let`][if-let]语句对枚举进行匹配，它相当于简化的
`match`语句。

[if-let]: if-let.md
