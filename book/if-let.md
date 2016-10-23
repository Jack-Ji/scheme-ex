% if let

前面提到过，`if let`是match的简化版，用它我们可以方便的对变量进行模式匹配，如果
匹配成功则与其关联的代码块会被执行。首先看下面使用match的例子：

```rust
# let option = Some(5);
# fn foo(x: i32) { }
match option {
    Some(x) => { foo(x) },
    None => {},
}
```

我们可以通过方法调用判断`option`的类型：

```rust
# let option = Some(5);
# fn foo(x: i32) { }
if option.is_some() {
    let x = option.unwrap();
    foo(x);
}
```

显然这过于依赖类型本身，更好的方法是使用`if let`，代码如下：

```rust
# let option = Some(5);
# fn foo(x: i32) { }
if let Some(x) = option {
    foo(x);
}
```

和match一样，`if let`同样会创建新的变量绑定。此外当匹配不成功时也可通过`else`进
一步处理：

```rust
# let option = Some(5);
# fn foo(x: i32) { }
# fn bar() { }
if let Some(x) = option {
    foo(x);
} else {
    bar();
}
```

## `while let`

`while let`创建了一个循环，该循环的条件是值与模式相匹配。首先我们看使用`for`
和`match`的版本：

```rust
let mut v = vec![1, 3, 5, 7, 11];
loop {
    match v.pop() {
        Some(x) =>  println!("{}", x),
        None => break,
    }
}
```

然后我们再看更为简洁的`while let`版本：

```rust
let mut v = vec![1, 3, 5, 7, 11];
while let Some(x) = v.pop() {
    println!("{}", x);
}
```

[patterns]: 模式.md
