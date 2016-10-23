% Drop

我们已经学习了trait，本章我们讨论Rust标准库提供的一个特殊的trait：[`Drop`][drop]。
`Drop`的功能是添加一段代码供某个值失效时运行：

[drop]: https://doc.rust-lang.org/std/ops/trait.Drop.html

```rust
struct HasDrop;

impl Drop for HasDrop {
    fn drop(&mut self) {
        println!("Dropping!");
    }
}

fn main() {
    let x = HasDrop;

    // do stuff

} // x goes out of scope here
```

上面`drop()`方法会在`x`超出有效范围时调用。`drop()`是`Drop`包含的唯一方法，并且
其接受的参数是`&mut self`。

`Drop`的核心内容就这么多，是不是很简单？

另外需要提醒你的是多个值一起失效时，`drop()`被调用的顺序与各个值声明的顺序相反：

```rust
struct Firework {
    strength: i32,
}

impl Drop for Firework {
    fn drop(&mut self) {
        println!("BOOM times {}!!!", self.strength);
    }
}

fn main() {
    let firecracker = Firework { strength: 1 };
    let tnt = Firework { strength: 100 };
}
```

以上代码输出：

```text
BOOM times 100!!!
BOOM times 1!!!
```

那么`Drop`有什么用处呢？一般的，`Drop`可用于安全的释放与值相关的某些资源。例如，
[`Arc<T>` 类型][arc]就使用了`Drop`在其实例失效时递减引用计数，如果减小到0则释放
底层资源。

[arc]: https://doc.rust-lang.org/std/sync/struct.Arc.html
