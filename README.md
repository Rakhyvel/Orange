
<div align="center">
    <a href="#"><img src="docs/budi.png" alt="Budi the Orangutan!" width="30%"></a>
    <h6><em>For When Life Gives You Oranges...</em></h6>
    <h1>Orange Programming Language</h1>
</div>

> **⚠️WARNING**! Orange is still a work in progress! Expect exciting changes and improvements.

## 🍊 What is Orange?

Orange is a versatile systems programming language I've been developing that gives developers control without sacrificing expressiveness. It is designed to be both lightweight and simple, making it a great choice for enthusiasts and professionals alike.

* Visit [the website (coming soon)](http://ornglang.org) to learn more about Orange.
* Tutorials can be found [here (coming soon)](http://ornglang.org/tutorials).
* Documentation can be found [here (coming soon)](http://ornglang.org/docs).

## Quick Start
```sh
# Orange compiler requires Zig 0.15.1
git clone --recursive https://github.com/Rakhyvel/Orange.git

# Build Orange
cd Orange
zig build orng
```

A fancy hello-world example:
```rs
fn main() with core.System {
    @print("Enter your name here: ")

    let name_buf = core.String_Buffer.new()
    defer name_buf.dispose()
    stdin.readln(&name_buf)

    greet(name_buf.str())
}

fn greet(recipient: String) with .. {
    @println("Hello, {recipient}")
}
```

Run it with:
```sh
orng run
```

## Standout Features
Orange comes with a wide range of features that make it a powerful and flexible programming language, including:

### Implicit User-Defined Contexts
This is probably Orange's largest standout feature. Orange allows the user to define contexts that can be passed around implicitly. This is how side-effects like allocation, IO, file operations, and networking are tracked in Orange.

```rs
context Allocating {
    alloc: &mut dyn Allocator
}

// Declare that a function uses a context
fn main() with Allocating {
    // A caller function must be able to provide a callee's context
    let x = alloc_something()
    x^ = 100
}

// Contexts can be inferred
fn alloc_something() -> &mut Int with .. {
    let retval: &mut Int = Allocating.alloc.new[Int]()
    retval
}
```

### Algebraic Data Types & Pattern Matching
Algebraic Data Types (ADTs) allow you to define types that can be one of several variants with zero runtime overhead. Pattern matching in Orange lets you elagantly deconstruct those ADTs with a single, readable expression.

```rs
enum Expr {
    num(Int)
    add(&Expr, &Expr)
    mul(&Expr, &Expr)
}

fn eval(e: Expr) -> Int {
    match e {
        Expr.num(n)         => n
        Expr.add(lhs, rhs)  => eval(lhs) + eval(rhs)
        Expr.mul(lhs, rhs)  => eval(lhs) * eval(rhs)
    }
}
```

[More examples](https://github.com/Rakhyvel/Orange/blob/main/tests/integration/pattern)

### Generics & Traits
Geerics and traits offer a flexible way to write code for any type.

```rs
trait Ord {
    fn lt(self, other: Self) -> Bool
}

fn max[T: Ord](a: T, b: T) -> T {
    if a.lt(b) { b } else { a }
}

impl Ord for Int {
    fn lt(self, other: Int) -> Bool { self < other }
}

fn main() with core.System {
    let x = max(10, 20)
    @println("max is {x}")
}
```

### Seamless C Interoperability
Compile to C and parse C header files with ease. Orange bridges the gap between low-level system programming and high-level expressiveness.

[More examples](https://github.com/Rakhyvel/Orange/blob/main/tests/integration/build)

### Arbitrary Compile-Time Execution
Orange has a robust compile-time execution model that allows any function to be ran at compile-time.

```rs
fn compile_regex(pattern: String) -> Regex {
    // normal code: parse, build NFA, determinize, minimize...
}

const number_regex = compile_regex("[0-9]+") // regex compiled at compile-time

fn main() with core.System {
    if number_regex.matches("12345") {
        @println("Matched a number!")
    }
}
```

[More examples](https://github.com/Rakhyvel/Orange/blob/main/tests/integration/comptime)

## Contributing
Contributions of all kinds are welcome. Check out [CONTRIBUTING.md](https://github.com/Rakhyvel/Orange/blob/main/CONTRIBUTING.md) for more info.

## License
Orange is open-source and released under the MIT License. See `LICENSE` for details.