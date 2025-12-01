# The Orange Programming Language
## Introduction
Orange is a systems level programming language. It emphasizes manual memory management, together with high-level abstractions.

Below is a "Hello, World!" program written in Orange:
```orng
fn main() -> ()!() with core::IO {
    try @println("Hello, World!")
}
```

## Program Structure
### Packages
Packages are collections of Orange code. Packages contain a root module, and can depend on other Orange packages. Package dependencies cannot be cyclic.

### Module System
Orange uses a file-based module system where each `.orng` file represents a module. The module name is derived from the file basename.

### Imports
Items are imported using the `import` keyword. Imports can be paths. The left-most item in a path must be a module or package. The right-most item in a path is declared as an alias to the original item the path refers to.
```orng
import module_name
import package_name
import package_name::module_name::Some_Type
```

### Cincludes
C headers can be included using `cinclude`, with a string known at compile-time.
```orng
cinclude "SDL3/SDL.h"
```

## Types
### Primitive Types
The following are builtin primitive types.
| Type name | Description              | C Equivalent |
|-----------|--------------------------|--------------|
| `Int8`    | 8-bit signed integer     | `int8_t`     |
| `Int16`   | 16-bit signed integer    | `int16_t`    |
| `Int32`   | 32-bit signed integer    | `int32_t`    |
| `Int64`   | 64-bit signed integer    | `int64_t`    |
| `Word8`   | 8-bit signed integer     | `uint8_t`    |
| `Word16`  | 16-bit unsigned integer  | `uint16_t`   |
| `Word32`  | 32-bit unsigned integer  | `uint32_t`   |
| `Word64`  | 64-bit unsigned integer  | `uint64_t`   |
| `Float32` | 32-bit floating point    | `float`      |
| `Float64` | 64-bit unsigned integer  | `double`     |
| `Float64` | 64-bit unsigned integer  | `double`     |
| `Bool`    | Boolean                  | `_Bool`      |
| `()`      | Unit type                | `void`       |
| `Void`    | Void type                | N/A          |
| `Int`     | Type alias for `Int64`   | `int64_t`    |
| `Byte`    | Type alias for `Word8`   | `uint8_t`    |
| `Float`   | Type alias for `Float64` | `double`     |

### Tuple Types
1. A _tuple_ represents a heterogenous collection of children types.
2. Tuple types support the index operator when the index is known at compile-time.
3. Tuple index bounds are checked at compile-time.

```orng
// A tuple
let mut tuple: (Int, Float, String) = (67, 3.14, "🍊")

// Tuples support indexing
tuple[0] += 2
tuple[2]

// Tuple 
```

### Array Types
1. An _array_ is a fixed-size sequence of `n` elements of type `T`. The array type is written as `[n]T`. 
2. The size of an array must be known at compile-time.
3. The size of an array is of type `Int`.
4. Arrays support the index operator.
5. The bounds for an array type of length `n` are `[0..n)`.

```orng
// An array
let mut array: [3]Int = [1, 2, 3]

// Arrays support indexing
array[0] = 4
array[2]
```

### Slice Types
1. A _slice_ represents a 'view' into an array of elements of type `T`.
2. An immutable slice does not allow its elements to be modified. It is written as `[]T`.
3. A mutable slice allows its elements to be modified. It is written as `[mut]T`.
4. Slices have a `data` field that represents the underlying pointer to the data. For immutable slices, it is of type `[*mut]T`. For mutable slices, it is of type `[*]T`.
5. Slices have a `length` field that represents the number of items in the slice. It is of type `Int`.
6. Slice types support the index operator.
5. The bounds for a slice type of length `n` are `[0..n)`.

### Strings
1. _Strings_ are an immutable slice of `Byte`s, representing UTF-8 encoded text.

### Struct Types
1. A _struct_ type is a heterogenous product of other types.
2. All types in a struct are annotation types, with a field name.
3. Fields of a struct type are accessed with the select operator.

### Enum Types
1. An _enum_ type is a tagged union of other types.
2. The payload of the active enum variant can be accessed with the select operator.

### Function Types
1. A _function_ takes a list of parameter types, called the domain, to an output type, called the codomain.
2. Function types can optionally specify a list of context types.
3. Values of function types can be called.

### Address Types
1. An _address_ represents the location of a value at run-time.
2. An immutable address of a type `T`, written `&T`, does not allow the modification of the refered-to value through the address.
3. A mutable address of a type `T`, written `&mut T`, allows the modificatin of the refered-to value through the address.
4. An address is assumed not to be `NULL`.
5. Address types support the dereference operator.

### Pointer Types
1. A _pointer_ represents the starting location of many values at run-time.
2. An immutable pointer of a type `T`, written `[*]T`, does not allow the modification of the refered-to value through the pointer.
3. A mutable pointer of a type `T`, written `[*mut]T`, allows the modificatin of the refered-to value through the pointer.
4. An pointer may have any representation, including `NULL`.
5. Address types support the index operator.
6. No bounds checks are generated for indexes on pointer types.

### Trait Object Types
1. A _trait object_ is an opauqe value of another type that implements the trait.
2. An immutable trait object of a trait `Trait`, written `&dyn Trait`, allows only virtual methods with no reciver, the `self` receiver, and the `&self` receiver to be invoked through the trait object.
3. A mutable trait object of a trait `Trait`, written `&dyn Trait`, allows all virtual methods to be invoked through the trait object.

### Type Parameters
1. A _type parameter_ represents an generic type.
2. Type parameters may have trait implementation constraints assigned to them.
3. Values of type parameters only support the operations for which they've been constrained to.

## Declarations
### Constant Declarations
1. A _constant_ represents a value known at compile-time.
2. Constants are declared with the `const` keyword.
3. Constant declarations do not support type inference.

### Variable Declarations
1. A _variable_ represents a value which may vary at run-time
2. Immutable variables cannot have their value changed once set.
3. Mutable variables can have their value changed after being set.
4. Variable declarations support type inference.

### Function Declarations
1. A function is declared with the `fn` keyword, and the declaration can be used as a function value.
2. A function may optionally specify a name.
3. A function has a list of zero or more generic parameters.
4. A function has a list of zero or more parameters.
    1. Parameters have a name and type.
    2. Parameters optionally have a default value that is known at compile-time.
5. A function may optionally specify a return type. If no return type is specified, then it is the unit type.
6. A function has a list of zero or more contexts.
7. A function has a body block. The result of calling the function is the value of the function in the context of the specified generic parameters, parameters, and specified contexts.

### Struct Declarations
1. A _struct declaration_ declares and names a struct type.
2. A struct declaration begins with the `struct` keyword.
3. A struct declaration has a list of zero or more generic parameters.
4. A struct declaration has a list of zero or more fields.

### Enum Declarations
1. An _enum declaration_ declares and names a enum type.
2. An enum declaration begins with the `enum` keyword.
3. An enum declaration has a list of zero or more generic parameters.
4. An enum declaration has a list of zero or more variants.
    1. An enum variant consists of a name, and an optional tuple payload type.

### Type Alias Declarations
1. A _type alias declaration_ introduces a new name for a type expression.
2. A type alias declaration begins with the `type` keyword.
3. A type alias declaration has a list of zero or more generic parameters.

### Trait Declarations
1. A _trait declaration_ declares and names a trait.
2. A trait declaration begins with the `trait` keyword.
3. A trait declaration has a list of zero or more method, type alias, and constant declarations.

### Method Declarations
1. A method declaration_ is declared with the `fn` keyword.
2. Method declarations can optionally start with the `virtual` keyword, as long as the rest of the method declaration does not refer to the `Self` type.
3. Method declarations must specify a name.
4. Method declarations have a list of parameters.
    1. The first parameter may be either the `self`, `&self`, or `&mut self` receivers.
    2. Parameters have a name and a type.
    3. Parameters may optionally have a default value that is known at compile-time.
5. A method declaration can have a body block.
    1. When the method declaration appears in an implementation declaration, this body is mandatory.
    2. When the method declaration appears in a trait declaration, this body is optional.

### Implementation Declaration
1. An _implementation declaration_ declares an implementation of a trait for a type.
2. Implementations begin with the `impl` keyword.
3. Implementations have a list of zero or more generic parameters.
4. Implementation optionally specify a trait. If no trait is specified, a unique, anonymous trait is constructed for the implementation.
5. Implementations have a for-type, introduced after the `for` keyword.
6. Implementations have a list of method, type alias, and constant declarations. These must match in type, virtuality, arity, and constraints with the specified trait.
7. If a declaration does not appear in an implementaiton, then its declartion is taken from the trait.

### Test Declarations

### Context Declarations
1. A _context declaration_ declares and names a context.
2. A context declaration begins with the `context` keyword.
3. A context declaration has a list of zero or more generic parameters.
4. A context declaration has a list of zero or more fields.

## Expressions
### Literal Epxressions
1. An integer literal expression represents an integer value.
2. Integer literals can be coerced to any numeric type, defaulting to `Int`.
3. A floating point literal expression represents a floating point value.
4. Floating point literals can be coerced to any numeric type, defaulting to `Float`.
5. Character literals reprsent a unicode codepoint.
6. Character literals can be coerced to any word type, defaulting to `Word32`.
7. String literals represent UTF-8, null terminated text. They are of type `String`.

<!-- 
TODO:
- bin, oct, hex
- sci notation
- escapes
- multiline strings
- true/false
- struct/enum/array/tuple values
-->

### Block Expressions
1. A block is a list of statements, surrounded by `{` `}`.
2. The final statement can optionally be a return, continue, or break.
3. If a block does not contain a final statement, it evaluates to the last statement in the block.

### Operator Expressions
The following operators are in order of precedence.
| Syntax       | Description                |
|--------------|----------------------------|
| `a and b`    | Boolean AND                |
| `a or b`     | Boolean OR                 |
| `a == b`     | Equality                   |
| `a != b`     | Inequality                 |
| `a < b`      | Greater-than               |
| `a > b`      | Less-than                  |
| `a <= b`     | Greater-than or equal      |
| `a >= b`     | Less-than or equal         |
| `a catch b`  | Defaulting error unwrap    |
| `a orelse b` | Defaulting optional unwrap |
| `a + b`      | Addition                   |
| `a - b`      | Subtraction                |
| `a * b`      | Multiplication             |
| `a / b`      | Division                   |
| `a % b`      | Modulus                    |
| `a as T`     | Type cast                  |
| `not a`      | Boolean NOT                |
| `-a`         | Negation                   |
| `&a`         | Address-of                 |
| `&mut a`     | Mutable ddress-of          |
| `[]a`        | Slice-of                   |
| `[mut]a`     | Mutable slice-of           |
| `try a`      | Early return error unwrap  |
| `a()`        | Call                       |
| `a[b]`       | Index                      |
| `a[b..c]`    | Sub-slice                  |
| `a.b`        | Field select               |
| `a::b`       | Member access              |
| `a.>b()`     | Method invoke              |
| `a^`         | Dereference                |

### Call Expressions
1. A _call expression_ has a left-hand side that refers to a function, and a list of arguments.
2. Arguments can either be all positional, corresponding to the function's parameters by position, or named, corresponding to the function's parameters by name, but not both.
3. Named arguments can be specified in any order.
4. Any unspecified arguments are filled in with the default values from the function.

### If Expressions
1. An _if expression_ has a condition and body block. When evaluated, if the condition is true, the if expression evaluates to the else block.
2. An if expression optionally has an else block. If the condition is false, the if expression evaluates to the else block.

### While Loop
1. A _while loop_ has a pre-condition statement, a condition, and a post-condition statement, a loop body block, and an else block.
2. The pre-condition statement is executed before the while loop is executed.
3. The condition is evaluated before each execution of the loop. If it is true, then the loop body block is executed. If it is false, then the else block is executed.
4. After every iteration, the post-condition statement is executed.

### Match Expressions
1. A _match expression_ consists of an expression to match, and a list of mappings.
2. Each mapping has a pattern and a body.
3. The match expression evaluates to the body of the first mapping whose pattern matches the value.

### Comptime Expressions
1. A _comptime expression_ is denoted with the `comptime` keyword, and a block to execute at compile-time.

## Statements
### Defer Statements
1. The _defer statement_ consists of the `defer` keyword, and a statement to run before the enclosing block exits.
2. Defer statements are ran in reverse order of declaration.
3. Defer statements that are not reached are not executed as the block exits.

### Errdefer Statements
1. The _errdefer statement_ consists of the `errdefer` keyword, and a statement to run before the enclosing function exits, if it is returning an error.
2. Errdefer statements are ran in reverse order of declaration.
3. Errdefer statements that are not reached are not executed as the block exits.
4. Errdefer statements are executed even if the error is returned passively.

## Patterns
1. The `_` pattern matches any value.
2. Identifier pattern match and name any value.
3. Literal patterns match values that are equal to the literal's value.
4. Tuple patterns consist of a comma-separated list of patterns.
5. Tuple patterns match if all patterns match.

## Generics
1. Generic declarations are monomorphized to new anonymous declarations for each unique set of generic parameters.

## Implicit Context System
1. New contexts can be introduced with the `with` keyword.
2. Functions that specify contexts must be called from within those contexts.
3. The following contexts can be requested by the `main()` function and `test`s are:
    1. `core::Allocating`
    2. `core::IO`
    3. `core::File_IO`
    4. `core::Args`

### Format
1. The `@println`, `@print`, and `@write` built-in functions require the `core::IO` context.
2. The `@println`, `@print`, and `@write` built-in functions take a format string, which surrounds format arguments in `{` `}` in the string.
3. Format arguments in the format string in `{` `}` must implement `core::Format`.
4. The characters `{` and `}` can be escapped in a format string with `{{` and `}}` respectively.
5. The `@write` built-in function takes a trait object that implements `core::Writer` as the first argument, and writes the formatted string to the writer.
5. The `@print` built-in function writes the formatted string to the writer from `core::IO`.
5. The `@println` built-in function writes the formatted string and OS-dependnet newline to the writer from `core::IO`.

## Build System
1. Each package has a file called `build.orng`, which contains a function called `build`, that returns a `core::Package` detailing the build information about the package.
2. The `build` function is interpreted at compile-time for each package.
3. Each package has a root module.
