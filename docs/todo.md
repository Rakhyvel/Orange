### Nits
<!-- These are good first-issues for those looking to contribute -->
- [x] Rename `case` to `match`
- [x] Find copy-pasted code and tidy it up
- [x] Standardize token, ast, ir *kind* naming convention
- [x] Alphabetize type imports by the module, then by the type.
- [x] Errors should be capitalized
- [x] Everything should be snake_case
    - [x] Token Kinds should be snake_case, prefer not to use abbreviations
- [x] Replace `ast.<union member>.common.token` with `ast.get_token()` for ASTs outside of ast.zig
- [x] Replace `ast.<union member>.common` with `ast.get_common()` for ASTs outside of ast.zig
- [x] Organize functions declarations in BFS order
- [ ] Order switches on enums to the order they were declared in
    - [ ] Order enums better too
- [ ] Comments on each function and struct field
- [x] Make sure nothing is more than 140 columns
- [x] Change some while loops with counter to enhanced for loops
- [x] See if things *really* need to be marked `pub`
- [x] All inits have a corresponding deinit
- [x] Plot input orng sloc against output c sloc, check to see if there's any outlier, analyze...
- [ ] Find some code not covered and write an integration test for it
- [x] Errors should print lines
    - [x] Errors print to stderr instead of debug
    - [x] Lexer fills lines array
    - [x] Errors print line, caret
    - [x] AST `printType` prints to stderr instead of debug
- [x] Symbols def before use tree-walk
- [x] `cond` has at least one mapping with a rhs
- [x] lhs of assignment is lvalue validation tree-walk
- [x] expect types to be `Type` type
- [x] type inferrence, typeof, type checking
    - [x] typeOf function
        - [x] generic `_type` field for ASTs, typeOf checks if it is null, if it is, constructs the type, sets the field. otherwise returns field
- [x] immutability checking
- [x] Redo symbol versions, should probably just be symbols
- [x] rewrite [op]= assignments
- [x] Comparison operators should not be chainable
- [x] Parens (and only parens) should be one 'line', like in Python
- [x] Remove `'` from integer and identifier token. Replace with `_` for integers, disallow for identifiers.
- [x] Require named fields/args to have `.`
- [x] Enforce naming convetions
    - [x] Must be snake case
        <!-- `([A-Z]*[a-z0-9]*_)*[A-Z]*[a-z0-9]*`
    - [x] Cannot define names that begin with `_` outside of prelude
    - [x] Types, traits, and functions that return types must be capitalized. Nothing else may be.
- [x] Make modulo not chainable (perhaps even replace it with `mod` and `rem` prelude functions)
- [x] Make it so that calls take in a list, not necessarily a product, so you can do something like this: `f((1, 2, 3))`
- [x] Adopt Go's newline insertion rules
- [x] Allow trailing commas in parenthesis (`, nl )` => `nl )`)
- [x] Create a builtin module which contains info about builtin types
    - [x] Grep for every instance of `Int8` or something, extract that info to a file
- [x] Fix camelCase detector (post-check, either all letters are capped, or just first of block)
- [x] Error if a symbol is marked mut when it isn't muted
- [x] 1-tuple, 1-sum
- [x] Equality of products is just equality of all members in a product. Same with slices, strings, etc
    - [x] Addresses are equalable
    - [ ] tuple not-equal
- [x] Some function call lines are prepended and then popped before function call
- [x] IRs should use L_Values for sources too (its not harder to optimize btw)
- [x] Put Validation_State in its own file, bring along poisoned and init_structures
- [x] ir -> {(IR, IRKind, IRData), (SymbolVersion, L_Value), BasicBlock, CFG}, lower.zig
- [x] Move DAG to its own file, along with the type set functions
- [x] Keep Symbol and Scope together, remove SymbolKind, create a new file for creating the tree
- [x] Separate symbol pass to map identifiers to symbols
    - Add a separate `field` AST kind, that doesn't refer to an identifier, but to a field
    - Remove anything that now doesn't need to pass scope
    - typeof, expand_identifier, typesMatch
- [x] Simplify validate
- [x] Move `reprFromTokenKind` into TokenKind
- [x] Create a `Labels` struct for lower, to simplify things
- [x] If a `create()` function is inerrant, call it `init` instead
- [x] Functions with long switches should not have any external state, cases should be ideally <20~30 lines long
    - [x] Perhaps create an `arrayOf` AST kind
- [x] In import graph, 1.5 <= (E+1)/N <= 4.0
- [x] look into `Walk.zig` from ziglang, seems like a pretty good way to walk over an AST
    - rewrite `ast` to be like IR
- [x] Write a python script to look for duplicate code!
- [x] Error if non-unit/non-void expression in block that isn't the final expression (this must be discarded, discards are unit typed)
- [x] Figure out how to do lints before GCC does
- [x] Deduplication
- [x] AST walker struct
- [ ] Go through MISRA when writing reqs
- [x] Omitting a `->` in a function defaults to return type to be unit
- [ ] go through and see if parameters are needed, or if their select-children can be taken instead
- [ ] go through anywhere `unreachable` is used, and replace with a descriptive panic, prepend message with `compiler error: `
- [x] bring back `const` as an introducer, remove it as a pattern

### Testing
- [x] test.orng should detect which folders are in tests/integration, and create those folders in tests/integration/build, rather than it being hard-coded.
- [x] find out if GCC supports UBSan! enable it
- [x] allow more than one command line argument
- [x] `count` command which just counts the number of .orng files
- [x] Enable coverage for subdirectories of integration
- [x] Give a list of the tests that failed after a run
- [x] Print integration test results after the tests are run, so it takes less time
- [x] Negative tests should be in folders according to the kind of error, fail if other error than the expected
- [ ] Log manager, which takes in list of names from cmd line, only emits logs if the filename is in the list
- [ ] Automate a way to cycle through problems.txt, when it runs into one that crashes, have it automatically create a regressions file
- [ ] When a test fails, keep track of which test it was and how many times it's failed

### In-House rdgen
- [ ] Written in Zig
- [ ] Parses an LL(1) grammar
- [ ] Rejects if grammar is not LL(1) ("ambiguity" but not exactly)
- [ ] Generates JSON files of example programs, with identifiers, numbers, etc replaced with random values
- [ ] Supports including other ebnf files
- [ ] Multiply all possible paths, barring recursion, through grammar, give that number and use it for fuzz testing metrics
- [ ] Accepts escape sequences

### Language Features
- [x] addresses
    - [x] addr-of operator
    - [x] dereference operator
        - [x] validate address type
        - [x] lvalue
    - [x] codegen pointer types
    - [x] deref copy
    - [x] `&mut` and validation (also find a way to validate assigning to mut symbols, though that's technically different)
- [x] basic optimizations
    - [x] version symbols
    - [x] Constant fold
    - [x] Dead code elimination
    - [x] If a BB is just a branch on a phi parameter, and the argument is a known value, branch to whichever BB would have been branched to  
- [x] `defer`
    - [x] count defers in block tree-walk
- [x] `continue`/`break`/`return`
- [x] function calls
    - [x] parameters
    - [x] anon functions
    - [x] nested functions
    - [x] mut function parameters
    - [x] function call arity/type validation
    - [x] collect function types in program struct, generate typedefs
    - [x] default parameters
    - [x] named arguments
- [x] tuples
    - [x] type dependency DAG
        > Two product types are C-equivalent if they're field's types match as you go through the struct
        > Then, when you generate them, just do `struct struct_1 {int _1, int _2}`, which would match `(x: Int, y: Int)` and `(Int, Int)`
    - [x] `.` select
    - [x] select and loadStruct optimizations
    - [x] dot-assign
        - [x] validate dot lvalue
    - [x] default fields
    - [x] named fields
        - [x] change grammar to use assignments instead of `<-`. assignments have void type anyway, probably no confusion with `==`
    - [x] pointer to tuple, tuple of pointers, select deref
    - [x] should not be able to mix annot and non-annot tuples
- [x] arrays
    - [x] index
    - [x] index-assign
        - [x] validate index lvalue
    - [x] when deref copy prop op is enabled, pemdas.orng emits type error
- [x] slices
    - [x] `[]` slice from array operator, with variants
    - [x] subslices
    - [x] sublices with inferred lower bound (0)
    - [x] sublices with inferred upper bound (length)
    - [x] slice literals
- [x] UTF8
    - [x] Byte type
    - [x] string literals
        > NOT array/slice of characters. UTF8 encoding is variable width, so array/slice of Bytes!
        - [x] Character escapes (`\n`, `\r`, `\t`, `\x<hex byte>`)
        - [x] Error on invalid escapes (this should happen at lex-time)
        - [x] `String` type in prelude, which is an alias for `[]Byte` (immutable byte array)
        - [x] multiline string literals
    - [x] Character literals
        - [x] Character escapes (`\n`, `\r`, `\t`)
        - [x] Error on invalid escapes
        - [x] Error if character literal contains more than one character
- [x] Sum types
    - [x] `||` for union
    - [x] Optimize `double.orng`: use-def analysis is buggy for selects, select-copy elimination opt isn't working
    - [x] equality on sum type values
    - [x] reset back to call syntax, arrow syntax sucks weenee
    - [x] Builtin-function (?) to get tag name as string from sum
- [x] optionals
    - [x] `?` constructor
    - [x] remove `fromOptional`
    - [x] `orelse` coalesce
    - [x] control-flow operators return optional if else isn't present
- [x] errors
    - [x] binary `!`
    - [x] `catch`
    - [x] `errdefer`
    - [x] `try`
    - [x] inferred errors
        > Investigate. Seems problematic to implement well
        - [x] Add convenience functions for sums to get the `ok` annotation from an error, instead of indexing the sum directly
    - [x] remove `throw`
- [x] more compile errors
    - [x] negative tests (coarse grained, just a bunch of files that should fail... for some reason or another. Would be too chaotic to mandate which error or where in source the error should occur)
        - [x] negative tests up to type checking
        - [x] `.poison` AST node which represents apart of the program with errors that should not be re-validated
        - [x] negative tests beyond type checking
        - [x] fix spans being inconsistently off (perhaps make it so that the whole expression is highlighted)
        > 'positive' test cases compile (panics are 'positive'!). 'negative' test cases do not compile!
    - [x] unreachable
        - [x] stack traces
        - [x] go back and add optional `case`, with `| else => unreachable` clauses
    - [x] proper fuzz tests
        > If a program ever compiles, make a log of what the orng program was
        > Fuzz testing will unfortunately crash if compiler crashes (maybe can be de-coupled?), so store latest orng program somewhere too
    - [x] default values
    - [x] explicit discarding with `_`
        - [x] error if discard when it's used
        - [x] error if function parameter is not used
        - [x] error if the result from a call is not used
    - [x] static index out of bounds
    - [x] dynamic index out of bounds
    - [x] static slice lower <= upper
    - [x] dynamic slice lower <= upper
    - [x] static checked arithmetic (actually easy since optimizer already does these automatically)
    - [x] static union tag check
    - [x] dynaminc union tag check
    - [x] sanitize call stack lines to escape " and \
    - [x] suggest the most similar visible identifier when an unknown identifier is used
    - [x] Make Arithmetic (operators) Great, Allegedly
        - [x] Types on both sides should be equal. Expect proper type (No subtyping!)
            - [x] Most binops need types to be *identical*
            - [x] Comparison operators only need types to be *assignable*
                > `x = 1 + 2 + 3` counterpoint: here there are two `+` operators, one performs identical, the other performs assignable
            - [x] Define representability too for constants
        - [x] Add Int8, Int16, Int32, Int64, Float32, Float64, Byte, Word16, Word32, Word64
        - [x] Add UB checking for signed integer arithmetic
            > {Int8, Int16, Int32, Int64} x {negate, add, sub, mult, div, mod, exponent}
    - [x] Enable the following warnings:
        - [x] `-Wformat=2`
        - [x] `-Werror`
        - [x] `-Wall`
            - [x] variable set but not used
            - [x] `unsigned char*` strings
            - [x] self-comparison optimization
            - [x] mark $panic() as _Noreturn
        - [x] `-Wextra`
        - [x] `-Wpedantic`
        - [x] `-pedantic-errors`
        - [x] `-Wconversion`
        - [x] `-Wsign-conversion`
        - [x] `-Wfloat-conversion`
        - [x] `-Wcast-qual`
        - [x] `-Wlogical-op`
        - [x] `-Wshadow`
        - [x] `-Wmisleading-indentation`
- [x] pattern matching
    - [x] `let` product destructuring
        - [x] `mut` applies before symbols
        - [x] Identifiers define new symbols, having a name repeat is a redefinition error
        - [x] recursive pattern matching
        - [x] `_` to ignore
            - [x] error if `_` is marked as `mut` or `const`
    - [x] assign product destructuring
        ```
        let (mut x, mut y): (Int, Int) = {4, 5}
        (x, y) = {y, x}
        ```
        - [x] `_` to ignore
    - [x] `match` expression
        - [x] Value matching
            > expr equals value
            - [x] compile-time check that value is same type as expr
        - [x] Block values
        - [x] Symbol capture
            > infallible match, declare symbol before rhs
        - [x] Product destructuring
            > all of terms match
            - [x] check product arity at compile-time, must match
        - [x] Sum destructuring
            > tag of expr matches tag of pattern
            - [x] check that expr is of the same sum type at compile-time
        - [x] Inferred members
        - [x] Injection
            > lhs sum matches, rhs pattern matches
            - [x] validate injection is valid for type, of course
            - [x] fix injection cruft with domainOf
        - [x] Fix syntax with nested matches
        - [x] `_` to ignore value 
        - [x] Error if match is not total
        - [x] Remove `else` from match blocks
        - [ ] `|` to match different cases
            > Must be paren'd, like `,`
            > All cases must have the same number of symbols defined
            > For each symbol defined, it must have the same type for all cases
        - [ ] `pattern if cond => rhs` for arbitrary additional conditions in match cases
            > I don't like how this is a pattern in itself in rust
        - [x] `low..upp` for ranges, like in Zig. Can leave off `low` or `upp` to be (theoretically) "negative and positive infinity", respectively
            - [x] should work for floats, also
- [ ] new optimizations
    - [x] measure source-to-output ratio
    - [x] string literals should be indexed at compile-time, dont do runtime check
    - [x] remove ifndefs
    - [x] don't generate section comments if the section is empty
    - [x] don't generate the label for a basic block if a CFG only has one basic block
    - [x] If a temporary is only used once, just inline it's definition with where it's used
        > Will require parens if the surrounding def precedence is higher
    - [x] Panics shouldn't jump, call to panic function instead of just for loop
    - [x] Try and print lvaues better
        - [x] `*&x` is just `x`
        - [x] Perhaps do precedence stuff too. Too many parenthesis!
        - [x] `+0` in lvalue for indexing is kinda dumb
    - [x] expand_identifiers should only allocate if anything changes
    - [x] set retval to poison, don't just return from validateAST
    - [x] identity optimizations (adding 0, multiplying by 1, etc..)
        > These aren't always reduced, especially if the other side is variable
    - [x] multiplying by 0, 1 mod x
    - [x] avoid struct copies on select
    - [x] output BB breadth-first, use a queue, do not use recursion
        - [x] don't `goto` BB if it's the next one in the queue
    - [x] replace `x != 0` with `x`, `x == 0` with `!x`
    - [x] copy propagate `loadSymbol` IR
    - [x] instead of checking if def is null, do `what is the most recent definition in this bb`
        > *might* be able to add a list of predecessors to BBs, check list of BBs for defs too, if they agree on a value
        > Currently theres a discontinuity with how the IR works and how codegen works.
        > IR is in SSA form, whereas C is flattened so that all symbvers of a same symbol share a same variable
        > This is the preferable way to gen C code, but it leads to tricky situations one must be careful of
    - [x] better select optimizations
    - [ ] local value numbering
    - [ ] partial redundancy elimination
    - [ ] induction variable identification/unrolling (this is a good one if possible)
        - [ ] loop invariant lifting
    - [ ] ! inlining
        > Incredibly important optimization
    - [ ] ? pointer aliasing analysis (should squash most whitebox testing)
    - [ ] Codegen optimizations (less important)
        - [x] if a branch depends on a symbver with a `negate` def IR, plunge `negate` and flip labels
        - [ ] `derefCopy` should elide `*&`s
        - [ ] `printSymbolVersion` should collapse addrOf IR, etc
    - [x] coverage!
    - [x] fuzz with updated `fuzz.ebnf` to force a proper main function
- [x] compile-time evaluation (12/17/23)
    > Validate, IR, Optimize. Don't codegen, interpret!
        > Create the program struct very early on
        > Pass the program struct to validator, as validation may result in comptime code execution
        > Symbol > MIR > Optimization > Offset/slots > LIR (concat to program struct instructions)
        > When interpreting, use the program struct's instructions.
        > Symbols are referenced by their offsets.
        > BB and CFG are referenced by the next instruction, as if they were labels
        > Nothing should change for codegen, codegen uses symbvers and such, still prints out labels
    - [x] `const` is an annotation and symbol modifier
    > When a compile-time expression is evaluated
        - validate expr
        - surround with CFG context
        - convert to IR
        - optimize
        - interpret
    > When a function is needed to run at compile-time, check to see if the function CFG exists.
    - [x] expressions marked `comptime`
        - [x] redo code-gen, pass around writer, not a file, move main functions to modules
        - [x] be able to get expanded type from lvalue
        - [x] rename index field to rhs
    - [x] eval symbol defs defined as const
    - [x] eval default inits
    - [x] eval array sizes
    - [x] eval type annotations
    - [x] should be able to index heterogenous tuples with a comptime value
    - [x] various type operations
        - [x] `typeof` prefix operator which returns type expression of expression
        - [x] `default` prefix operator, which takes a type and returns the default value for that type
        - [x] define type inference and defaults for variable declarations in terms of `typeof` and `default`
            > This will require some fenangling, but it should be possible.
        - [x] define `==` and `!=` operators for types, do at comptime. Defined to be `lhs <: rhs and rhs <: lhs`
            - [ ] stretch goal: `<` and `>` for subtypes
        - [x] `sizeof`
        - [x] make `typeof`, `default`, `sizeof` functions that start with `@`
    - [x] reduce `if` at comptime
    - [x] Panic for div by zero, if int type cannot represent value, out of bounds (redo this for codegen!!)
- [ ] type classes / interfaces / traits 
    > Do not use for operator overloading!
    - [x] `trait T { ... }` to define it
        > These can be defined in any scope. They belong to their enclosing scope.
        > They are namespaced separately from other symbols. Lookup is similar.
    - [x] `impl [T] for X { ... }` to implement it for a type
        > These can be implemented in any scope. They belong to their enclosing scope.
        > Each scope has a `Type: AST -> Set(Trait: Trait)` "map". `Trait` types are a list of member function declarations.
        > This should create a global vtable
        - [x] Can leave trait null, static member functions
        - [x] Cannot impl for an addr-of
    - [x] `dyn Tr`, which is two pointers, one to the data, the other to the global vtable
    - [x] dot prepend `x.>f()`
        > In Scope, given `T = @typeof(x)`, lookup any trait `Tr` impls for the type `T` which match function name, parameter and return type
        > `scope.impl_map.lookup(@typeof(x))` gets you all the traits that the typeof x implements
        - [x] If x is a `dyn Tr`, still works
    - [x] `virtual` keyword
    - [x] `Self` type
        - [x] `Self` type in traits means the future type that will implement the type
        - [x] `Self` type in impls means the "for" type
    - [ ] Add an optimization such that if the type of an invoke is known, rather than an invoke, change to just a call to the method
- [ ] function stamping
    - [x] functions returning `Type` should be capitalized
        - [x] capitalized should be all of them not just the first first first
    > When a function has any constant parameters, stamp out a new version of the function for each unique combination of arguments
    - [x] first-class types based generics (stamp)
        - [x] templated templates
        - [x] default parameters
        - [x] default const parameters
        - [x] template-call in template function
        - [x] generic types without `comptime` keyword
    - [x] memoize expanded call types
    - [ ] impl constants
        - [x] iterators & for loops
            - [ ] multi-loops, ranges like zig
        - [x] type methods (Default trait)
    - [x] generic traits
        - [x] `trait From(const T: Type) { fn from(value: T) -> Self }`
    - [x] generic type unification
        > Types can begin with `$ident`, where the type of `ident` will be inferred, and defined as a constant parameter
        - [x] error if an identifier is `$` twice
        - [x] error if `$` appears anywhere else but a type annotation
    - [ ] generic methods
    - [ ] `impl Tr` stamps out a new monomorphised function for every new impl of `Tr`
        > Even though the type is stamped out, you can only treat it using its trait methods
    - [x] allocators
    - [x] Eq, Ord, Num, Bits, Convertible in std
    - [ ] derive
    - [x] `as` method which can do reinterpret casting (maybe different name?)
        > trait Convertible(const Other: Type) { fn as(self) -> Other }
- [ ] build system (built upon compile-time evaluation) 
    > **!IMPORTANT!** Should output .c and .h pair for each .orng file. Track dependencies, and only run CC on modified files and the files that depend* on modified files, to produce .o files which should be linked.
    > Projects are made up of packages are made up of modules. Projects have a `build.orng`.
    - [x] compile phase
        - [x] locate `build.orng`
        - [x] compile `build.orng` file into a module
    - [ ] build phase
        - [x] Setup a `Builder` object, which represents a DAG of goals (ie compiling an executable), and steps that achieve those goals
        - [x] Pass the Builder (along with cmd line args) to `build()` in `build.orng`, interpret
        - [x] `build()` fleshes out the DAG
            - [ ] executables have build modes like (debug | release), (executable | static library | dynamic library)
                - [ ] **IMPORTANT** indexes need to make their lhs lvalues in IR iff debug mode is off
            - [x] be able to include C source files
            - [x] be able to specify C header directories
            - [x] be able to link static libraries
            - [x] be able to link dynamic libraries
            - [ ] freestanding programs
    - [x] make phase
        - [x] Directed by the Builder object, and by the target specified by the cmd line args, execute the steps
    - [ ] `pub` keyword
    - [ ] `[pub] import qualified-name`, `[pub] using qualified-name` keywords
        > Packages are directories with a `build.orng` file (and likely either a `main.orng` or `root.orng` file)
        > Modules are either `.orng` files, or directories with a `root.orng` file, that conventionally imports/uses other modules in it's directory
        > Importing/Using namespaces begin either locally in the immediate directory, or with the dependencies added in the packages `build.orng`
        - [x] `Builder::add_dependency`, which can take either an absolute, relative, or git input
    - [x] `::` for module access
    - [ ] `@filename()`, `@line()`, `@fn_name()`
    - [x] `test` to do tests
        > Rework integration tests
    - [x] `fn main(sys: System)->!()`
        > System contains:
            - args: []String // the command line args
            - stdin: dyn Reader
            - stdout: dyn Writer
            - stderr: dyn Writer
            - global_allocator: dyn Allocator
            - fs: // File System?
            - net: // Socket system?
            - env: // How should environment variables work?
    > How do externs work?
    - [ ] random trait stuff now
        - [ ] Cannot impl an empty trait for primitives!
        - [ ] If T isn't null, either T or X must be defined in the same module
        - [ ] If T is null, X must be defined in the same module
- [ ] refinement types
    > This feature may not be needed, or good
    - [ ] `where` which checks at runtime if a condition is true, panics if it's not
    - [ ] `==>` implies boolean operator might be handy
        > `a ==> b` == `(not a) or b`
        > this is just sorta useful in general, even without refinements, which might not be as useful actually
        > `debug::assert(ast^ == .sum_value ==> ast_type^ == .sum_type)` <- very concise, readable, useful!
        > `ast^ == .match ==> mappings_have_rhs(ast)` <- short-circuits exactly like `(not a) or b`, (which isn't actually all that useful because rhs has to eval to a bool...)
    - [ ] `where comptime` checks a condition at compiletime. If a condition is false at compile-time, error
- [ ] type reflection
    - [ ] `@type_info()` and `@Type_Info()`, which give and are type reflection respectively
    - [ ] `@insert()`, which takes a comptime string and returns comptime AST structure

### Maybe Pile & Refactors
- [x] `undefined` (does this break some optimization assumptions?)
- [ ] Split up validate into separate passes (typecheck, pattern, arg resolution, consteval)
- [ ] Split up CompilerContext into separate views (Module_Compiler, Package_Resolver, Compilation_Session coords both)
- [ ] Node IDs for ASTs, Symbols. Lazy passes hold maps from ID to some record answering some query. Substitutions should be on symbol ID, not name
- [ ] Various AST wrappers w/ information (Typed AST, Decorated AST, Lowered AST) and avoid modifying ASTs in-place
- [ ] `Symbol` shouldn't hold pointer to `CFG`
- [ ] Counter for how many impls/symbols/etc have been validated, ensure everything is validated before codegen
- [ ] `AST.clone` preserves identifiers if not being substituted
- [ ] Make test script more "TUI"-y, only re-write last line, newlines only for errors. Tell me how many tests passed/failed
- [ ] Convert some recursive calls to iteration. Pipelines should be iterative. `walker.zig`, `scope_validate.zig`, should use a work queue, not recursion. Might be able to do validate_ast and lower_ast too, by splitting up the sequencing and the execution
- [ ] Logger
- [ ] rename files meant to be types to be their true Orange_Case
- [ ] comptime contexts, interpretation mode, REPL and notebooks, live coding?

### Core Library
> Mostly trait and type definitions, and really basic stuff. Available for freestanding programs
- [ ] ASTs
    > All the ASTs, for type reflection and everything
- [x] Ops
    - [x] `trait Add`
    - [x] `trait Sub`
    - [x] `trait Mul`
    - [x] `trait Div`
    - [x] `trait Mod`
- [x] IO
    - [x] `trait Writer`
    - [x] `trait Reader`
- [x] Iterator
    - [x] `trait Iterator`
    - [x] `trait Into_Iterator`
    - [ ] `map`, `filter`, `reduce` (requires templated methods)
- [x] ASCII
    - [x] `is_whitespace()`
    - [x] `is_lower()`
    - [x] `is_upper()`
    - [x] `is_digit()`
    - [x] `is_alphanumeric()`
- [ ] Collections
    - [ ] List
        - [x] `Array_List`
        - [ ] `Double_Linked_List`
    - [x] HashMap
- [x] Memory
    - [x] `trait Allocator`
    - [x] `const Fixed_Buffer_Allocator`
    - [x] `const Arena_Allocator`
    - [x] `impl Eq for []T\Eq`
- [x] Strings
    - [x] String Buffer
- [x] UTF8
    - [x] `utf8_byte_sequence_length()`
    - [x] `utf8_decode()`
    - [x] `utf8_count_codepoints`

### Standard Library
> Full standard library for user-mode programs
- [ ] Debug (?)
    - [x] `print()`
    - [x] `assert()`
    - [ ] `dump_currect_stack_trace()`
- [x] Testing
    - [x] `expect()`
    - [x] `expect_equal()`
- [ ] Process
    - [ ] something like `subprocess.Popen` in Python
- [ ] Threads
- [x] Fmt
    - [x] `trait Format`
    - [x] `parse_int()`
    - [x] `parse_float()`
- [x] Rand
    - [x] `prng()`
- [ ] File System
    - [ ] `const max_path_bytes`
    - [ ] `realpath()`
    - [ ] `cwd()`
- [ ] Math
    - [ ] `add() // does overflow check`
    - [ ] `sub() // does overflow check`
    - [ ] `mul() // does overflow check`
    - [ ] `lossy_cast()`
- [ ] C stdlib headers, even python imports!

### Language Server
- [ ] Highlighting
- [ ] Goto definition
- [ ] Show types when you hover
- [ ] Show types when they're inferred
- [ ] Show error messages
- [ ] Formatter
    - [ ] Takes in code and formats it
- [ ] Linter
    > Make as many of these errors as you can, no one pays attention to warnings
    - [ ] Warn if line is too long (140, after being whitespace stripped, not including comments, is excessive)
    - [ ] Warn about code such as `y, x = x, y` which is parsed as `y, (x = x), y`
        > Error when non-unit expressions are in the middle of a block?
    - [ ] Warn if identifier is not in the right style
    - [ ] Warn if identifier begins with `0` (not an octal, a decimal! use `0o` for octal)
- [ ] Documentation generator
    - [ ] `///` Comment blocks are documentation
    - [ ] `\<attribute>` to mark attributes
    - [ ] error if attributes aren't documented or if they are but aren't present in the function
- [ ] Testing
    - [ ] Can discover and run tests in VSCode

### Wishlist
- [ ] Make matches have to be exhaustive over all product types too. Make it UB to not match. Don't return optional value for matches
    > Generate all possible values (treat Ints and Floats as unit)
    > Feed values to patterns in order (ignore {block} patterns). 
    > If a pattern accepts, mark pattern, next value. 
    > If no patterns accept, value is witness of a value not matched by match, emit error.
    > After, if a pattern is not marked, it is a useless pattern, emit error.
    > Just not sure on the details. How are values represented? How are they generated? How are they accepted?
    > There's a better way to do this with matrices, but I didn't understand the papers.