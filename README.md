<img
    src="res/logo.png"
    alt="Babble's logo, showing a colon character inside matching square braces - both items of punctuation which appear often in Babble code."
    width="150"
    height="150"
    align="left" />
    
# Babble

**[Language tour](doc/tour.bbl)** | **[API documentation](https://aaronc81.github.io/babble)** | **[VS Code extension](https://github.com/AaronC81/babble-vscode)**

<hr />

Babble is a **very experimental interpreted programming language**, which takes inspiration from
Smalltalk and Ruby's syntax, and Rust's algebraic data type model.

Babble aims to have as few language constructs as possible, instead implementing control-flow like
conditionals and loops using methods and _blocks_ (closures).

```
impl Integer {
    func factorial {
        self lessThanOrEquals: 1 $
            ifTrue: [ 1 ]
            else:   [ self * (self - 1) factorial ]
    }
}

Console println: 7 factorial.
```

If you'd like some more examples:

- I used Babble to solve over half of [Advent of Code 2022](https://github.com/AaronC81/advent-of-code-2022)
- There is a [Brainfuck interpreter](examples/brainfuck.bbl) in the _examples_ directory, alongside some other examples

## Data Types

Unlike Smalltalk, Babble isn't object-oriented; data is expressed with sum and product types like
Rust or Haskell. Specifically, the naming from Rust is used - structs hold known fields, and enums
have multiple variants which can each optionally hold a different set of fields:

```smalltalk
struct Person name age occupation.

enum Occupation {
    Student.
    Lecturer.
    SoftwareEngineer.
    Other title.
}

me = Person name: "Aaron" age: 22 occupation: Occupation#SoftwareEngineer.
you = Person name: "The Reader" age: 99 occupation: (Occupation#Other title: "Unknown!").
```

# Usage

Babble is complete enough to use for simple scripts, but there are plenty of rough edges and gaps in
functionality.

To run a file of Babble code:

```
cargo run --release -- -f <file>
```

(Running in release mode is strongly recommended! It's very easy to overflow the stack without...)

# To-do list

- [x] Mixins - a way of implementing shared functionality, like Rust's traits
- [x] Pattern matching - implemented with match-blocks, which take patterns as parameters and return
      `Match#Hit returnValue` or `Match#Miss` depending on whether the pattern was actually matched
- [x] Better parser errors
- [x] Better runtime errors
- [x] Complex assignment targets (e.g struct fields)
- [ ] Collection types and literals
    - [x] Arrays
    - [x] Dictionaries
    - [ ] Sets
- [x] String interpolation
- [ ] More access control
    - [ ] Private fields
    - [ ] Private constructors
- [x] Keyword to allow function arguments to appear in any order, for constructors
- [x] Core mixins on types (e.g. `Equatable` but static) for more consistency between types and values
