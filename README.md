<img
    src="res/logo.png"
    alt="Babble's logo, showing a colon character inside matching square braces - both items of punctuation which appear often in Babble code."
    width="150"
    height="150"
    align="left" />
    
# Babble

**[Language tour](doc/tour.bbl)** | **[API documentation](https://aaronc81.github.io/babble)**

<hr />

Babble is a **very experimental interpreted programming language**, which takes inspiration from
Smalltalk and Ruby's syntax, and Rust's algebraic data type model.

Babble aims to have as few language constructs as possible, instead implementing control-flow like
conditionals and loops using methods and _blocks_ (closures).

Here's what it looks like to print the numbers 1 to 50:

```smalltalk
50 times: [ |i|
    Console println: i + 1.
].
```

This calls a method on the type `Integer` named `times:`, and passes it one argument, a block
(denoted by square braces). This block accepts one parameter called `i`; each time the block is
called by the `times:` implementation, `i` is 1 greater, from 0 to 49.

We could also try implementing this using a plain-old conditional loop, too:

```smalltalk
i = 0.
[ i lessThan: 50 ] whileTrue: [
    i = i + 1.
    Console println: i.
].
```

Rather than a `while` construct built-in, the type `Block` has a method named `whileTrue:`, where 
the block repeatedly evaluates itself and calls another block while it remains true.

> If you'd like a longer example, there is a [Brainfuck interpreter](examples/brainfuck.bbl) in the
> _examples_ directory!

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

Babble is _very_ early and not too useful right now. That said, you can run a file of Babble code
with:

```
cargo run -- -f <file>
```

# To-do list

- [x] Mixins - a way of implementing shared functionality, like Rust's traits
- [x] Pattern matching - implemented with match-blocks, which take patterns as parameters and return
      `Match#Hit returnValue` or `Match#Miss` depending on whether the pattern was actually matched
- [ ] Better parser errors
- [x] Better runtime errors
- [x] Complex assignment targets (e.g struct fields)
- [ ] Collection types and literals
    - [x] Arrays
    - [x] Dictionaries
    - [ ] Sets
