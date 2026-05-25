# Test 2

The following test will have just a single question. Please read the specification carefully and answer it in the `Sol.hs` file.

You CAN (and should) use any library that's available in the current project, but you should not add any new dependency.

This test will be done in groups of 3 or 4 students.

My recommendation is: if you feel that you are struggling with haskell, please group with one student that
is more comfortable with the language. That way you have double the support.

Here is a small (non-exhaustive) list of such students:

- Francisco Marquez
- Leonardo Lopez
- Alejandro Meneses
- Samuel Sanchez

## A Typed Lambda Calculus Interpreter.

Your task is to implement an interpreter for a simply typed lambda calculus which supports:

- Arithmetic operations on Double values: addition, subtraction, multiplication, division, exponentiation,
    sign changed.
- if-then-else expressions (lazy evaluation of branches).
- Function abstraction and application.
- Expression sequencing and mutable variables.
- printing expressions to the console.

## The syntax of the language

The syntax of the language is very typical:

- Variables are identifiers which:
    - start with an alphabetical latter `(a-z, A-Z)`, or an underscore (`_`), and...
    - can contain alphanumeric characters, underscores.
    - can end with a trail of optional simple quotes (`'`).
- We will use `R`,`B`, and `<t1> -> <t2>` to denote the types of Doubles, Booleans, and functions, respectively.
- We will use the json definition of a number for number literals.
- Arithmetic operators are written using the usual symbols: `+`, `-`, `*`, `/`, `^`.
- if-then-else expressions are written as `if <cond> then <expr1> else <expr2>`.
- Function abstraction is written as `/. <identifier> : <type> => <expr>`,
    where `type` is the type of the argument, and `expr` is the body of the funcion.
- Function application is written as `<expr1> <expr2>`, where `expr1`
    is the function being applied, and `expr2` is the argument. Notice that
    function application does not require parentheses for the arguments.
-  Action sequencing is written as `<expr1> ; <expr2>`,
    where `expr1` is the first action to be evaluated,
    and `expr2` is the second action to be evaluated.
- Variable declaration is written as: `let <identifier> : <type> := <expr1>`
    where `identifier` is the name of the variable being declared,
    `type` is the type of the variable, and `expr1` is the expression whose value will be assigned to the variable.
- Variable re-assignment is written as: `<identifier> := <expr1>`,
    where `identifier` is the name of the variable being assigned,
    and `expr1` is the expression whose value will be assigned to the variable.
- Printing is written as: `print <expr>`, where `expr` is the expression to be printed to the console.
- It is not expected that the language supports multi-line expressions, but you can decide to support them if you want to.
    - My recommendation is: look into `sepBy` and `sepBy1` from `Text.Parsec.Combinators`, and combine it with `spaces`/`newline`/...
- Precedence and associativity should be the same as haskell.
    - It's your duty to find the precedence of lambda functions, function application, `if-then-else` expressions,
        sequencing, and variable declaration/re-assignment.



## The semantics of the language

The semantics of the language is also very typical:

- Arithmetic are evaluated in the usual way.
- if-then-else expressions should evaluate the condition first, and then
    evaluate either the then-branch or the else-branch, depending on the value of the condition.
    - `0` Is treated as False
    - Anything else that's not 0 is treated as True
- Function application should be strict in both arguments.
- Actions (sequencing and assignment) don't need to appear in expression contexts
    - That is `(let x := 3) + (let y := 9; y + 9)` can be trated as an error if you prefer.
    - If they are allowed in expression contexts, how are they scoped?
- Printing should return the value that was printed and its treated as an expression.
- Every function is curried by default,
    - That is `<t1> -> <t2> -> <t3>` is the type of a function that takes an argument of type `<t1>`,
        and returns a function of type `<t2> -> <t3>`.
- The language should be statically typed, and the type of every expression should be checked before evaluation.
- The language is also statically scoped, and variables should be resolved according to the lexical scope rules.
    - Lambda functions create a new scope.
- Every value should be printable.
    - parenthesest is, you should be able to print: number literals, boolean literals and functions.
    - Printing a function `/. x : R => x + 1` should be printed as `/. x : R => x + 1`.
    - You can decide to disregard unnecessary parentheses when printing, that is,
        you can print `/. x : R => (3 * 5) + 1` as `/. x : R => 3 * 5 + 1`.
- It is your duty to decide whether you need closures.
- On error, the program can crash with an error message (NOT a haskell exception).

## Recommended approach

- Define the types of the language as a separate data type.
- Define the abstract syntax tree (AST) of the language.
- Divide the work in 4 sections:
    - Parsing
    - Static Analysis and Type Checking.
    - Evaluation
    - Pretty Print
- Pretty printing should be the first thing you do. Makes debugging easier.
- Parsing, Static Analysis and Evaluation can be done in parallel.
- Use:
    - `Control.Monad.Reader` for tracking variables, in particular check `local` function
    - `Cotrol.Concurrent.MVar` for tracking mutable variables (you are gonna end up using `Monad IO`).
    - `Control.Monad.Except` for error handling.
    - `mtl` to combine the above monads.
- Use `showsPrec` for pretty printing, read the example i gave you in the previous class.
- If you want your program to be more elegant, dont use `MonadIO`, instead:
    - Create a `Teletype` effect with a `print` operation
    - Create a `Ref` effect with `newRef`, `readRef` and `writeRef` operations.
    - This way you can guarantee your program doesn't use arbitrary IO operations.

## Expected entry point

You are required to implement a `runL` function wuith the following type signature:

```haskell
runL :: FilePath -> IO ()
```

Which reads a file, and interprets it.


## Some examples

The following program encodes the Fibonacci function, and then prints the result of `fib 6`:

```
let fib : R -> R := /. n : R => if lt n 1 then n else fib (n - 1) + fib (n - 2);
print (fib 6)
```
