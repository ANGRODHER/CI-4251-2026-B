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
- Boolean operations: conjunction, disjunction, negation, implication
- Comparison operations **OVER THE SAME TYPE**: equality, inequality, less than,
    less than or equal to, greater than, greater than or equal to.
- if-then-else expressions (lazy evaluation of branches).
- Function abstraction and application.
- Expression sequencing and mutable variables.
- printing expressions to the console.
- Two ways of parenthesizing: parentheses and curly brackets.
    - Parentheses will not create a new scope.
    - Curly brackets will create a new scope.

## The syntax of the language

The syntax of the language is very typical:

- Variables are identifiers which:
    - start with an alphabetical latter `(a-z, A-Z)`, or an underscore (`_`), and...
    - can contain alphanumeric characters, underscores.
    - can end with a trail of optional simple quotes (`'`).
- We will use `R`,`B`, and `<t1> -> <t2>` to denote the types of Doubles, Booleans, and functions, respectively.
- We will use `True` and `False` to denote the boolean literals.
- We will use the json definition of a number for number literals.
- Arithmetic operators are written using the usual symbols: `+`, `-`, `*`, `/`, `^`.
- Boolean operators are written using the usual symbols: `&&`, `||`, `!`, `=>`.
- Comparison operators are written using the usual symbols: `==`, `!=`, `<`, `<=`, `>`, `>=`.
- if-then-else expressions are written as `if <cond> then <expr1> else <expr2>`.
- Function abstraction is written as `/. <identifier> : <type> => <expr>`,
    where `type` is the type of the argument, and `expr` is the body of the funcion.
- Function application is written as `<expr1> <expr2>`, where `expr1`
    is the function being applied, and `expr2` is the argument. Notice that
    function application does not require parentheses for the arguments.
-  Expression/action sequencing is written as `<expr1> ; <expr2>`,
    where `expr1` is the first expression to be evaluated,
    and `expr2` is the second expression to be evaluated.
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

- Arithmetic and boolean operations are evaluated in the usual way.
    - However the `&&`, `||` and `=>` operators should be short-circuiting.
- Comparison operations are evaluated in the usual way, but they only work on values of the same type.
    That is `3 == True` should be treated as an error.
- if-then-else expressions should evaluate the (boolean) condition first, and then
    evaluate either the then-branch or the else-branch, depending on the value of the condition.
- Function application should be strict in both arguments.
- Expression sequencing returns the right hand side expression as the value of the whole expression,
    - That is, `<expr1> ; <expr2>` should evaluate `expr1`, then evaluate `expr2`, and return the value of `expr2` as the value of the whole expression.
- Function declaration and re-assignment should return the value that was assigned.
    - That is: `let x : R := 9.6 + 5` should return `14.5`
    - And `x := 3.14` should return `3.14`.
- Printing should return the value that was printed.
- Every function is curried by default,
    - That is `<t1> -> <t2> -> <t3>` is the type of a function that takes an argument of type `<t1>`,
        and returns a function of type `<t2> -> <t3>`.
- The language should be statically typed, and the type of every expression should be checked before evaluation.
- The language is also statically scoped, and variables should be resolved according to the lexical scope rules.
    - Lambda functions create a new scope.
    - Parentheses do not create a new scope
    - Curly Brackets create a new scope.
- Every value should be printable.
    - That is, you should be able to print: number literals, boolean literals and functions.
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
let fib : R -> R := /. n : R => if n <= 1 then n else fib (n - 1) + fib (n - 2);
print (fib 6)
```


The following program first prints 9.5 and then prints 3.14

```
let x : R := 3.14; {let x : R := 9.5; print x}; print x
```

If instead we use parentheses, the program should crash with an error signaling
variable re-definition:

```
let x : R := 3.14; (let x : R := 9.5; print x); print x
```

However, re-assignment should work in both cases, that is both of these programs should print 9.5 and then 9.5:

```let x : R := 3.14; {x := 9.5; print x}; print x```
```let x : R := 3.14; (x := 9.5; print x); print x```

It's a good practice that formal parameters of the functions follow the declarative correspondence principle,
which means, they should be trated as if they were normal variables. Thus, the following program
should print 5.

```
(/. x : R -> x := 5; print x) 3.14
```

Notice that the above program should be equivalent to:

```
{let x : R = 3.14; x := 5; print x}
```
