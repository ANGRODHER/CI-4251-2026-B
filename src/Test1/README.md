# Test 1

The following test will have 4 questions. Please read them carefully and answer them in the `Sol.hs` file.

Do NOT change any type signature, you may create helper definitions and types if you need to.

You CAN (and should) use any library that's available in the current project, but you should not add any new dependency.

Some libraries that you may find useful are:

- `Data.Folddable`
- `Data.Traversable`
- `Data.Map`
- `Data.Set`
- `Control.Monad.Reader`
- `Control.Monad.Free`
- `Control.Monad.State`
- `Control.Monad.Except`

(all of these libraries are already imported in the `Sol.hs` file)
(you can add more libraries if you need to, but you should not add any new dependency)


## Question 1.


Consider an ATM with 4 operations:

- `CheckBalance`: Which given an account ID returns the balance (as a `Float`ing number) of that account.
- `Deposit`: Which given an account ID and an amount (as a `Float`ing number) adds that amount to the balance of the account.
- `Withdraw`: Which given an account ID and an amount (as a `Float`ing number, either substracts that amount from the balance
    of the account if the balance is enough, or returns an error in any other case.
- `GetUser`: Which given an account ID, returns the `User` associated with the ID or `Nothing` if no such user exists.

Represented by the haskell data type:

```haskell
data ATMF a
    = CheckBalance AccountId (Int -> a)
    | Deposit AccountId Int a
    | Withdraw AccountId Int (Either Error a)
    | GetUser AccountId (Maybe User -> a)

```

### Part 1.

Define a `Functor` instance for the `ATMF` type. Prove that your implementation
satisfies the functor laws (can be inside a comment, or in a markdown file).

### Part 2.

Provide an implementation for the `checkBalance`, `deposit`, `withdraw`, and `getUser` functions,
which lifst each constructor into the `Free ATMF` monad.

```haskell
checkBalance :: AccountId -> Free ATMF Int
checkBalance = undefined

deposit :: AccountId -> Int -> Free ATMF ()
deposit = undefined

withdraw :: AccountId -> Int -> Free ATMF (Either Error ())
withdraw = undefined

getUser :: AccountId -> Free ATMF (Maybe User)
getUser = undefined
```

### Part 3.

Provide an implementation for the `transfer` function, which mimics
a transference between two parties. Handle every possible error case (e.g. insufficient funds, non existing accounts, etc).

```haskell
transfer :: AccountId -> AccountId -> Int -> Free ATMF (Either Error ())
transfer = undefined
```

### Part 4.

Is it possible to "commute" the effect of the return type from `transfer`? that is,
can we implement the function:

```haskell
   transfer' :: AccountID -> AccountID -> Float -> Either Error (Free ATMF ())
```

If so, provide an implementation. If not, explain why it is not possible.
Your argument should be paired with a typing example that shows the possible problem with commuting the effects.

**Extra1**: If its not possible, provide an alternate type signature for `transfer'` that would allow us to commute the effects.
    Provide an implementation for this alternate type signature.
    Do NOT change the arguments, just change the constraints:

```haskell
transfer' :: (...) => AccountID -> AccountID -> Float -> Either Error (Free ATMF ())
```

### Part 5.

Provide an interpreter for the `Free ATMF` type, that is implement the

```haskell
   interpret :: Free ATMF a -> BankState -> (Either Error a, BankState)
```

Function which interprets the `Free ATMF` program using the
`BankState` DB.

### Part 6.

Given a  `Free ATMF` program, use static analysis to get all the account IDs that are used in it.
That is, implement the function:


```haskell
    accountIDs :: Free ATMF a -> [AccountId]
```

## Question 2.

Consider a data type that models a weighted graph encoded as an adjencency list:


```haskell
type Weight = Int
newtype Graph a = Graph [(a,[(Weight,a)])]
```

### Part 1.


Implement the `paths` function, which returns all the possible non-cycling paths in a graph
starting from the given node.

```haskell
paths :: forall a. Ord a => a -> Graph a -> [[a]]
```

The implementation must use the Reader/State monad. Explain why did you choose one over
the other? Is it still possible to implement the other?

## Question 3.

Given the following data type:

```haskell
data MyExceptT e m a = MyExceptT { runMyExceptT :: m (Either e a) }
```

Implement a `Functor, Applicative` and `Monad` instance for `MyExceptT e m`
(you can assume that `m` is a `Functor/Applicative/Monad`).


## Question 4.

We'll implement a small chess game in this section.

### Part 1.

Define the type `AdditionalState` which will carry any additional information that
you want to keep track of in the game.

```haskell
type AdditionalState = ()
```

### Part 2.


Implement the `initialState` function, which will return the initial state of the chess game:

```haskell
initialState :: BoardState
```

### Part 3.

Implement the `toString` function, which given a `Board`, returns a string representation of the board.

```haskell
toString :: Board -> String
```

Formatting wise, you can choose any format you want, but it should be human readable.
(That is, make it as pretty as you can).

### Part 4.

Implement the `move` function, which given a `Board` and a `Move`, returns the new `Board` in a context where we can
handle errors, keep track of additional state and perform IO operations.

```haskell
move :: Move -> StateT MyState (ExceptT BoardError IO) Board
```

### Part 5

Implement the `parseMove` function, which given a string representation of a move,
returns the `Move` if the string is valid, or an error otherwise.

the string representation of a move should follow the
[Long Algebraic Notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Long_algebraic_notation) format, with
the following additions:

- `o-o` for kingside castling
- `o-o-o` for queenside castling

The `parseMove` must disregard casing and whitespace.

```haskell
parseMove :: String -> Either MoveError Move
```

### Part 6.

Implement the `playGame` function, which will run a game of chess till either player
wins. If an error occurs, the game should not end, instead, the error should be printed
and the game should continue.

```haskell
playGame :: StateT MyState (ExceptT BoardError IO) ()
```
