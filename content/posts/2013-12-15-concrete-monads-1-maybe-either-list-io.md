---
title: concrete monads 1 : Maybe, Either, list, IO
tags: haskell, monad
---


Last Modified : 2014 Feb 19 (Wed) 12:35:40 by Harold Carr.

---

This post is part of a [series of posts on monads](http://haroldcarr.com/posts/2014-02-19-monad-series.html).

---

# initial intuition

-   A `Monad` is used to pipe the *output* of one function into another.

-   A monadic "pipe" has code that executes "behind the scenes" (in a
    `Monad` typeclass) rather than being embedded in the main lines of a
    program.
    
    -   This "extra" code is sometimes called "monadic context".

-   Monadic values are sometimes referred to "computations" or  "actions".

-   Sometimes a `Monad` can be viewed as a "container" (e.g., `Maybe`, `Either`, a list).

## monads and side-effects

-   A `Monad` has *nothing* to do with "real" side-effects (e.g., reading/writing a file).

-   A monadic type is often used to simulate side-effects in a purely functional way, aka "effectful".
    
    -   This "effectful" aspect is not shown here in part 1.  It is discussed in part 2.

-   Particular monads, `IO` and some others, do "real" side-effects.
    The monadic part of `IO` is used for ordering, not the side-effects
    themselves.

---

# setup

    import Data.Char
    import Data.Either
    import Data.Maybe (fromJust)
    import System.IO

## unit test

Rather than showing GHCI input/output this article shows test
input/expected results (see [verified articles](http://haroldcarr.com/posts/2013-11-07-verified-articles.html)):

    import Test.HUnit
    import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util
    import System.IO.Unsafe -- for one unit test

---

# Maybe

`Maybe` is used for functions that might return no results (i.e., to turn partial functions into total functions):

    :i Maybe
    => data Maybe a = Nothing | Just a

For example, a hash table would return `Just <something>` if the key is found, otherwise `Nothing`.

The `Nothing` and `Just` parts of the `Maybe` type definition have nothing to do with monads.

A common pattern is to check `Maybe` results:

    maybeExampleNonMonadic :: Maybe t -> (t -> Maybe a) -> (a -> b) -> Maybe b
    maybeExampleNonMonadic i f1 f2 =
        case i of
            Nothing -> Nothing
            Just x  -> case f1 x of
                           Nothing -> Nothing
                           Just y  -> Just $ f2 y

If any result in the pipeline is `Nothing` then stop at that point and
return `Nothing`.  Otherwise, extract the value from `Just` and pass
it to the next function.

In this example, the last function (`f2`) does not return `Maybe`. But
the the result of `maybeExampleNonMonadic` is `Maybe`, so the result
of `f2` is "wrapped" with `Just`.

    toNothing :: t -> Maybe a
    toNothing    _ =  Nothing
    
    nonMonadicDouble :: Num a => a -> a
    nonMonadicDouble x = x + x
    
    tm1 = tt "tm1"
         [ maybeExampleNonMonadic Nothing  Just      nonMonadicDouble -- 1
         , maybeExampleNonMonadic (Just 3) toNothing nonMonadicDouble -- 2
         ]
         Nothing

In `tm1`

-   1: the result is immediately `Nothing` if the first argument is `Nothing`.  Or,

-   2: if the first argument is `(Just 3)` but `f1` (here `toNothing`) returns
         `Nothing` then `Nothing` is returned at that step (and
         `nonMonadicDouble` is never called).

&nbsp;

    tm2 = t "tm2"
         (maybeExampleNonMonadic (Just 3) Just      nonMonadicDouble)
         (Just 6)

In `tm2` the result is `(Just 6)` since both the the first argument and the result of `f1` are `Just` values.

Since the pattern in `maybeExampleNonMonadic` is common, it is abstracted into the monadic
aspects of `Maybe` such that it can be rewritten as:

    exampleMonadic :: Monad m => m t -> (t -> m a) -> (a -> b) -> m b
    exampleMonadic i f1 f2 =
         i >>= f1 >>= return . f2

What remains is the "important" part: piping "successful" computations
from one function to another.  The extraction of values from `Just`,
the short-circuiting of `Nothing` and the sequencing of values is factored
into the `Maybe` `Monad` instance (shown below).

Note: `do` notation can be used instead of `>>=` (note: the type
signatures of `exampleMonadic` and `exampleMonadicDo` are identical).

    exampleMonadicDo :: Monad m => m t -> (t -> m a) -> (a -> b) -> m b
    exampleMonadicDo i f1 f2 = do
        x <- i
        y <- f1 x
        return $ f2 y

In the above, `return` can be thought of as an alias (in this context) for `Just` (more on `return` below).

All three version of the example function return equivalent results:

    tm3 = tt "tm3"
         [ maybeExampleNonMonadic Nothing  Just      nonMonadicDouble
         , exampleMonadic         Nothing  Just      nonMonadicDouble
         , exampleMonadicDo       Nothing  Just      nonMonadicDouble
         ]
         Nothing
    
    tm4 = tt "tm4"
         [ maybeExampleNonMonadic (Just 3) toNothing nonMonadicDouble
         , exampleMonadic         (Just 3) toNothing nonMonadicDouble
         , exampleMonadicDo       (Just 3) toNothing nonMonadicDouble
         ]
         Nothing
    
    tm5 = tt "tm5"
         [ maybeExampleNonMonadic (Just 3) Just      nonMonadicDouble
         , exampleMonadic         (Just 3) Just      nonMonadicDouble
         , exampleMonadicDo       (Just 3) Just      nonMonadicDouble
         ]
         (Just 6)

---

# >>= (aka "bind")

The infix `Monad` `>>=` function handles sequencing and extraction:

-   <http://hackage.haskell.org/package/base-4.6.0.1/docs/src/GHC-Base.html#Monad>

&nbsp;

    class  Monad m  where
        -- | Sequentially compose two actions, passing any value produced
        -- by the first as an argument to the second.
        (>>=)       :: forall a b. m a -> (a -> m b) -> m b

<a id="INSTANCE-MONAD-MAYBE" name="INSTANCE-MONAD-MAYBE"></a>
The short-circuiting of the pipeline on `Nothing` values and the
extraction of values from `Just` and applying a function to those values
is done via the `>>=` definition of the `Maybe` `Monad` typeclass
instance:

-   <http://hackage.haskell.org/package/base-4.6.0.1/docs/src/Data-Maybe.html#Maybe>

&nbsp;

    instance  Monad Maybe  where
        (Just x) >>= k      = k x
        Nothing  >>= _      = Nothing

`Maybe`'s version of `>>=` is given a `Maybe` value on the left and a function `k` on the right.

-   If the left is `(Just x)` then `x` is extracted and given as the argument to `k`.
    
    -   The return value of `>>=` is the return value of `k x`.

-   If the left is `Nothing` then the right function is never called.
    
    -   The return value of `>>=` is `Nothing`.

For more on "bind" see:

-   <http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html>

---

<a id="RETURN" name="RETURN"></a>

# return

The `Monad` `return` function "wraps" a value with an appropriate `Monad`:

    class  Monad m  where
        -- | Inject a value into the monadic type.
        return      :: a -> m a

In the case of `Maybe`, `return x` is `Just x`:

    instance  Monad Maybe  where
        return              = Just

Type-inferencing in the compiler decides which `instance` of `return` to use.

Note: in `exampleMonadic`, `Just . f2` could have been used in place
of `return . f2` &#x2014; they are the same thing in the `Maybe` context.
However, using `return` makes the function generic, as will be seen in
the following examples of other monads (which is why `exampleMonadic`
is not named `maybeExampleMonadic`).

---

# example Maybe evaluations

Given the definition:

    exampleMonadic :: Monad m => m t -> (t -> m a) -> (a -> b) -> m b
    exampleMonadic i f1 f2 =
         i >>= f1 >>= return . f2

and the application:

    exampleMonadic         Nothing  Just      nonMonadicDouble

-   `Nothing` value constructor creates a `Maybe t` `Monad` instance

-   `Nothing` is value of `i`

-   `i` is the left argument of the first `>>=`

-   Since the value is `Nothing`, `f1` is never called and the first `>>=` returns `Nothing`

-   `Nothing` is then the input to the left side of the second `>>=`

-   Since the value is `Nothing`, `return . f2` is never called and the second `>>=` returns `Nothing`

-   `Nothing` is the result of `exampleMonadic`

For the application:

    exampleMonadic         (Just 3) toNothing nonMonadicDouble

-   `Just 3` value constructor creates a `Maybe Int` `Monad` instance

-   `Just 3` is value of `i`

-   `i` is the left argument of the first `>>=`

-   `>>=`
    
    -   extracts `3` from `Just`
    
    -   calls `f1 3`
        
        -   `f1`, in this case, is `toNothing`, so the result of `f1 3` is `Nothing`

-   `Nothing` is the result of the first `>>=`

-   This `Nothing` result is the input to the left side of the second `>>=`

-   Since the value is `Nothing`, `return . f2` is never called and the second `>>=` returns `Nothing`

-   `Nothing` is the result of `exampleMonadic`

For the application

    exampleMonadic         (Just 3) Just      nonMonadicDouble

-   `Just 3` value constructor creates a `Maybe Int` `Monad` instance

-   `Just 3` is value of `i`

-   `i` is the left argument of the first `>>=`

-   first `>>=`
    
    -   extracts `3` from `Just`
    
    -   calls `f1 3`
        
        -   `f1`, in this case, is `Just`, so the result of `f1 3` is `Just 3`

-   `Just 3` is the result of the first `>>=`

-   This `Just 3` result is the input to the left side of the second `>>=`

-   the second `>>=`
    
    -   extracts `3` from `Just`
    
    -   calls `(return . f2) 3`
        
        -   `f2`, in this case, is `nonMonadicDouble`, so the result of `f2 3` is `6`
        
        -   `6` becomes the input to `return 6`
        
        -   since evaluation is happening in the `Maybe` `Monad` instance, `return 6` results in `Just 6`

-   `Just 6` is the result of the second `>>=`

-   `Just 6` is the result of `exampleMonadic`

To see how monadic chaining is useful in long compositions of `Maybe`, see Real
World Haskell [chapter 10](http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html).  Search for `parseP5` (version without
monadic function composition) and `parseP5_take2` (version with
monadic composition &#x2014; but using `>>?` instead of `>>=`).

---

# Either

`Either` is like `Maybe`, but additional information is given on "failure" instead
of `Nothing`:

    :i Either
    => data Either a b = Left a | Right b

`Left` corresponds to `Nothing`.  `Right` corresponds to `Just`.

`Either` is typically used such that `(Right x)` signals a successful
evaluation, whereas `(Left x)` signals an error, with `x` containing
information about the error.

The `Left` and `Right` parts of the `Either` type definition have nothing to do with monads.

The pattern of using `Either` is identical to `Maybe` except, when
short-circuiting on `Left`, the `Left` information is retained and
returned:

    eitherExampleNonMonadic :: Either l t -> (t -> Either l a) -> (a -> b) -> Either l b
    eitherExampleNonMonadic i f1 f2 =
        case i of
            Left  l -> Left l
            Right x -> case f1 x of
                           Left  l -> Left l
                           Right y -> return $ f2 y

The `Monad` instance of `Either` is also identical to `Maybe` except for retaining `Left` information.

-   <http://hackage.haskell.org/package/base-4.6.0.1/docs/src/Data-Either.html#Either>

&nbsp;

    instance  Monad (Either e)  where
        Left  l >>= _ = Left l
        Right r >>= k = k r
    
        return = Right

The evaluation of `Either` is also identical to `Maybe` exception for retaining/returning `Left` information:

    -- Note: these are used instead of directly using Left/Right in the
    -- tests so as not to have to repeatedly specify types at point of use.
    toRight :: Int -> Either Int Int
    toRight = Right
    
    toLeft :: Int -> Either Int Int
    toLeft  = Left
    
    te1 = tt "te1"
         [ eitherExampleNonMonadic (Left (-1)) toRight nonMonadicDouble
         , exampleMonadic          (Left (-1)) toRight nonMonadicDouble
         , exampleMonadicDo        (Left (-1)) toRight nonMonadicDouble
         ]
         (Left (-1))
    
    te2 = tt "te2"
         [ eitherExampleNonMonadic (Right 3)   toLeft  nonMonadicDouble
         , exampleMonadic          (Right 3)   toLeft  nonMonadicDouble
         , exampleMonadicDo        (Right 3)   toLeft  nonMonadicDouble
         ]
         (Left 3)
    
    te3 = tt "te3"
         [ eitherExampleNonMonadic (Right 3)   toRight nonMonadicDouble
         , exampleMonadic          (Right 3)   toRight nonMonadicDouble
         , exampleMonadicDo        (Right 3)   toRight nonMonadicDouble
         ]
         (Right 6)

Notice how `exampleMonadic` was able to be used with both `Either` and `Maybe`.
That is because the appropriate instances of `>>=` and `return` are used based on the type.

Note that in a long chain of `Either`, say the very first value in
the chain is `Left <something>`.  In this case, the entire chain of
`>>=` calls would still be evaluated.  Each one would extract
`<something>` and then just wrap it back up in a new `Left`.  In other
words, there is a slight cost, even in the failure case.

(Note: the `Either e` in the `Monad` instance definition is a
partially applied type constructor &#x2014; see
<http://mvanier.livejournal.com/5103.html> for more info &#x2014; search for
"Making an error-handling monad".)

---

# []

Just as `Maybe` and `Either` may represent none/error (`Nothing`, `Left`) or one (`Just`, `Right`) results,
a list:

    :i []
    => data [] a = [] | a : [a]

can be used to represent none (`[]`) or one or more (`[x, ...]`) results.

The `[]` and `a : [a]` parts of the `[]` type definition have nothing to do with monads.

The list `Monad` typeclass instance:

-   <http://www.haskell.org/ghc/docs/7.4.2/html/libraries/base/src/GHC-Base.html> (search for `Monad []`)

&nbsp;

    instance  Monad []  where
        m >>= k  = foldr ((++) . k)  [] m
    
        return x = [x]

shows that the function `k` is applied to each element of the list
`m`.  Each call to `k` is expected to return zero or more results in a
list.  The results of all the calls to `k` are appended into a single
list.

A non-monadic list version of the example pipelining function might be:

    listExampleNonMonadic :: [t] -> (t -> [a]) -> (a -> b) -> [b]
    listExampleNonMonadic i f1 f2 =
        case i of
            [] -> []
            xs -> case concatMap f1 xs of
                      [] -> []
                      ys -> map f2 ys


(Note: `listExampleNonMonadic` is a bit contrived, as are the examples
in the tests below.  The idea is to keep the examples consistent
between the different `Monad` class instances.)

Note: Although the above function checks for `[]` to "short-circuit" further
evaluation, it is not really necessary since any function returning
`[]` will operate the same:

    listExampleNonMonadic' :: [t] -> (t -> [a]) -> (a -> b) -> [b]
    listExampleNonMonadic' i f1 f2 =
        map f2 $ concatMap f1 i

Given the above non-monadic list functions and the existing
`exampleMonadic` functions it can be seen that the list `Monad`
typeclass instance operates like the `Maybe` and `Either` instances:

    toEmpty :: Int -> [Int]
    toEmpty x = [ ]
    
    toList  :: Int -> [Int]
    toList  x = [x]
    
    tl1 = tt "tl1"
         [ listExampleNonMonadic   [ ]      toList   id
         , listExampleNonMonadic'  [ ]      toList   id
         , exampleMonadic          [ ]      toList   id
         , exampleMonadicDo        [ ]      toList   id
         ]
         []
    
    tl2 = tt "tl2"
         [ listExampleNonMonadic   [1,2,3]  toEmpty  id
         , listExampleNonMonadic'  [1,2,3]  toEmpty  id
         , exampleMonadic          [1,2,3]  toEmpty  id
         , exampleMonadicDo        [1,2,3]  toEmpty  id
         ]
         []
    
    tl3 = tt "tl3"
         [ listExampleNonMonadic   [1,2,3]  toList   id
         , listExampleNonMonadic'  [1,2,3]  toList   id
         , exampleMonadic          [1,2,3]  toList   id
         , exampleMonadicDo        [1,2,3]  toList   id
         ]
         [1,2,3]

See also:

-   <http://en.wikibooks.org/wiki/Haskell/Understanding_monads/List>

---

# recap

The monads above were used for

-   sequencing

-   "wrapping"/"unwrapping" values to/from monads

-   in the explicit case of `Maybe` and `Either`, to short-circuit further evaluation on `Nothing` or `Left`.
    
    -   Explicit short-circuiting was not necessary in the list `Monad` because there is "nothing to do" on an empty list.

Note: the monads above did not involve side effects.

Notice that the type signatures of all the examples so far are isomorphic:

    maybeExampleNonMonadic  ::             Maybe    t  -> (t  -> Maybe     a)  -> (a  -> b) -> Maybe    b
    exampleMonadic          ::  Monad m => m        t  -> (t ->  m         a)  -> (a  -> b) -> m        b
    exampleMonadicDo        ::  Monad m => m        t  -> (t ->  m         a)  -> (a  -> b) -> m        b
    eitherExampleNonMonadic ::             Either l t  -> (t ->  Either l  a)  -> (a  -> b) -> Either l b
    listExampleNonMonadic   ::             [        t] -> (t ->  [         a]) -> (a  -> b) -> [        b]

and follow the shape of `>>=` :

    (>>=)                   :: forall a b. m        a  -> (a  -> m         b)               -> m        b

---

# IO

`IO` uses monadic sequencing (`>>=`) to ensure that operations happen
in a certain order (e.g., writes happen before reads when prompting
for user input).  Those operations also perform side-effects.  The
side-effects are part of `IO`, not part of `Monad`.

    ioExampleMonadic   :: FilePath -> String -> IO Bool
    ioExampleMonadic filename output =
        openFile filename WriteMode >>= \o     ->
        hPutStrLn o output          >>= \_     ->
        hClose o                    >>= \_     ->
        openFile filename ReadMode  >>= \i     ->
        hGetLine i                  >>= \input ->
        hClose i                    >>= \_     ->
        return (input == output)
    
    ioExampleMonadicDo :: FilePath -> String -> IO Bool
    ioExampleMonadicDo filename output = do
        o <- openFile filename WriteMode
        hPutStrLn o output
        hClose o
        i <- openFile filename ReadMode
        input <- hGetLine i
        hClose i
        return (input == output)
    
    ti1 = tt "ti1"
          [ unsafePerformIO $ ioExampleMonadic   "/tmp/BAR.txt"  "BAR"
          , unsafePerformIO $ ioExampleMonadicDo "/tmp/BAR.txt"  "BAR"
          ]
          True

## non-monadic tangent: IO is partitioned from pure functions

There is no way to write a non-monadic `IO` example as was done for
other `Monad` instances above.  The type system partitions
side-effecting `IO` computations types from pure functions.  Pure
functions guarantee the same results for the same inputs.  `IO` does
not.

The `Maybe`, `Either` and `[]` monads have functions that allow one to
extract values from the monads and pass them down, independent of their
`Monad`:

    tx1 = t "tx1"
         ((\x (Right y) -> x + y) (fromJust (Just 3)) (Right 4)) -- passed down, into +
         7

`tx1` uses `fromJust` and pattern matching (to extract from `Right`)
to extract values from `Maybe` and `Either` monadic values and pass
them down into `+`.  That is fine, even with `IO`, since it doesn't
matter where values given to `+` come from, `+` will always
return the same results for same values:

    txiodown :: IO Int
    txiodown = do
        putStrLn ""
        putStrLn "Enter the number '3':"
        x <- getLine
        putStrLn "Enter the number '4':"
        y <- getLine
        let result = (read x :: Int) + (read y :: Int)
        putStrLn $ "Result: " ++ show result
        return result

`txiodown` uses side-effects (`getLine`) to get values.  Those values
are then extracted from the `IO` `Monad` and given to `+`.  After
printing the result *must* be "wrapped" in the `IO` monad via
`return`.  That is because the result of evaluating `txiodown`
involved real side-effects and any values obtained via real
side-effects must always carry that fact with them in their type.
This makes it easy to determine which parts of a program are purely
functional and which involve side-effects.  This is important since it
can be argued that most bugs arise from entanglements with state and
time.  The pure part of the code are free from such issues.

It is possible, in general, to extract values from monads:

    :t fromJust
    => fromJust :: Maybe a -> a

&nbsp;

    tx2 = t "tx1"
         (fromJust (Just 7))                                     -- passed up/out
         7

`tx2` uses `fromJust` to extract a value from a `Maybe`
monad and lets that value pass up/out to the unit test framework for
comparison with the expected response.

This type of "up/out" extraction is not possible with `IO` because
doing so would break the partitioning of values obtained via
side-effects from pure values mentioned above.

Note: It is possible to extract values from `IO` via:

    :t unsafePerformIO
    => unsafePerformIO :: IO a -> a

and it has been used in the unit tests:

    tx3 = t "tx2"
         (unsafePerformIO txiodown)
         7

See Simon Peyton Jones [Tackling the awkward squad](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/) for why `unsafePerformIO` should rarely be used.

---

# Monad laws

For a typeclass `instance` to be a `Monad` it must satisfy the
following laws:

## left identity

-   `return` only wraps a value.  It does does not change the value and
    does not do anything else in the `Monad`.  Both left and right (see
    below) identity enable the compiler to eliminate `return` calls
    without changing semantics).

&nbsp;

    leftIdentity :: (Eq (m b), Monad m) => a -> (a -> m b) -> Bool
    leftIdentity a f = (return a >>= f) == f a
    
    tli = tt "tli"
          [ leftIdentity  3  ((\x -> Nothing) :: Int -> Maybe Int)
          , leftIdentity  3   (Just   . (+2))
    
          , leftIdentity  3  ((Left   . (+2)) :: Int -> Either Int Int)
          , leftIdentity  3  ((Right  . (+2)) :: Int -> Either Int Int)
    
          , leftIdentity  3   (\x   -> [x*2])
          , leftIdentity  3  ((\x   -> [   ]) :: Int -> [Int])
          ]
          True

## right identity

-   `return` only wraps a value.  It does does not change the value and
    does not do anything else in the `Monad`.

&nbsp;

    rightIdentity :: (Eq (m b), Monad m) => m b -> Bool
    rightIdentity m = (m >>= return) == m
    
    tri = tt "tri"
          [ rightIdentity   (Just  3)
          , rightIdentity   (Nothing  :: Maybe Int)
    
          , rightIdentity   (Left  3  :: Either Int Int)
          , rightIdentity   (Right 3  :: Either Int Int)
    
          , rightIdentity   [3]
          , rightIdentity  ([]        :: [Int])
          ]
          True

## associativity

-   Monadic composition is associative.  This allows an extra `do` block
    to group a sequence of monadic operations.  This allows functions
    that return monadic values to work properly.

&nbsp;

    associativity :: (Eq (m b), Monad m) => m a -> (a -> m a1) -> (a1 -> m b) -> Bool
    associativity m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
    
    tas = tt "tas"
          [ associativity (Just 3)  (\x -> Nothing)  (Just . (*2))
          , associativity (Just 3)  (Just  . (+2))   ((\x -> Nothing) :: Int -> Maybe Int)
          , associativity (Just 3)  (Just  . (+2))    (Just . (*2))
    
          , associativity (Left 3) ((Left  . (+2)) :: Int -> Either Int Int)
                                   ((Left  . (*2)) :: Int -> Either Int Int)
    
          , associativity (Left 3) ((Right . (+2)) :: Int -> Either Int Int)
                                   ((Left  . (*2)) :: Int -> Either Int Int)
    
          , associativity [3]       (\x  -> [   ])    (\x -> [x*2])
          , associativity [3]       (\x  -> [x+2])   ((\x -> [   ])   :: Int -> [Int])
          , associativity [3]       (\x  -> [x+2])    (\x -> [x*2])
          ]
          True

See:

-   <http://www.haskell.org/haskellwiki/Monad_laws>

-   <http://stackoverflow.com/questions/3433608/explanation-of-monad-laws>

---

# see also

-   <http://monads.haskell.cz/html/index.html>

-   <http://mvanier.livejournal.com/3917.html>

---

# example accuracy

    runTests :: IO Counts
    runTests =
        runTestTT $ TestList $ tm1 ++ tm2 ++ tm3 ++ tm4 ++ tm5 ++
                               te1 ++ te2 ++ te3 ++
                               tl1 ++ tl2 ++ tl3 ++
                               ti1 ++
                               tx1 ++ tx2 ++ tx3 ++
                               tli ++ tri ++ tas

&nbsp;

    runTests
    => Counts {cases = 58, tried = 58, errors = 0, failures = 0}

---

# summary and next steps

This article showed

-   the mechanics of several monads

-   pointed out that monads have nothing to do with side-effects (although monads are often used to simulate side-effects)

-   distinguished the "real" side-effects part of the `IO` monad from the monadic part (the part that does sequencing of operations)

-   mentioned the monad laws that a type must satisfy to correctly be a `Monad`

The 2nd article (TBD) in this series will show monads being used to simulate side-effects in a purely functional way.

The 3rd article (TBD) will show how to combine 2 or more transformers.

## source code

The emacs org-mode literate source code of this article is available at:

-   <https://github.com/haroldcarr/learn-haskell-coq-ml-etc/blob/master/haskell/paper/haroldcarr/2013-12-15-concrete-monads-1-maybe-either-list-io.org>
