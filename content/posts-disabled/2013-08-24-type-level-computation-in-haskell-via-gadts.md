---
title: Type Level Computation in Haskell via GADTs
tags: haskell, gadt, type-level-computation
---

Issue 22
([pdf](http://themonadreader.files.wordpress.com/2013/08/issue221.pdf))
of [The Monad Reader](http://themonadreader.wordpress.com/) contains
a hands-on tutorial of GADTs in [Haskell](http://www.haskell.org/) by Anton Dergunov.  For me, besides GADTS, it
was an introduction to [type-level computation](http://www.impredicative.com/ur/tutorial/tlc.html) (aka [type arithmetic](http://www.haskell.org/haskellwiki/Type_arithmetic)).

What follows is a compressed summary of the info in that article.
The page numbers refer to Anton's tutorial.

<!-- MORE -->

Notes:

-   The info below is 3rd-hand info, go to Anton's article for 1st and
    2nd-hand info and follow his references for 1st-hand info.

-   The code extracted from below is at GitHub: [gadt.hs](https://github.com/haroldcarr/learn-haskell/blob/master/paper/monad-reader/issue22/gadt.hs).

---

# Intro (p. 5)

[Generalized Algebraic Data Types](http://www.haskell.org/haskellwiki/Generalised_algebraic_datatype) ([GADTs](http://en.wikipedia.org/wiki/Generalized_algebraic_data_type)) generalize algebraic data
types by enabling value constructors to return specific types
(return type must be instance of more general type being defined).

Use-cases:

-   DSLs

-   generic programming

-   ensuring correctness.

Theoretical foundation for GADTs is [dependent types](http://en.wikipedia.org/wiki/Dependent_type).



## (setup)


    {-# LANGUAGE GADTs, ExistentialQuantification, DataKinds, TypeFamilies, TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
    
    import Data.Char
    import Debug.Trace
    
    debug = flip trace

Example [Algebraic Data Type](http://en.wikipedia.org/wiki/Algebraic_data_type) (ADT):

-   `Test`  : type constructor

-   `TI` / `TS` : value constructors
    
    -   each value constructor can specify zero or more components

&nbsp;

    data Test a = TI Int | TS String a

&nbsp;

    :t TI
    -- TI :: Int -> Test a
    
    :t TI 10
    -- TI 10 :: Test a
    
    :t TS
    -- TS :: String -> a -> Test a
    
    :t TS "test" 'c'
    -- TS "test" 'c' :: Test Char

Equivalent version using GADT syntax (p. 7):

    data Test' a where
      TI' :: Int         -> Test' a
      TS' :: String -> a -> Test' a

GADT version that specifies return type:

    data Test'' a where
      TI'' :: Int         -> Test'' Int  -- locks down return type HERE
      TS'' :: String -> a -> Test'' a

    :t TI''
    -- TI'' :: Int -> Test'' Int
    :t TI'' 10
    -- TI'' 10 :: Test'' Int
    
    -- TS'' same as TS
    :t TS'' "foo" 'c'
    -- TS'' "foo" 'c' :: Test'' Char

Key GADT feature: pattern matching causes type refinement, so guaranteed `Int`:

    f :: Test'' a -> a
    f (TI'' i) = i + 10

    f (TI'' 3)
    -- 13

---

# Motivating example: Expression Evaluator (p. 8)

    data IntExpr' = IntVal' Int
                  | AddInt' IntExpr' IntExpr'

    :t AddInt' (IntVal' 5) (IntVal' 7)
    -- AddInt' (IntVal' 5) (IntVal' 7) :: IntExpr'

    evaluate' :: IntExpr' -> Int
    evaluate' e = case e of
        IntVal' i     -> i
        AddInt' e1 e2 -> evaluate' e1 + evaluate' e2

    evaluate' $ AddInt' (IntVal' 5) (IntVal' 7)
    -- 12

Extend with booleans:

    data ExtExpr'' = IntVal''  Int
                   | BoolVal'' Bool
                   | AddInt''  ExtExpr'' ExtExpr''
                   | IsZero''  ExtExpr''

    :t IsZero'' (BoolVal'' True)
    -- IsZero'' (BoolVal'' True) :: ExtExpr''

PROBLEM: type checker accepts:

    :t AddInt'' (IntVal'' 5) (BoolVal'' True)
    -- AddInt'' (IntVal'' 5) (BoolVal'' True) :: ExtExpr''

Since `ExtExpr`'' is NOT parameterized by return value type, evaluation function is complicated:

    evaluate'' :: ExtExpr'' -> Maybe (Either Int Bool)
    evaluate'' e = case e of
        AddInt'' e1 e2 -> case (evaluate'' e1, evaluate'' e2) of
                            (Just (Left i1), Just (Left  i2)) -> Just $ Left $ i1 + i2
                            (Just (Left i1), Just (Right b2)) -> error "wrong type given to AddInt''" -- dynamic type-checking
                            _                                 -> error "not implemented"
        IntVal''  i    -> Just (Left  i)
        BoolVal'' b    -> Just (Right b)
        _              -> error "not implemented"

    evaluate'' $ AddInt'' (IntVal'' 5) (IntVal'' 7)
    -- Just (Left 12)
    
    evaluate'' $ AddInt'' (IntVal'' 5) (BoolVal'' True)
    -- *** Exception: wrong type given to AddInt''

FIX: represent expressions with values of types parameterized by return type (p. 9):

    data PhantomExpr''' t = IntVal'''  Int
                          | BoolVal''' Bool
                          | AddInt'''  (PhantomExpr''' Int) (PhantomExpr''' Int)
                          | IsZero'''  (PhantomExpr''' Int)

`t` above is expr return value type.  Want `IntVal''' 5` to be typed `PhantomExpr''' Int`, but:

    :t IntVal''' 5
    -- IntVal''' 5 :: PhantomExpr''' t
    
    :t BoolVal''' True
    -- BoolVal''' True :: PhantomExpr''' t

PROBLEM: incorrect exprs still accepted by type checker:

    :t IsZero''' (BoolVal''' True)
    -- IsZero''' (BoolVal''' True) :: PhantomExpr''' t

FIX (trick): wrap value constructors with functions:

    intVal'''  :: Int                -> PhantomExpr''' Int
    intVal'''   = IntVal'''
    boolVal''' :: Bool               -> PhantomExpr''' Bool
    boolVal'''  = BoolVal'''
    isZero''   :: PhantomExpr''' Int -> PhantomExpr''' Bool
    isZero''    = IsZero'''

Now bad exprs rejected by type checker (p. 10):

    :t isZero'' (boolVal''' True)
    --    Couldn't match type `Bool' with `Int'
    --    Expected type: PhantomExpr''' Int
    --      Actual type: PhantomExpr''' Bool
    :t isZero'' (intVal''' 5)
    -- isZero'' (intVal''' 5) :: PhantomExpr''' Bool

PROBLEM: Want evaluate type signature to be (p. 10):

    evaluate''' :: PhantomExpr''' t -> t
    evaluate''' (IntVal''' i) = i
    evaluate''' _             = error "not implemented"

but get:

    Couldn't match expected type `t' with actual type `Int'
      `t' is a rigid type variable bound by
          the type signature for evaluate'' :: PhantomExpr''' t -> t
          at r22.hs:150:15
    In the expression: i
    In an equation for evaluate'': evaluate'' (IntVal''' i) = i

because return type of value constructor `IntVal`''' is `Phantom t`
but `t` can be refined to any type:

    :t IntVal''' 5 :: PhantomExpr''' Bool
    -- IntVal''' 5 :: PhantomExpr''' Bool :: PhantomExpr''' Bool

Need to specify type signature of value constructors exactly (so pattern matching will cause type refinement for `IntVal`''' here).
This is exactly what GADTs do.

(Useless) GADT version (all return general type, so no type refinement):

    data PhantomExpr'''' t where
        IntVal''''  :: Int                                        -> PhantomExpr'''' t
        BoolVal'''' :: Bool                                       -> PhantomExpr'''' t
        AddInt''''  :: PhantomExpr'''' Int -> PhantomExpr'''' Int -> PhantomExpr'''' t
        IsZero''''  :: PhantomExpr'''' Int                        -> PhantomExpr'''' t

FIX: Final (useful) GADT version (value constructors return specific types) (p. 11):

    data Expr t where
        IntVal  :: Int                             -> Expr Int
        BoolVal :: Bool                            -> Expr Bool
        AddInt  :: Expr Int  -> Expr Int           -> Expr Int
        IsZero  :: Expr Int                        -> Expr Bool
        If      :: Expr Bool -> Expr t   -> Expr t -> Expr t

Bad exprs rejected:

    :t IsZero (BoolVal True)
    --    Couldn't match type `Bool' with `Int'
    --    Expected type: Expr Int
    --      Actual type: Expr Bool

Specific type returned by `IsZero`:

    :t IsZero (IntVal 5)
    -- IsZero (IntVal 5) :: Expr Bool

Well-defined evaluator / pattern matching causes type refinement:

    evaluate :: Expr t -> t
    evaluate (IntVal i)     = i                           -- right hand side has type Int
    evaluate (BoolVal b)    = b                           -- right hand side has type Bool
    evaluate (AddInt e1 e2) = evaluate e1 + evaluate e2   -- right hand side has type Expr Int
                                                          --       and types of e1 e2 must be Expr Int
    evaluate (IsZero e)     = evaluate e == 0
    evaluate (If e1 e2 e3)  = if evaluate e1 then evaluate e2 else evaluate e3

    AddInt (IntVal 5) (BoolVal True)
    --    Couldn't match type `Bool' with `Int'
    --    Expected type: Expr Int
    --      Actual type: Expr Bool
    
    :t evaluate $ AddInt (IntVal 5) (IntVal 7)
    -- evaluate $ AddInt (IntVal 5) (IntVal 7) :: Int
    
    evaluate $ AddInt (IntVal 5) (IntVal 7)
    -- 12

---

# Generic Programming with GADTs (p. 12)

[Datatype-generic](http://en.wikipedia.org/wiki/Generic_programming) : functions take types as an arg, change behavior depending on type.

Example: encode data in binary form (can be done with type classes too).

Representation type whose values represent types:

    data Type t where
        TInt  :: Type Int
        TChar :: Type Char
        TList :: Type t -> Type [t]
        TDyn  :: Type Dynamic        -- not used until p. 14 in exposition

    :t TInt
    -- TInt :: Type Int
    :t TList
    -- TList :: Type t -> Type [t]
    :t TList TInt
    -- TList TInt :: Type [Int]

Since Haskell `String` is `[Char]`, define value constructor:

    tString :: Type String
    tString = TList TChar

Output of encoding function is list of bits:

    data Bit = F | T deriving (Eq, Show)

Encoding function (p. 13):


    encode :: Type t -> t -> [Bit]
    encode TInt i           = encodeInt i
    encode TChar c          = encodeChar c
    -- note T consed on front and F on end as separators
    encode (TList _) []     = F : []
    encode (TList t) (x:xs) = T : (encode t x) ++ encode (TList t) xs
    encode TDyn (Dyn t v)   = encode t v                               -- not used until p. 14

<http://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell> :

    encodeInt :: Int -> [Bit]
    encodeInt 0 = [F]
    encodeInt n = reverse $ helper n
        where helper 0 = []
              helper n = let (q,r) = n `divMod` 2 in (mkBit r) : helper q
              mkBit  i = if i == 1 then T else F
    
    encodeChar :: Char -> [Bit]
    encodeChar c = encodeInt $ ord c

    encodeInt 0 == [F]
    encodeInt 5 == [T,F,T]
    encode TInt 331 == [T,F,T,F,F,T,F,T,T]
    encode TInt 333 == [T,F,T,F,F,T,T,F,T]
    -- Note: paper shows [T,F,T,...,F,F,F] for this
    
    encode TInt 1 == [T]
    encode TInt 2 == [T,F]
    encode TInt 3 == [T,T]
    
        encode TInt 1 ++      encode TInt 2 ++       encode TInt 3         == [  T,  T,F,  T,T]
    T : encode TInt 1 ++ (T : encode TInt 2) ++ (T : encode TInt 3) ++ [F] == [T,T,T,T,F,T,T,T,F]
    encode (TList TInt) [1,2,3]                                            == [T,T,T,T,F,T,T,T,F]
    -- Note: paper shows [T,T,F,...,F,F,F]

[Universal Data Type](http://thid.thesa.com/thid-0513-0671-th-1425-3196) : Organize around single universal type
(e.g., APL/real number arrays; SNOBOL/strings; LISP/lists; fun prog/exprs, Object/Java - except unboxed primitives).

Pair representation type with value (requires `ExistentialQuantification`) (p. 13):

    data Dynamic' = forall t. Dyn' (Type t) t

Previous defines [existential data type](http://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types):
way of "squashing" a group of types into one, single type (in Haskell).

Can also be represented as GADT:

    data Dynamic where
        Dyn :: Type t -> t -> Dynamic
    
    encode' :: Dynamic -> [Bit]
    encode' (Dyn t v) = encode t v

    let c = Dyn (TList TInt) [1,2,3]
    :t c
    -- c :: Dynamic
    encode' c == encode (TList TInt) [1,2,3]
    encode' c == [T,T,T,T,F,T,T,T,F]

Define heterogeneous lists (p. 14):

    let d = [Dyn TInt 10, Dyn tString "test"]
    :t d
    --      [Dyn TInt 10, Dyn tString "test"] :: [Dynamic]
    -- (Note: paper had : Dyn TString "test")

But cannot make this list `Dynamic`.

FIX:

-   extend representation, adding value constructor (done above in `data Type t`).

-   add to patterns of encode functions (done above in `encode TDyn`).

&nbsp;

    let e = Dyn (TList TDyn) d
    :t e
    -- e :: Dynamic
    encode' e == [T,T,F,T,F,T,T,T,T,T,F,T,F,F,T,T,T,F,F,T,F,T,T,T,T,T,F,F,T,T,T,T,T,T,F,T,F,F,F,F]

Dynamic data type is useful for communication with env when type not known in advance.
Then a type cast is required (p. 14):

    castInt :: Dynamic -> Maybe Int
    castInt (Dyn TInt i) = Just i
    castInt (Dyn _    _) = Nothing

More generic solution that works for all types referenced (but not shown) in paper.

Conclusion:

-   PRO: generic programming possible

-   CON: must extend representation type whenever define new data type

---

# Proving Correctness of List Operations (p. 15)

Types can ensure only a non-empty List is passed to `head`.
Types can encode other properties: e.g., non-empty lists; lists of certain length.

    -- ADT:
    -- data List t = Nil | Cons t (List t)
    
    -- GADT:
    data List t where
        Nil  ::                List t
        Cons :: t -> List t -> List t
    
    listHead :: List t -> t
    listHead (Cons a _) = a
    listHead Nil        = error "empty list"

## Encode empty/non-empty list in type

To ensure no failure, define non-empty lists:

    data Empty
    data NonEmpty
    
    -- param f is Empty when list is empty, NonEmpty otherwise
    data SafeList' t f where
        Nil'  ::                       SafeList' t Empty
        Cons' :: t -> SafeList' t f -> SafeList' t NonEmpty
    
    -- head that can ONLY take non-empty lists (p. 16):
    headSafe' :: SafeList' t NonEmpty -> t
    headSafe' (Cons' t _) = t

    headSafe' Nil'
    --    Couldn't match type `Empty' with `NonEmpty'
    --    Expected type: SafeList' t0 NonEmpty
    --      Actual type: SafeList' t0 Empty
    let hs = Cons' 1 $ Cons' 2 $ Cons' 3 Nil'
    :t hs
    hs :: SafeList' Integer NonEmpty
    headSafe' hs
    -- 1

PROBLEM:

    repeatElem' :: a -> Int -> SafeList' a ???
    repeatElem' a 0 = Nil'
    repeatElem' a n = Cons' a (repeatElem a (n-1))

Cannot determine return type because `Empty` / `NonEmpty` lists have completely different types.

FIX: relax `Cons`' value constructor:

    data SafeList'' t f where
        Nil''  ::                        SafeList'' t Empty
        Cons'' :: t -> SafeList'' t f -> SafeList'' t f'     -- note f'

Now `SafeList t Empty` is a type of possibly empty lists:

    :t Nil''
    -- Nil'' :: SafeList'' t Empty
    :t Cons'' 'a' Nil''
    -- Cons'' 'a' Nil'' :: SafeList'' Char f'
    :t Cons'' 'a' Nil'' :: SafeList'' Char Empty
    -- Cons'' 'a' Nil'' :: SafeList'' Char Empty    :: SafeList'' Char Empty
    :t Cons'' 'a' Nil'' :: SafeList'' Char NonEmpty
    -- Cons'' 'a' Nil'' :: SafeList'' Char NonEmpty :: SafeList'' Char NonEmpty

Now can define (p. 17):

    repeatElem'' :: a -> Int -> SafeList'' a Empty
    repeatElem'' a 0 = Nil''
    repeatElem'' a n = Cons'' a (repeatElem'' a (n-1))

    -- note: cannot Show it
    let a = repeatElem'' 'c' 3
    :t a
    -- a :: SafeList'' Char Empty

PROBLEM: anything can slip through `f`':

    :t Cons'' 'a' Nil'' :: SafeList'' Char Bool
    -- Cons'' 'a' Nil'' :: SafeList'' Char Bool :: SafeList'' Char Bool
    
    :t Cons'' 'a' Nil'' :: SafeList'' Char Int
    -- Cons'' 'a' Nil'' :: SafeList'' Char Int :: SafeList'' Char Int

FIX: give `Empty` / `NonEmpty` same kind.   Discussed later for `Nat`.

## Encode list length in type

Stronger invariant: list length (p. 17):

Note:  `Empty` / `NonEmpty` not enough.  Need to encode length in type.


(Requires `DataKinds`.)

    -- Peano numbers
    data Zero'''
    data Succ''' n
    
    data List''' a n where
        Nil'''  ::                     List''' a Zero'''
        Cons''' :: a -> List''' a n -> List''' a (Succ''' n)
    
    headSafe''' :: List''' t (Succ''' n) -> t
    headSafe''' (Cons''' t _) = t
    
    -- type encode that map does not change length
    mapSafe''' :: (a -> b) -> List''' a n -> List''' b n
    mapSafe''' _         Nil''' = Nil'''
    mapSafe''' f (Cons''' x xs) = Cons''' (f x) (mapSafe''' f xs)

    let hs = headSafe''' $ Cons''' 1 $ Cons''' 2 $ Nil'''
    :t hs
    -- hs :: Integer
    
    let ms = mapSafe''' (\x -> x + 1) $ Cons''' 1 $ Cons''' 2 $ Nil'''
    :t ms
    -- ms :: List''' Integer (Succ''' (Succ''' Zero'''))


To implement concatenation need type-level Peano addition.

-   One way: type families (here understood as type-level function)

-   Requires `TypeFamilies`

-   p. 18

&nbsp;

    type family Plus''' a b
    type instance Plus''' Zero'''     n = n
    type instance Plus''' (Succ''' m) n = Succ''' (Plus''' m n)
    
    concatenate''' :: List''' a m -> List''' a n -> List''' a (Plus''' m n)
    concatenate''' Nil''' ys = ys
    concatenate''' (Cons''' x xs) ys = Cons''' x (concatenate''' xs ys)

&nbsp;

    let c = concatenate''' (Cons''' 1 $ Cons''' 2 $ Nil''') (Cons''' 3 $ Cons''' 4 $ Nil''')
    :t c
    -- c :: List''' Integer (Succ''' (Succ''' (Succ''' (Succ''' Zero'''))))

PROBLEM: `Succ` has a type parameter of `kind *`.

-   allows nonsense: `Succ Int`


FIX: Types classify values.  Kinds classify types.  So declare a new kind:

-   `Nat`' is a type, `Zero`' / `Succ`' are value constructors.

-   But, due to promotion, `Nat`' also a kind; `Zero`' / `Succ`' also types.

-   Sometimes necessary to prepend quote (e.g., '`Succ`') to refer to **type** (not value constructor)

&nbsp;

    data Nat'''' = Zero'''' | Succ'''' Nat''''

&nbsp;

    -- Type-level representation of number two (although prepended quote not necessary here):
    type    Two = 'Succ'''' ('Succ'''' 'Zero'''')
    :i Two
    -- type Two = 'Succ'''' ('Succ'''' 'Zero'''')

Now `Succ Int` will be rejected.

Specify type of second parameter has kind Nat (p. 19):

    data List'''' a (n::Nat'''') where
        Nil''''  ::                      List'''' a 'Zero''''
        Cons'''' :: a -> List'''' a n -> List'''' a ('Succ'''' n)

PROBLEM: But can't write return type for:

    repeatElem'''' :: a -> Int -> List'''' ????

Need count both a runtime and type-check time.

FIX: singleton types (types with only one value other than bottom):

    data NatSing (n::Nat'''') where
        ZeroSing ::              NatSing 'Zero''''
        SuccSing :: NatSing n -> NatSing ('Succ'''' n)


`NatSing` constructors mirror `Nat`'''' constructors.
Thus every TYPE of kind `Nat` corresponds to exactly **one** VALUE of the singleton data type where parameter `n` has exactly this type.

    :t ZeroSing
    -- ZeroSing :: NatSing 'Zero''''
    
    :t SuccSing $ SuccSing ZeroSing
    -- SuccSing $ SuccSing ZeroSing :: NatSing ('Succ'''' ('Succ'''' 'Zero''''))

Can now define:

    repeatElem'''' :: a -> NatSing n -> List'''' a n
    repeatElem'''' _ ZeroSing     = Nil''''
    repeatElem'''' x (SuccSing n) = Cons'''' x (repeatElem'''' x n)  -- note: subtraction done by structural induction

    let re = repeatElem'''' 'C' (SuccSing $ SuccSing ZeroSing)
    :t re
    -- re :: List'''' Char ('Succ'''' ('Succ'''' 'Zero''''))

## Encode length comparison in type

Example: do not exceed list length

Requires `TypeOperators`

Requires type-level magnitude comparison function (defined by structural induction):

    type family   (m::Nat'''')  :< (n::Nat'''') :: Bool
    type instance  m            :< 'Zero''''     = 'False
    type instance 'Zero''''     :< ('Succ'''' n) = 'True
    type instance ('Succ'''' m) :< ('Succ'''' n) = m :< n

-   given
    
    -   list of length      `m`
    
    -   index  `n`

-   ensure           `n :< m`

-   note: `~` is equality constraint

&nbsp;

    nthElem'''' :: (n :< m) ~ 'True => List'''' a m -> NatSing n -> a
    nthElem'''' (Cons'''' x  _) ZeroSing     = x
    nthElem'''' (Cons'''' _ xs) (SuccSing n) = nthElem'''' xs n

&nbsp;

    let ne = nthElem'''' (repeatElem'''' 'C' (SuccSing $ SuccSing ZeroSing)) (SuccSing $ SuccSing ZeroSing)
    --    Couldn't match type 'False with 'True
    --    Expected type: 'True
    --      Actual type: 'Succ'''' ('Succ'''' 'Zero'''')
    --                   :< 'Succ'''' ('Succ'''' 'Zero'''')
    
    let ne = nthElem'''' (repeatElem'''' 'C' (SuccSing $ SuccSing ZeroSing))            (SuccSing ZeroSing)
    :t ne
    -- ne :: Char

## LIST SUMMARY (p. 21):

-   Used GADTs to specify correctness of list operations verified by type-checker.

-   Specified necessary properties in the data type.

-   Set of properties motivated by the actual operations to be performed.

-   `head` : only needed `Empty` / `NonEmpty`

-   Other operations need count of elements it contains.


---

# Proving Correctness of Red-Black Tree Insert (p. 21)

-   [Red-Black Trees](http://en.wikipedia.org/wiki/Red%E2%80%93black_tree)

-   [Stephanie Weirich's](http://www.seas.upenn.edu/~sweirich/)
    
    -   slides ([pdf](http://www.seas.upenn.edu/~sweirich/talks/flops2012.pdf)) for reference (p. 33)
    
    -   [course/code](http://www.seas.upenn.edu/~cis552/12fa/schedule.html) - scroll down to RedBlack[1|2|3]

&nbsp;

    data Color   = R | B deriving (Eq, Show)
    data Node' a = E' | N' Color (Node' a) a (Node' a)
    type Tree' a = Node' a

For any node `N c l x r`, values less than `x` are stored in `l`, otherwise `r`:

    member' :: Ord a => a -> Tree' a -> Bool
    member' _ E' = False
    member' x (N' _ l a r)
        | x < a = member' x l
        | x > a = member' x r
        | otherwise = True

Invariants (guarantee tree is balanced) (p. 22)

-   ensure longest path from root
    
    -   containing alternating red-black nodes)

-   can only be twice as long as the shortest path
    
    -   containing only red nodes.

Ensure operations take *O* (log *n* ) time,
(where *n* is number of elements) in worst case.

1.  Root is black.

2.  Leafs are black.

3.  Red nodes have black children.

4.  *Black Height*: For each node, all paths from that node to leaf
    contain same number of black nodes.

&nbsp;

    insert' :: Ord a => Tree' a -> a -> Tree' a
    insert' t v = blacken (insert'' t v) where
        insert'' n@(N' c l a r) x
            | x < a = leftBalance'  (N' c (insert'' l x) a           r)
            | x > a = rightBalance' (N' c           l    a (insert'' r x))
            | otherwise = n
        insert''    E'     x    = N' R E' x E'
        blacken    (N' _ l x r) = N' B l  x r

Same recursive descent to leaf nodes as binary search trees, except
ensuring invariants:

-   4: red node inserted

-   1: blacken root

-   3: `leftBalance` / `rightBalance`

&nbsp;

    leftBalance' :: Node' a -> Node' a
    leftBalance' (N' B (N' R (N' R a x       b) y       c)  z d) =
                  N' R (N' B       a x       b) y (N' B c   z d)
    leftBalance' (N' B (N' R       a x (N' R b  y       c)) z d) =
                  N' R (N' B       a x       b) y (N' B c   z d)
    leftBalance' n = n
    
    rightBalance' :: Node' a -> Node' a
    rightBalance' (N' B       a x (N' R       b  y (N' R c  z d))) =
                   N' R (N' B a x             b) y (N' B c  z d)
    rightBalance' (N' B       a x (N' R (N' R b  y       c) z d))  =
                   N' R (N' B a x             b) y (N' B c  z d)

## Proving 4th invariant maintained by insert (p. 23)

Add black height:

    data Nat = Zero | Succ Nat deriving (Eq, Show)

    {-
    data Node a (bh::Nat) where
        -- leaf has bh 0
        E :: Node a 'Zero
        -- bh must be conditionally incremented based on color
        N :: Color -> Node a bh -> a -> Node a bh -> Node a ???
    -}

Increment done via type family (requires `TypeFamilies`, `DataKinds`) (p. 24):

    type family IncBH (c::Color) (bh::Nat) :: Nat
    type instance IncBH R bh =      bh
    type instance IncBH B bh = Succ bh

Requires color to be passed as type (for `IncBH`) and as a value (for
`Node` value constructor).  Use singleton type as bridge:

    data ColorSingleton (c::Color) where
        SR :: ColorSingleton R
        SB :: ColorSingleton B
    
    instance Show (ColorSingleton c) where
        show SR = "R"
        show SB = "B"

Value of singleton type passed to `Node` value constructor and
color type used for `IncBH`:

    data Node4 a (bh::Nat) where
        E4 :: Node4 a 'Zero
        N4 :: ColorSingleton c -> Node4 a bh -> a -> Node4 a bh
                               -> Node4 a (IncBH c bh)

In Haskell, when creating a new type, every type variable on
right-hand side of definition must also appear on left-hand
side. Therefore (p. 24):

PROBLEM: cannot write:

    type Tree4 a = Node4 a bh
    #+BEGIN_EXAMPLE
    
    FIX 1: use /existential types/ (requires =RankNTypes=):
    
    #+BEGIN_EXAMPLE
    type Tree4 a = forall bh. Node4 a bh

FIX 2: GADT:

    data Tree4 a where
        Root4 :: Node4 a bh -> Tree4 a

`insert` same as above except type annotations (p. 36):

    insert4 :: Ord a => Tree4 a -> a -> Tree4 a
    insert4 (Root4 t) v = blacken (insert' t v) where
        insert' :: Ord a => Node4 a n -> a -> Node4 a n
        insert' n@(N4 c l a r) x
            | x < a = leftBalance4  (N4 c (insert' l x) a          r)
            | x > a = rightBalance4 (N4 c          l    a (insert' r x))
            | otherwise = n
        insert'    E4     x    =        N4 SR E4 x E4
        blacken   (N4 _ l x r) = Root4 (N4 SB l x r)
    
    
    leftBalance4  :: Node4 a bh -> Node4 a bh
    leftBalance4  (N4 SB (N4 SR (N4 SR a x       b) y        c)  z d) =
                   N4 SR (N4 SB       a x        b) y (N4 SB c   z d)
    leftBalance4  (N4 SB (N4 SR       a x (N4 SR b  y        c)) z d) =
                   N4 SR (N4 SB       a x        b) y (N4 SB c   z d)
    leftBalance4 n = n
    
    rightBalance4 :: Node4 a bh -> Node4 a bh
    rightBalance4 (N4 SB        a x (N4 SR        b  y (N4 SR c  z d))) =
                   N4 SR (N4 SB a x               b) y (N4 SB c  z d)
    rightBalance4 (N4 SB        a x (N4 SR (N4 SR b  y        c) z d))  =
                   N4 SR (N4 SB a x               b) y (N4 SB c  z d)

## Proving 3rd invariant maintained by insert (p. 25)

Valid colors for a node on type level.  Can be done via type families
(as above) or type classes (here) (requires `MultiParamTypeClasses`):

    class ValidColors (parent::Color) (child1::Color) (child2::Color)

Functions not needed on `ValidColors`, just valid instance (requires `FlexibleInstances`):

    instance ValidColors R B  B  -- red with only black children
    instance ValidColors B lc rc -- black with children of any color

Add color type as param to `Node` and restrict to `ValidColors`
(also ensures 2nd invariant):

    data Node a (bh::Nat) (c::Color) where
        E :: Node a 'Zero B
        N :: ValidColors c lc rc => ColorSingleton c
               -> Node a bh lc -> a -> Node a bh rc
                  -> Node a (IncBH c bh) c
    
    instance Show a => Show (Node a b c) where
        show  E          = "eb"
        show (N c l x r) = "(N"
                             ++ " " ++ (show c)
                             ++ " " ++ (show l)
                             ++ " " ++ (show x)
                             ++ " " ++ (show r)
                             ++ ")"

Root of `Tree` is black (1st invariant):

    data Tree a where
        Root :: Node a bh B -> Tree a
    
    instance Show a => Show (Tree a) where
        show (Root t) = "(Root " ++ show t ++")"

Insert can temporarily invalidate 3rd invariant.
So cannot use `Tree`.  Instead a `Node` with color restrictions:

    data IntNode a (n::Nat) where
        IntNode :: ColorSingleton c
                     -> Node a n c1 -> a -> Node a n c2
                        -> IntNode a (IncBH c n)

Update type of `insert` functions (p. 26/38):

    insert :: Ord a => Tree a -> a -> Tree a
    insert (Root t) v = blacken (insert' t v) where
        insert' :: Ord a => Node a n c -> a -> IntNode a n
        insert'     n@(N c l a r) x
            | x < a = leftBalance  c (insert' l x) a          r    `debug` "i<"
            | x > a = rightBalance c          l    a (insert' r x) `debug` "i>"
            | otherwise = IntNode  c          l    a          r    `debug` "i="
        insert'        E          x =
                          IntNode  SR         E    x          E    `debug` "iE"
        blacken (IntNode _ l x r) =
                          Root  (N SB         l    x          r)   `debug` "blacken"

Before, passed whole `Node` as param.
But 3rd invariant can be temporarily violated.
So explicitly pass params of `Node` and left child
using `IntNode`:

    leftBalance :: ColorSingleton c
                   -> IntNode a n -> a -> Node a n c'
                      -> IntNode a (IncBH c n)
    -- 1:
    leftBalance         SB (IntNode SR (N SR a              x       b) y       c)   z d =
                IntNode SR (N       SB       a              x       b) y (N SB c    z d)    `debug` "lb1"
    -- 2:
    leftBalance         SB (IntNode SR       a              x (N SR b  y       c))  z d =
                IntNode SR (N       SB       a              x       b) y (N SB c    z d)    `debug` "lb2"
    -- 3:
    -- tree balanced, but need to change type from IntNode to Node:
    leftBalance         c  (IntNode SB       a              x       b)              z d =
                IntNode c  (N       SB       a              x       b)              z d     `debug` "lb3"
    -- 4:
    -- red nodes must have black children
    leftBalance         c  (IntNode SR       a@(N SB _ _ _) x       b@(N SB _ _ _)) z d =
                IntNode c  (N       SR       a              x       b)              z d     `debug` "lb4"
    -- 5:
    -- red nodes must have black children
    leftBalance         c  (IntNode SR       E              x          E)           z d =
                IntNode c  (N       SR       E              x          E)           z d     `debug` "lb5"
    
    -- cannot happen, but not enough type info to omit:
    leftBalance         _  (IntNode SR        (N SR _ _ _)  _          _)           _ _ =
                    error "cannot happen"
    leftBalance         _  (IntNode SR      _               _         (N SR _ _ _)) _ _ =
                    error "cannot happen"
    
    -- The case of one regular node and one leaf node is not valid,
    -- because nodes have different black heights
    -- so no need to look for that case.

p. 38

    rightBalance :: ColorSingleton c
                   -> Node a n c' -> a -> IntNode a n
                      -> IntNode a (IncBH c n)
    -- 1:
    rightBalance               SB a x (IntNode SR       b              y (N SR c  z d)) =
                 IntNode SR (N SB a x                   b)             y (N SB c  z d)    `debug` "rb1"
    -- 2:
    rightBalance               SB a x (IntNode SR (N SR b              y       c) z d)  =
                 IntNode SR (N SB a x                   b)             y (N SB c  z d)    `debug` "rb2"
    -- 3:
    rightBalance         c        a x (IntNode SB       b              y            d)  =
                 IntNode c        a x (N       SB       b              y            d)    `debug` "rb3"
    -- 4:
    rightBalance         c        a x (IntNode SR       b@(N SB _ _ _) y            d@(N SB _ _ _)) =
                 IntNode c        a x (N       SR       b              y            d)    `debug` "rb4"
    -- 5:
    rightBalance         c        a x (IntNode SR  E                   y  E)            =
                 IntNode c        a x (N       SR  E                   y  E)              `debug` "rb5"
    
    rightBalance _                _ _ (IntNode SR (N SR _ _ _)         _  _)            =
                 error "cannot happen"
    rightBalance _                _ _ (IntNode SR _                    _ (N SR _ _ _))  =
                 error "cannot happen"

## Red-Black Trees in Action

    member :: Ord a => a -> Tree a -> Bool
    member x (Root t) = mem x t where
        mem :: Ord a => a -> Node a bh c -> Bool
        mem x E = False
        mem x (N _ l y r)
            | x < y     = mem x l
            | x > y     = mem x r
            | otherwise = True
    
    elements :: Ord a => Tree a -> [a]
    elements (Root t) = aux t [] where
        aux :: Ord a => Node a bh c -> [a] -> [a]
        aux E xs = xs
        aux (N _ l y r) xs = aux l (y : aux r xs)

    member 100 $                         insert (Root E) 100
    member   0 $                 insert (insert (Root E) 100) 101
    member 100 $         insert (insert (insert (Root E) 100) 101) 1
    member   0 $ insert (insert (insert (insert (Root E) 100) 101) 1) 0
    elements   $ insert (insert (insert (insert (Root E) 100) 101) 1) 0
    -- [0,1,100,101]

<http://cs.lmu.edu/~ray/notes/redblacktrees/>

In progress &#x2026;

    let root  = (Root E)
    
    (Root eb)
    
    let one   = insert root   4
    
    iE
    blacken
    (Root (N B eb 4 eb))

    let two   = insert one    7
    
    i>
    iE
    rb5
    blacken
    (Root (N B eb 4 (N R eb 7 eb)))

    let three = insert two   12
    
    i>
    i>
    iE
    rb5
    rb1
    blacken
    (Root (N B (N B eb 4 eb) 7 (N B eb 12 eb)))

    let four  = insert three 15

    let five  = insert four   3

    let six   = insert five   5

    let seven = insert six   14

    let eight = insert seven 18

    let nine  = insert eight 16

    let ten   = insert nine  17

## Red-Black Tree proofs in Agda and Coq

-   [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php)
    
    -   See [Dan Licata's](http://www.cs.cmu.edu/~drl/) lecture videos at
        [Oregon Programming Languages Summer School 2013](http://www.cs.uoregon.edu/research/summerschool/summer13/curriculum.html)
        (scroll down)

-   [Coq](http://coq.inria.fr/)
    
    -   In [Ada Chlipala's](http://adam.chlipala.net/)
        *Certified Programming with Dependent Types*
        [MoreDep](http://adam.chlipala.net/cpdt/html/MoreDep.html)
        chapter (scroll down)

---

table of contents (non-functional)

<div id="table-of-contents">
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-3">Motivating example: Expression Evaluator (p. 8)</a></li>
<li><a href="#sec-4">Generic Programming with GADTs (p. 12)</a></li>
<li><a href="#sec-5">Proving Correctness of List Operations (p. 15)</a>
<ul>
<li><a href="#sec-5-1">Encode empty/non-empty list in type</a></li>
<li><a href="#sec-5-2">Encode list length in type</a></li>
<li><a href="#sec-5-3">Encode length comparison in type</a></li>
<li><a href="#sec-5-4">LIST SUMMARY (p. 21):</a></li>
</ul>
</li>
<li><a href="#sec-6">Proving Correctness of Red-Black Tree Insert (p. 21)</a>
<ul>
<li><a href="#sec-6-1">Proving 4th invariant maintained by insert (p. 23)</a></li>
<li><a href="#sec-6-2">Proving 3rd invariant maintained by insert (p. 25)</a></li>
<li><a href="#sec-6-3">Red-Black Trees in Action</a></li>
<li><a href="#sec-6-4">Red-Black Tree proofs in Agda and Coq</a></li>
</ul>
</li>
</ul>
</div>
</div>
