---
title: verified articles
tags: haskell, hunit
---


For writing articles on Haskell, rather than showing `ghci` input/output like:

    Prelude> map (*2) [1..10]
    [2,4,6,8,10,12,14,16,18,20]

I do the following:

<!-- MORE -->

---

# setup

    import Test.HUnit
    import Test.HUnit.Util  -- https://github.com/haroldcarr/test-hunit-util

---

# article main body

&#x2026;

    t1 = t "t1"
         (map (*2) [1..10])           -- "input"
         [2,4,6,8,10,12,14,16,18,20]  -- "output"

&#x2026; or, if many examples evaluate to same value:

    t2 = tt "t2"
         [(map (*2)          [1..10]) -- "input1"
         ,(map (\x -> x * 2) [1..10]) -- "input2"
         ]
         [2,4,6,8,10,12,14,16,18,20]  -- "output"

---

# example accuracy

Then, in this section at the end of the article, I show the test setup:

    main = do
        runTestTT $ TestList $ t1 ++ t2

and its evaluation:

    main
    => Counts {cases = 3, tried = 3, errors = 0, failures = 0}

## input/output format

Also note, that when I do actually show `ghci` input/output, rather than do:

    *Main> :t t1
    t1 :: [Test]

I do:

    :t t1
    => t1 :: [Test]

---

# code

The code for `t`, `tt` and a couple of other short aliases is at

-   <https://github.com/haroldcarr/test-hunit-util>
