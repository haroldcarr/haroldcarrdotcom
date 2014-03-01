---
title: using graphviz via haskell
tags: haskell, graphviz, shelly
---

One of the tools I use for drawing graphs is `dot` from [Graphviz](http://www.graphviz.org/).
Recently I was drawing a series of diagrams that have mostly identical
parts.  It was error-prone and a pain to keep the identical parts in
sync between the multiple `*.dot` files.  I found [Haskell](http://www.haskell.org/)'s
[Data.GraphViz](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz.html) package to be the solution.

The documentation for that package is good, but it needs a few more
examples.  The purpose of this post is to add examples to the pool.

<!-- MORE -->

## setup

This post uses the following extension and imports:

    {-# LANGUAGE OverloadedStrings #-}
    
    import           Data.Graph.Inductive
    import           Data.GraphViz
    import           Data.GraphViz.Attributes.Colors.Brewer
    import           Data.GraphViz.Attributes.Complete
    import           Data.GraphViz.Types.Generalised        as G
    import           Data.GraphViz.Types.Monadic
    import           Data.Text.Lazy                         as L
    import           Data.Word
    import           Shelly                                 (shelly)
    import           WriteRunDot

---

# creating dot graphs from Data.Graph.Inductive input

When searching for `Data.GraphViz` examples one of the most useful I found was

-   <http://speely.wordpress.com/2010/09/17/haskell-graphs-and-underpants/>

-   <https://github.com/mcandre/mcandre/blob/master/haskell/gnomes.hs> &#x2014; more up-to-date

It shows how to turn [Data.Graph.Inductive](http://hackage.haskell.org/package/fgl-5.4.2.4/docs/Data-Graph-Inductive.html) graphs into dot graphs.
The input:

    ex1 :: Gr Text Text
    ex1 = mkGraph [ (1,"one")
                  , (3,"three")
                  ]
                  [ (1,3,"edge label") ]
    
    ex1Params :: GraphvizParams n L.Text L.Text () L.Text
    ex1Params = nonClusteredParams { globalAttributes = ga
                                   , fmtNode          = fn
                                   , fmtEdge          = fe
                                   }
      where fn (_,l)   = [(Label . StrLabel) l]
            fe (_,_,l) = [(Label . StrLabel) l]
    
            ga = [ GraphAttrs [ RankDir   FromLeft
                              , BgColor   [toWColor White]
                              ]
                 , NodeAttrs  [ Shape     BoxShape
                              , FillColor (pastel28CL 2)
                              , Style     [SItem Filled []]
                              ]
                 ]

along with some color helper functions:

    pastel28CL :: Word8 -> ColorList
    pastel28CL n = toColorList [toColor (BC (BScheme Pastel2 8) n)]
    
    pastel28 :: Word8 -> Attribute
    pastel28 n = Color $ pastel28CL n

results in:

![&nbsp;](../images/2014-02-28-using-graphviz-via-haskell-ex1.png)

---

# creating dot graphs from a Data.GraphViz Haskell representation

Besides supporting `Data.Graph.Inductive`, `Data.GraphViz` provides
several Haskell [representations](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types.html):
[canonical](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types-Canonical.html),
[generalized](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types-Generalised.html),
[graph](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types-Graph.html) and
[monadic](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types-Monadic.html).
The `monadic` representation looks very similar to [dot notation](http://www.graphviz.org/content/dot-language):

    ex2 :: G.DotGraph L.Text
    ex2 = digraph (Str "ex2") $ do
    
        graphAttrs [RankDir FromLeft]
        nodeAttrs  [style filled]
    
        cluster (Int 0) $ do
            node "Ready"               [textLabel "ready"
                                       , Shape DoubleCircle, pastel28 1, FixedSize True, Width 1]
        cluster (Int 1) $ do
            graphAttrs [textLabel "active"]
            node "Open"                [textLabel "open"
                                       , Shape       Circle, pastel28 2, FixedSize True, Width 1]
            node "OpenExpectFragment"  [textLabel "open expect\nfragment"
                                       , Shape       Circle, pastel28 2, FixedSize True, Width 1]
            node "HalfClosed"          [textLabel "half-clsd"
                                       , Shape       Circle, pastel28 2, FixedSize True, Width 1]
            node "endMessage?"         [textLabel "end req?"
                                       , Shape DiamondShape, pastel28 6, FixedSize True, Width 1.25, Height 1.25]
            node "fragmentEndMessage?" [textLabel "end req?"
                                       , Shape DiamondShape, pastel28 6, FixedSize True, Width 1.25, Height 1.25]
            node "requestFragment"     [textLabel "FRAGMENT"
                                       , Shape     BoxShape, pastel28 5]
    
            "Open"                     --> "endMessage?"
            edge "endMessage?"             "HalfClosed"          [textLabel "true"]
            edge "endMessage?"             "OpenExpectFragment"  [textLabel "false"]
            "OpenExpectFragment"       --> "requestFragment"
            "requestFragment"          --> "fragmentEndMessage?"
            edge "fragmentEndMessage?"     "OpenExpectFragment"  [textLabel "false"]
            edge "fragmentEndMessage?"     "HalfClosed"          [textLabel "true"]
    
        cluster (Int 2) $ do
            graphAttrs [textLabel "done"]
            node "Closed"              [textLabel "closed"
                                       , Shape DoubleCircle, pastel28 1, FixedSize True, Width 1]
    
        -- outside the box(es)
        node "request"                 [textLabel "REQUEST"
                                       , Shape     BoxShape, pastel28 5]
        node "response"                [textLabel "RESPONSE"
                                       , Shape     BoxShape, pastel28 5]
    
        "Ready"      --> "request"
        "request"    --> "Open"
    
        "HalfClosed" --> "response"
        "response"   --> "Closed"

The above results in (a diagram for the beginnings of a simple wire
protocol with possibly fragmented request messages and single response
messages):

![&nbsp;](../images/2014-02-28-using-graphviz-via-haskell-ex2.png)

---

# enhancement request

Quite often I create diagrams that do not use clustering but have
different node types, each type with a distinct shape, size and color.
In dot, one can factor the shared attributes via `subgraph`:

    digraph exe {
        graph [rankdir=LR];
        subgraph {
            node [shape=doublecircle,fixedsize=true,width=1,style=filled,color="/pastel28/1"];
            Open [label=open];
            Closed [label=closed];
        }
        subgraph {
            node [shape=circle,fixedsize=true,width=1,style=filled,color="/pastel28/1"];
            ClosedWaitingAck [label="clsd waiting\nACK"];
        }
        subgraph {
            node [shape=box,width=1,style=filled,color="/pastel28/5"];
            cancel [label=CANCEL];
            cancelAck [label=CANCEL_ACK];
        }
        Open -> cancel;
        cancel -> ClosedWaitingAck;
        ClosedWaitingAck -> cancelAck;
        cancelAck -> Closed;
    }

which results in:

![&nbsp;](../images/2014-02-28-using-graphviz-via-haskell-ex3.dot.png)

Unfortunately `Data.GraphViz` only supports clustering (or at least I
have not found `subgraph` support yet). The dot output above was
produced from:

    ex3 :: G.DotGraph L.Text
    ex3 = digraph (Str "exe") $ do
    
        graphAttrs [RankDir FromLeft]
    
        cluster (Int 0) $ do
            nodeAttrs               [Shape DoubleCircle, FixedSize True, Width 1, style filled, pastel28 1]
            node "Open"             [textLabel "open"]
            node "Closed"           [textLabel "closed"]
    
        cluster (Int 1) $ do
            nodeAttrs               [Shape       Circle, FixedSize True, Width 1, style filled, pastel28 1]
            node "ClosedWaitingAck" [textLabel "clsd waiting\nACK"]
    
        cluster (Int 2) $ do
            nodeAttrs               [shape     BoxShape,                 Width 1, style filled, pastel28 5]
            node "cancel"           [textLabel "CANCEL"]
            node "cancelAck"        [textLabel "CANCEL_ACK"]
    
        "Open"             --> "cancel"
        "cancel"           --> "ClosedWaitingAck"
        "ClosedWaitingAck" --> "cancelAck"
        "cancelAck"        --> "Closed"

which almost has what I want as output:

    digraph exe {
        graph [rankdir=LR];
        subgraph cluster_0 {
            ...

Manually removing the `cluster_N` after `subgraph` gives me what I want.

If `cluster_N` is not removed what results is:

![&nbsp;](../images/2014-02-28-using-graphviz-via-haskell-ex3.png)

which is not what I'm after.

---

# running dot from Haskell via Shelly

To run `dot` on the dot output of `Data.GraphViz` I use some utilities that use the [Shelly](https://github.com/yesodweb/Shelly.hs) shell scripting DSL:

    {-# LANGUAGE ExtendedDefaultRules #-}
    {-# LANGUAGE OverloadedStrings    #-}
    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
    
    module WriteRunDot where
    
    import           Control.Monad          (forM_)
    import           Data.GraphViz
    import           Data.GraphViz.Printing
    import qualified Data.Text              as T
    import           Data.Text.Lazy         as L
    import           Shelly
    default (T.Text)
    
    writeDot :: PrintDot a => (T.Text, a) -> Sh ()
    writeDot ng = writeDotToDir "/tmp" ng
    
    writeDotToDir :: PrintDot a => T.Text -> (T.Text, a) -> Sh ()
    writeDotToDir d (n,g) =
        writefile (fromText (mkFileName d n "dot"))
                  (T.pack (unpack (renderDot $ toDot g)))
    
    runDot :: T.Text -> Sh ()
    runDot n = runDotFromTo "/tmp" "/tmp" n "png"
    
    runDotFromTo :: T.Text -> T.Text -> T.Text -> T.Text -> Sh ()
    runDotFromTo f t n e = do
        let from = mkFileName f n "dot"
        let to   = mkFileName t n e
        run_ "dot" [T.append "-T" e, from, "-o", to]
    
    doDots :: PrintDot a => [(T.Text, a)] -> Sh ()
    doDots cases = forM_ cases (\x -> do writeDot x; (runDot . fst) x)
    
    mkFileName :: T.Text -> T.Text -> T.Text -> T.Text
    mkFileName d n e = T.concat [d,"/",n,".",e]

and use the utilities via:

    main :: IO ()
    main = shelly $ do
        doDots [ ("ex1" , graphToDot ex1Params ex1) ]
        doDots [ ("ex2" , ex2)
               , ("ex3" , ex3)
               ]

Note: I would rather rather pipe the resulting dot files into `runDot`
(rather than writing `*.dot` files via `writeDot`), but I haven't
figured out how to do that in Shelly yet.

---

# summary

Using `Data.GraphViz` I can now write dot diagrams but use Haskell to
factor out the common parts of similar diagrams (not shown in the
examples above).  Of course, I also have the full power of Haskell
available.  And, when using a interactive Haskell environment (see [Tim
Dysinger's emacs environment](http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html)), the IDE catches type errors, syntax
errors, duplicates, etc., while your write.  A great improvement over
manually writing and maintaining `*.dot` files.

## source code

The emacs org-mode literate source code of this article is available at:

-   <https://github.com/haroldcarr/learn-haskell-coq-ml-etc/blob/master/haskell/paper/haroldcarr/graphviz/2014-02-28-using-graphviz-via-haskell/2014-02-28-using-graphviz-via-haskell.org>
