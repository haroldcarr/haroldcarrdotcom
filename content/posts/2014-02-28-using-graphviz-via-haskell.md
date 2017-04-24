---
title: using graphviz via haskell
tags: haskell, graphviz
---

One of the tools I use for drawing graphs is `dot` from [Graphviz](http://www.graphviz.org/).
Recently I was drawing a series of diagrams that have mostly identical
parts.  It was error-prone and a pain to keep the identical parts in
sync between the multiple `*.dot` files.  I found [Haskell](http://www.haskell.org/)'s
[Data.GraphViz](http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz.html) package to be the solution.

The documentation for that package is great, but it needs a few more
examples.  The purpose of this post is to add examples to the pool.

<!-- MORE -->

## setup

This post uses the following extension and imports:

    {-# LANGUAGE OverloadedStrings #-}
    
    module Main where
    
    import           Data.Graph.Inductive
    import           Data.GraphViz
    import           Data.GraphViz.Attributes.Complete
    import           Data.GraphViz.Types.Generalised   as G
    import           Data.GraphViz.Types.Monadic
    import           Data.Text.Lazy                    as L
    import           Data.Word
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
      where fn (_,l)   = [textLabel l]
            fe (_,_,l) = [textLabel l]
    
            ga = [ GraphAttrs [ RankDir   FromLeft
                              , BgColor   [toWColor White]
                              ]
                 , NodeAttrs  [ shape     BoxShape
                              , FillColor (myColorCL 2)
                              , style     filled
                              ]
                 ]

along with some color helper functions:

    -- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
    myColorCL :: Word8 -> ColorList
    myColorCL n | n == 1 = c $ (RGB 127 108 138)
                | n == 2 = c $ (RGB 175 177 112)
                | n == 3 = c $ (RGB 226 206 179)
                | n == 4 = c $ (RGB 172 126 100)
     where c rgb = toColorList [rgb]
    
    myColor :: Word8 -> Attribute
    myColor n = Color $ myColorCL n

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
            node "Ready"               [ textLabel "ready"
                                       , shape DoubleCircle, myColor 1, FixedSize True, Width 1]
        cluster (Int 1) $ do
            graphAttrs [textLabel "active"]
            node "Open"                [ textLabel "open"
                                       , shape       Circle, myColor 2, FixedSize True, Width 1]
            node "OpenExpectFragment"  [ textLabel "open expect\nfragment"
                                       , shape       Circle, myColor 2, FixedSize True, Width 1]
            node "HalfClosed"          [ textLabel "half-clsd"
                                       , shape       Circle, myColor 2, FixedSize True, Width 1]
            node "endMessage?"         [ textLabel "end req?"
                                       , shape DiamondShape, myColor 4, FixedSize True, Width 1.25, Height 1.25]
            node "fragmentEndMessage?" [ textLabel "end req?"
                                       , shape DiamondShape, myColor 4, FixedSize True, Width 1.25, Height 1.25]
            node "requestFragment"     [ textLabel "FRAGMENT"
                                       , shape     BoxShape, myColor 3]
    
            "Open"                     --> "endMessage?"
            edge "endMessage?"             "HalfClosed"          [textLabel "true"]
            edge "endMessage?"             "OpenExpectFragment"  [textLabel "false"]
            "OpenExpectFragment"       --> "requestFragment"
            "requestFragment"          --> "fragmentEndMessage?"
            edge "fragmentEndMessage?"     "OpenExpectFragment"  [textLabel "false"]
            edge "fragmentEndMessage?"     "HalfClosed"          [textLabel "true"]
    
        cluster (Int 2) $ do
            graphAttrs [textLabel "done"]
            node "Closed"              [ textLabel "closed"
                                       , shape DoubleCircle, myColor 1, FixedSize True, Width 1]
    
        -- outside the box(es)
        node "request"                 [ textLabel "REQUEST"
                                       , shape     BoxShape, myColor 3]
        node "response"                [ textLabel "RESPONSE"
                                       , shape     BoxShape, myColor 3]
    
        "Ready"      --> "request"
        "request"    --> "Open"
    
        "HalfClosed" --> "response"
        "response"   --> "Closed"

The above results in (a diagram for the beginnings of a simple wire
protocol with possibly fragmented request messages and single response
messages):

![&nbsp;](../images/2014-02-28-using-graphviz-via-haskell-ex2.png)

---

# minor limitation and workaround

Quite often I create diagrams that do not use clustering but have
different node types, each type with a distinct shape, size and color.
In dot, one can factor the shared attributes via `subgraph`:

    digraph ex3 {
        graph [rankdir=LR];
        subgraph {
            node [shape=doublecircle,fixedsize=true,width=1,style=filled,color="#7f6c8a"];
            Open [label=open];
            Closed [label=closed];
        }
        subgraph {
            node [shape=circle,fixedsize=true,width=1,style=filled,color="#7f6c8a"];
            ClosedWaitingAck [label="clsd waiting\nACK"];
        }
        subgraph {
            node [shape=box,width=1,style=filled,color="#e2ceb3"];
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

`Data.GraphViz` supports `subgraph` for `Data.Graph.Inductive` graphs
via the `isDotCluster` setting in `GraphvizParams`.

It also supports `subgraph` for `Data.GraphViz.Types` `Canonical` and
`Generalised` graphs via setting `isCluster` to `False` for their
appropriate `DotSubGraph` types.

However, `Graph` and `Monadic` do not (yet) have a setting that
supports `subgraph`.

The dot output above was produced from:

    ex3 :: G.DotGraph L.Text
    ex3 = digraph (Str "ex3") $ do
    
        graphAttrs [RankDir FromLeft]
    
        cluster (Int 0) $ do
            nodeAttrs               [shape DoubleCircle, FixedSize True, Width 1, style filled, myColor 1]
            node "Open"             [textLabel "open"]
            node "Closed"           [textLabel "closed"]
    
        cluster (Int 1) $ do
            nodeAttrs               [shape       Circle, FixedSize True, Width 1, style filled, myColor 1]
            node "ClosedWaitingAck" [textLabel "clsd waiting\nACK"]
    
        cluster (Int 2) $ do
            nodeAttrs               [shape     BoxShape,                 Width 1, style filled, myColor 3]
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

## workaround

Since all of Haskell is available to build the graph, one can do:

    doubleCircle :: n -> Text -> Dot n
    doubleCircle n l = node n [textLabel l, shape DoubleCircle, FixedSize True, Width 1, style filled, myColor 1]
    
    circle       :: n -> Text -> Dot n
    circle       n l = node n [textLabel l, shape       Circle, FixedSize True, Width 1, style filled, myColor 1]
    
    rectangle    :: n -> Text -> Dot n
    rectangle    n l = node n [textLabel l, shape     BoxShape,                 Width 1, style filled, myColor 3]
    
    open, closed, waiting, cancel, cancelAck :: Dot L.Text
    open      = doubleCircle "Open"             "open"
    closed    = doubleCircle "Closed"           "closed"
    waiting   = circle       "ClosedWaitingAck" "clsd waiting\nACK"
    cancel    = rectangle    "cancel"           "CANCEL"
    cancelAck = rectangle    "cancelAck"        "CANCEL_ACK"
    
    ex4 :: G.DotGraph L.Text
    ex4 = digraph (Str "ex4") $ do
    
        graphAttrs [RankDir FromLeft]
        open; closed; waiting; cancel; cancelAck
    
        "Open"             --> "cancel"
        "cancel"           --> "ClosedWaitingAck"
        "ClosedWaitingAck" --> "cancelAck"
        "cancelAck"        --> "Closed"

This results in the exact same output as the manually editted graph:

![&nbsp;](../images/2014-02-28-using-graphviz-via-haskell-ex4.png)

I also use the above "trick" to factor out common parts of graphs and
share them (my original motivation for using `Data.GraphViz` instead
of manually writing `dot`).

---

# creating images

Images for these examples can be created using the utilities (the
important piece being `runGraphvizCommand` and `addExtension`):

    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE OverloadedStrings     #-}
    
    module WriteRunDot where
    
    import           Control.Monad   (forM_)
    import           Data.GraphViz
    import           System.FilePath
    
    doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
    doDots cases = forM_ cases createImage
    
    createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
    createImage (n, g) = createImageInDir "/tmp" n Png g
    
    createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
    createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

and use the utilities via:

    main :: IO ()
    main = do
        doDots [ ("ex1" , graphToDot ex1Params ex1) ]
        doDots [ ("ex2" , ex2)
               , ("ex3" , ex3)
               , ("ex4" , ex4)
               ]

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
