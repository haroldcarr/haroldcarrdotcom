---
title: how to connect threepenny-gui to external events
tags: haskell, threepenny-gui
---

Over the years I occasionally reimplemented my [rdf-triple-browser](https://github.com/haroldcarr/rdf-triple-browser),
first in GWT, then Java/Swing and then last summer in Haskell using
[Threepenny-gui](https://wiki.haskell.org/Threepenny-gui) (which I will call 'TPG' for the rest of this article).
The hardest part for me was understanding how to connect to external events.

The documentation says `newEvent :: IO (Event a, Handler a)` "Create a
new event. Also returns a function that triggers an event occurrence."
What is doesn't say it that calling the returned `Handler` causes the
specific returned `Event` to happen.

I got lost in the documentation about `Handler`, going off on dead
ends with `register`.  Even the TAs at last summer's [Utrecht Haskell
Summer School](http://www.utrechtsummerschool.nl/courses/science/applied-functional-programming-in-haskell) could not figure it out (although, to be fair, they did
not spend that much time on it, they were concentrating on course
questions).

Fortunately, Max Taldykin, on [stackoverflow](http://stackoverflow.com/questions/24784883/using-threepenny-gui-reactive-in-client-server-programming), provided a small program
that lead me to discover the "magic" of how to use what is returned via
`newEvent`. Below I show an even smaller program that shows how it is
wired.

<!-- MORE -->

     1  module ThreepennyExternalNewEventDemo where
     2  
     3  import           Control.Concurrent     (forkIO)
     4  import           Graphics.UI.Threepenny
     5  import           Network
     6  import           System.IO              (hClose)
     7  
     8  main :: IO ()
     9  main = do
    10      (eAccept, hAccept) <- newEvent
    11      forkIO (acceptLoop hAccept 6789)
    12      forkIO (acceptLoop hAccept 9876)
    13      startGUI defaultConfig $ \win -> do
    14          bAccept <- stepper "" eAccept
    15          entree <- entry bAccept
    16          element entree # set (attr "size") "10" # set style [("width","200px")]
    17          getBody win #+ [element entree]
    18          return ()
    19  
    20  acceptLoop :: (String -> IO a) -> PortNumber -> IO b
    21  acceptLoop hAccept bindAddr = do
    22      s <- listenOn $ PortNumber bindAddr
    23      loop s
    24    where
    25      loop s = do
    26          (h, hostname, portNumber) <- accept s
    27          hClose h
    28          hAccept $ show bindAddr ++ " " ++ hostname ++ " " ++ show portNumber
    29          loop s

Line 10 uses `newEvent` to create an `Event a` and `Handler a`.  The
"magic" is that behind the scenes TPG has wired the return `Handler`
such that when it is called it generates an `Event` specific to the returned `Event`.

Line 14 creates a `Behavior` from that specific `Event`.  That
`Behavior` is then given to the `entry` widget.  Whenever the code
calls the `Handler` it triggers that `Event` that causes the
`Behavior` to change.  The widget shows the `Behavior` change.

Here is the wiring for the program:

![&nbsp;](../images/2015-02-06-how-to-connect-threepenny-gui-to-external-events.png)

Line 11 starts two threads that accept connection at two different
listening ports.  Whenever a connection is accepted, the loops call the
given `Handler` at line 28.  The `Handler` "magically" triggers an
`Event`.  The `Event` is turned into a `Behavior` via `stepper`.  The
`Behavior` was previously wired to the `entry` widget.  So, whenever a
connection is accepted the text field shows the info.  That's it!

Hopefully this small example will save someone some time.
