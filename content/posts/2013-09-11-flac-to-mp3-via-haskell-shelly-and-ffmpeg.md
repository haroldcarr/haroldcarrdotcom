---
title: FLAC to MP3 via Haskell, Shelly and ffmpeg
tags: haskell, shelly, flac, mp3, ffmpeg
---

I have a large digitized music collection, primarily encoded in
lossless [FLAC](http://en.wikipedia.org/wiki/FLAC).  Great for listening at home.  But lossy [MP3](http://en.wikipedia.org/wiki/MP3) is best
for mobile devices, in terms of size and the ability of players to
handle encoding formats.

So I wrote a script to make parallel MP3 tree of my canonical music
collection (which also includes FLAC, [WAV](http://en.wikipedia.org/wiki/WAV), [AIFF](http://en.wikipedia.org/wiki/Audio_Interchange_File_Format), MP3, &#x2026;).

I wrote it in [Haskell](http://www.haskell.org/), with the help of the [Shelly](https://github.com/yesodweb/Shelly.hs) shell scripting DSL and `ffmpeg`.

<!-- MORE -->

I am not an experienced Haskell/Shelly programmer.  If you have
suggestions for improvements please let me know (including useful
`ffmpeg` settings).

---

# Shelly

-   <http://hackage.haskell.org/packages/archive/shelly/1.3.0.7/doc/html/Shelly.html>

-   <https://github.com/yesodweb/Shelly.hs>

-   <https://github.com/yesodweb/Shelly.hs/blob/master/doc/shell-typed.markdown>

-   <https://github.com/yesodweb/Shelly.hs/blob/master/src/Shelly.hs>

-   <http://stackoverflow.com/questions/18187944/haskell-shelly-sample-code>

Other haskell shell examples: <http://www.haskell.org/haskellwiki/Applications_and_libraries/Operating_system#Haskell_shell_examples>

    sudo cabal install shelly shelly-extra

---

# FLAC to MP3

-   <https://wiki.archlinux.org/index.php/Convert_Flac_to_Mp3>

-   <http://www.discogs.com/groups/topic/151711>

-   <http://www.ffmpeg.org/ffmpeg.html>

&nbsp;

    sudo port search ffmpeg
    sudo port install ffmpeg @1.2.2
    ffmpeg -i "/Volumes/my-music/Ahmad Jamal/The Awakening/01-The Awakening.flac" -qscale:a 0 "/tmp/JUNK.mp3"

---

# the code

<https://github.com/haroldcarr/make-mp3-copies>

    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE ExtendedDefaultRules #-}
    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
    
    import           Control.Applicative
    import           Control.Exception (bracket, handle, SomeException)
    import           Control.Monad
    import           Control.Monad.IO.Class
    import           Data.Maybe (fromJust)
    import qualified Data.Text as T
    import           Shelly
    import           System.Directory
    import           System.FilePath
    import           System.IO (IOMode(..), hClose, hFileSize, openFile)
    default (T.Text)
    
    fromRoot = "."
    toRoot   = "/tmp/JUNK/"
    
    main = shelly $ verbosely $ do
        processDir fromRoot
    
    processDir fromPath = do
        contents <- ls fromPath
        forM_ contents $ \from -> do
                isDir <- test_d from
                if isDir
                    then do { maybeCreateDir from; processDir from}
                    else processFile from $ takeExtension $ fpToString from
    
    maybeCreateDir from = do
        let to = mkToFilePath from
        dirExists <- test_d to
        if dirExists
            then                  say "DIR EXISTS"  to
            else do { mkdir_p to; say "DIR CREATED" to }
    
    processFile from ".flac" = maybeDo convert True  False from
    processFile from ".mp3"  = maybeDo copy    False True  from
    processFile from ".jpg"  = maybeDo copy    False True  from
    processFile from _       = say "IGNORED" from
    
    maybeDo f extP sizeP from = do
        let to = mkToFilePath $ if extP then (fromText (T.pack (replaceExtension (fpToString from) ".mp3"))) else from
        fileExists <- test_f to
        if fileExists
            then doIf f sizeP from to
            else      f       from to
    
    doIf f sizeP from to = do
        fromSize <- lio getFileSize         from
        fromTime <- lio getModificationTime from
        toSize   <- lio getFileSize         to
        toTime   <- lio getModificationTime to
        if fromTime > toTime || (sizeP && (fromJust fromSize) /= (fromJust toSize))
            then f from to
            else say "FILE EXISTS" to
    
    convert from to = do
        flacToMp3 (toTextIgnore from) (toTextIgnore to)
        say "FILE CONVERTED" to
      where
        flacToMp3 from to = run_ "ffmpeg" ["-i", from, "-qscale:a", "0", to]
    
    copy from to = do
        cp from to
        say "FILE COPIED" to
    
    mkToFilePath path =
        (fpToString toRoot) Shelly.</> (fpToString path)
    
    fpToString fp = T.unpack $ toTextIgnore fp
    
    say msg fp =
        liftIO $ putStrLn $ show (fpToString fp) ++ " " ++ msg
    
    lio f fp =
        liftIO . f $ fpToString fp
    
    -- from Real World Haskell
    getFileSize path = handle handler $
        bracket (openFile path ReadMode) (hClose) (\h -> do
            size <- hFileSize h
            return $ Just size)
      where
        handler :: SomeException -> IO (Maybe Integer)
        handler _ = return Nothing
    
    -- End of file.

---

# usage

In the present version, `cd` to the root of the canonical music collection and run the script.
The output location is hardwired in the code.

    export MP3=~/make-mp3-copies
    alias m3='$MP3/MakeMP3Copies'
    cd $MP3
    ghc MakeMP3Copies.hs
    export PATH=.:$PATH
    pushd "/Volumes/my-music/Ahmad Jamal/"
    m3
