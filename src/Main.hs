{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import HC.Config
import HC.Main

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand = "rsync -ave 'ssh' _site/* haroldcarr_haroldcarrcom@ssh.phx.nearlyfreespeech.com:."
  }

siteConf :: SiteConfiguration
siteConf = SiteConfiguration
  { siteRoot = "http://haroldcarr.com"
  , siteGaId = "UA-15303127-1"
  }

feedConf :: String -> FeedConfiguration
feedConf title = FeedConfiguration
  { feedTitle       = "haroldcarr.com: " ++ title
  , feedDescription = "technology"
  , feedAuthorName  = "Harold Carr"
  , feedAuthorEmail = "harold.carr@gmail.com"
  , feedRoot        = "http://haroldcarr.com"
  }

main :: IO ()
main = hcMain hakyllConf siteConf feedConf

