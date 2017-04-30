{-# LANGUAGE OverloadedStrings #-}

module HC.Main where

import           Control.Applicative (Alternative (..), (<$>))
import           Control.Monad       (filterM)
import           Data.List           (intersperse, isSuffixOf)
import           Data.List.Split     (splitOn)
import           Data.Monoid         (mappend)
import           Hakyll
import           HC.Config
import           HC.Tags
import           System.FilePath     (splitExtension)
import           Text.Pandoc.Options (writerHtml5)

hcNumCurrent :: Int
hcNumCurrent = 12

hcMain :: Configuration -> SiteConfiguration -> (String -> FeedConfiguration) -> IO ()
hcMain hakyllConf siteConf feedConf = hakyllWith hakyllConf $ do
  -- let engineConf          = defaultEngineConfiguration
  let writerOptions       = defaultHakyllWriterOptions { writerHtml5 = True }
  let pandocHtml5Compiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions

  -- siteCtx :: Context String
  let siteCtx =
       deIndexedUrlField "url"               `mappend`
       constField "root" (siteRoot siteConf) `mappend`
       constField "gaId" (siteGaId siteConf) `mappend`
       constField "feedTitle" "Posts"        `mappend`
       constField "feedUrl" "/atom.xml"      `mappend`
       constField "gMapsApiScript" ""        `mappend`
       defaultContext

  -- postCtx :: Tags -> Context String
  let postCtx tags =
       dateField "date" "%e %B %Y"           `mappend`
       dateField "datetime" "%Y-%m-%d"       `mappend`
       (tagsFieldWith' getTags) "tags" tags  `mappend`
       siteCtx

  -- postList :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
  let postList tags sortFilter = do
       posts   <- sortFilter =<< loadAll "content/posts/*"
       itemTpl <- loadBody "templates/post-item.html"
       list    <- applyTemplateList itemTpl (postCtx tags) posts
       return list

  tags <- buildTags "content/posts/*" (fromCapture "tags/*/index.html")

  let postTagsCtx = postCtx tags

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "images/presentations/2013-09-15-JavaOne/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/dcplusplus/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/FODynamicCluster/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/FODynamicCluster/FODynamicCluster-slides-html/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/FOStaticCluster/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/cic2004/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/contactinfo/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/doa2003/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/doa2003/peptMinimalRpcArch/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/jbossMiddleware2003AndPEPt/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/middleware2003/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/middleware2003/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/middleware2003/onePagePept/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/soc2004/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/soc2004/soc2004-slides-html/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/pept/uucs2005-02-02/*" $ do
    route idRoute
    compile copyFileCompiler

  match "computerScience/presentations/*" $ do
    route idRoute
    compile copyFileCompiler

{- HC: replace with static copies - see next
  match (fromList $ vendorScriptFiles engineConf) $ do
    route $ customRoute (combine "js/vendor" . takeFileName . toFilePath)
    compile copyFileCompiler

  match (fromList $ lessFiles engineConf) $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter (lessCommand engineConf) $ "-" : (lessOptions engineConf))
-}

  match "css/*" $ do
    route idRoute
    compile copyFileCompiler

  match "content/about/index.md" $ do
    route $ stripContent `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= loadAndApplyTemplate "templates/about.html"   siteCtx
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls
      >>= deIndexUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag

    route idRoute
    compile $ do
      list <- postList tags (\t -> recentFirst t >>= filterM (fmap (elem tag) . getTags . itemIdentifier))
      let ctx =
            constField "tag"       tag                               `mappend`
            constField "posts"     list                              `mappend`
            constField "feedTitle" title                             `mappend`
            constField "title"     title                             `mappend`
            constField "feedUrl"   ("/tags/" ++ tag ++ "/index.xml") `mappend`
            siteCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"   ctx
        >>= relativizeUrls
        >>= deIndexUrls

    version "rss" $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
      route $ setExtension "xml"
      compile $ loadAllSnapshots pattern "content"
        >>= fmap (take hcNumCurrent) . recentFirst
        >>= renderAtom (feedConf title) feedCtx

  match "content/posts/*" $ do
--    route $ directorizeDate `composeRoutes` stripContent `composeRoutes` setExtension "html"
    route $ stripContent `composeRoutes` setExtension "html"
    compile $ do
      compiled <- pandocHtml5Compiler {- use getResourceBody instead for *.html -}
      full     <- loadAndApplyTemplate "templates/post.html"        postTagsCtx            compiled
      teaser   <- loadAndApplyTemplate "templates/post-teaser.html" postTagsCtx $ dropMore compiled
      _        <- saveSnapshot "content" full
      _        <- saveSnapshot "teaser" teaser
      loadAndApplyTemplate "templates/default.html" (postCtx tags) full
        >>= relativizeUrls
        >>= deIndexUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = field "posts" (\_ -> postList tags recentFirst) `mappend`
                       constField "title" "Archives"                   `mappend`
                       siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "content/index.html" $ do
    route stripContent
    compile $ do
      tpl  <- loadBody "templates/post-item-full.html"
      body <- readTemplate . itemBody <$> getResourceBody
      loadAllSnapshots "content/posts/*" "teaser"
        >>= fmap (take hcNumCurrent) . recentFirst
        >>= applyTemplateList tpl (postCtx tags)
        >>= makeItem
        >>= applyTemplate body (siteCtx `mappend` bodyField "posts")
        >>= loadAndApplyTemplate "templates/default.html" siteCtx
        >>= relativizeUrls
        >>= deIndexUrls

  -- https://github.com/AustinRochford/blog/blob/master/site.hs
  create ["tag-cloud.html"] $ do
        route idRoute
        compile $ do
            let cloudCtx = constField "title" "Tags" `mappend` siteCtx -- defaultContext

            renderTagCloud 100 300 tags
                >>= makeItem
                >>= loadAndApplyTemplate "templates/tag-cloud.html" cloudCtx
                >>= loadAndApplyTemplate "templates/default.html"   cloudCtx
                >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
      posts <- mapM deIndexUrls =<< fmap (take hcNumCurrent) . recentFirst =<<
        loadAllSnapshots "content/posts/*" "content"
      renderAtom (feedConf "blog") feedCtx (posts)

  match "templates/*"   $ compile templateCompiler
  match "templates/*/*" $ compile templateCompiler

stripContent :: Routes
stripContent = gsubRoute "content/" $ const ""

directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = dirs ++ "/index" ++ ext
      where
        (dirs, ext)  = splitExtension $ concat $ (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
        (date, rest) = splitAt 3 $ splitOn "-" path

stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) ("/."::String)
                 then take (length url - 10) url
                 else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

deIndexedUrlField :: String -> Context a
deIndexedUrlField key = field key $ fmap (stripIndex . maybe empty toUrl) . getRoute . itemIdentifier

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)

-- End of file.
