--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Control.Monad
import           Data.Maybe
import           Data.List (intersperse, sortBy)
import           Data.Ord (comparing)
import           Data.Monoid (mappend)
import           Data.Foldable
import           Hakyll
import           Data.List (isPrefixOf)
import           Data.Text (pack, unpack, replace, empty)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    mapM_ fileCopier filesToCopy
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/**" (fromCapture "tags/*.html")
    
    match "posts/**" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWTags tags)
            >>= (externalizeUrls $ feedRoot feedConfiguration)
            >>= saveSnapshot "content"
            >>= (unExternalizeUrls $ feedRoot feedConfiguration)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWTags tags)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
      let title  = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" (postCtxWTags tags) (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag-page.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
      
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            newPosts <- loadRecent "posts/**"
            let recentCtx = listField "posts" (postCtxWTags tags) (return newPosts)
                indexCtx = field "taglist" (\_ -> renderTagList tags) <> recentCtx <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/**" "content"
            sorted <- take 10 <$> recentFirst posts
            renderRss feedConfiguration feedCtx (take 10 sorted)

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/**" "content"
            sorted <- take 10 <$> recentFirst posts
            renderAtom feedConfiguration feedCtx sorted
            
    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

----  from Hakyll.TemplateList
sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
               mapM (\x -> liftM (x,) (f x)) xs

--- based off chronological               
yearSort :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
yearSort = sortByM (\x -> liftM (\y -> (read y :: Int)) $ getMetadataField' (itemIdentifier x) "year")

-- based off recentFirst
recentYearSort :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
recentYearSort = liftM reverse . yearSort

fileCopier :: Pattern -> Rules ()
fileCopier patt = match patt $ do
  route   idRoute
  compile copyFileCompiler

projMaker :: Pattern -> Rules ()
projMaker patt =  match patt $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" projCtx
    >>= relativizeUrls
---------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

postCtxWTags :: Tags -> Context String
postCtxWTags tags = tagsField "tags" tags <> postCtx

projCtx :: Context String
projCtx = let projTitleCtx = field "title" $ \item -> do
                metadata <- getMetadata (itemIdentifier item)
                return $ fromMaybe "No title" $ lookupString "projtitle" metadata
          in metadataField <> projTitleCtx <> bodyField "body" <> urlField "url" <> pathField "path"

indexPostCtxWTags :: Tags -> Context String
indexPostCtxWTags tags = tagsField "tags" tags <> bodyField "mdbody" <> metadataField <>
                        urlField "url" <> pathField "path" <> titleField "title" <>
                        dateField "date" "%B %e, %Y"

--code from divarvel/blog
feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

externalizeUrls :: String -> Item String -> Compiler (Item String)
externalizeUrls root item = return $ fmap (externalizeUrlsWith root) item

externalizeUrlsWith :: String -- ^ Path to the site root
                    -> String -- ^ HTML to externalize
                    -> String -- ^ Resulting HTML
externalizeUrlsWith root = withUrls ext
  where
    ext x = if isExternal x then x else root ++ x


unExternalizeUrls :: String -> Item String -> Compiler (Item String)
unExternalizeUrls root item = return $ fmap (unExternalizeUrlsWith root) item

unExternalizeUrlsWith :: String -- ^ Path to the site root
                      -> String -- ^ HTML to unExternalize
                      -> String -- ^ Resulting HTML
unExternalizeUrlsWith root = withUrls unExt
  where
    unExt x = if root `isPrefixOf` x then unpack $ replace (pack root) empty (pack x) else x

-- from roseedu/techblog/site.hs
loadRecent :: Pattern -> Compiler [Item String]
loadRecent patt = liftM (take 3) $ loadAllSnapshots patt "content" >>= recentFirst
-----------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "AML Space Blog - RSS feed"
    , feedDescription = "Blog for AML Space"
    , feedAuthorName  = "AML Space"
    , feedAuthorEmail = "artmusiclitspace@gmail.com"
    , feedRoot        = "https://art-music-lit.space/blog"
    }


filesToCopy :: [Pattern]
filesToCopy = ["images/**"
              , "js/**"
              , "css/bootstrap/*"
              ]
