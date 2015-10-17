module PostTest where

-- external
import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as DM
import Network.HTTP (HStream, Request(..), Response(..))
import qualified Network.Stream
import Network.URI (uriPath)
import Text.Parsec (ParseError(..))
import Text.Pandoc (Pandoc(..), def, readMarkdown)
import Text.Pandoc.Error (handleError)
import Text.Printf (printf)

-- internal
import qualified Scarlet.Post.Internal as SP
import qualified Scarlet.Entry as SE

-- testing harness
import Test.HUnit
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H

tests :: IO [C.Test]
tests = (++)
         <$> return (uncurry H.test <$> pureHTests)
         <*> sequence effectfulTests

effectfulTests :: [IO C.Test]
effectfulTests = [test4ForAbsentRelativeUris,
                  test5FromMarkdownFile]

test4ForAbsentRelativeUris :: IO C.Test
test4ForAbsentRelativeUris = let
    scanForAbsentRelativeUris :: Pandoc -> IO [String]
    scanForAbsentRelativeUris = SP.scanForAbsentRelativeUrisWithHTTP t4DummySimpleHTTP
  in do
      absentRelativeUris <- scanForAbsentRelativeUris t4md
      return $ H.test "finds absent relative URIs"
                $ TestList [
                    TestCase $ assert (t4AbsentRelativeUri `elem` absentRelativeUris),
                    TestCase $ assert (t4PresentRelativeUri `notElem` absentRelativeUris)
                  ]

test5File :: FilePath
test5File =  "test/t5.md"

test5FromMarkdownFile :: IO C.Test
test5FromMarkdownFile = do
    eT5e <- SP.parseEntry =<< readFile test5File
    case eT5e of
      Right t5e -> do
        absentUris <- SP.scanForAbsentRelativeUrisWithHTTP dummySimpleHTTP t5e
        return $ H.test "properly parses an example Markdown"
                  $ TestList [
                      TestCase (SE.entryDirectives t5e  @=? expectedEntryDirectives),
                      TestCase (SE.entryUri t5e         @?= expectedEntryUri),
                      TestCase (absentUris              @?= expectedAbsentUris)
                    ]
      Left e    -> return $ H.test "Parse failed" (TestCase (assertFailure (show e)))
  where
    expectedAbsentUris = ["mjölkfria-bullar.jpg"]
    expectedEntryDirectives = "fromList [(\"static_host\",\"cerulean.questionable.rocks\")]"
    expectedEntryUri = "cinnamon-buns-for-cinnamon-bun-day"
    expectedAbsoluteAbsentUris = attachPrefix <$> expectedAbsentUris
      where
        attachPrefix :: String -> String
        attachPrefix = intercalate "/" . (++) ["http://cerulean.questionable.rocks",
                                               expectedEntryUri] . (:[])
--    dummySimpleHTTP requestUri = dummyHTTPWrap $ notElem requestUri expectedAbsoluteAbsentUris
    dummySimpleHTTP :: String -> IO (Network.Stream.Result (Response String))
    dummySimpleHTTP requestUri = dummyHTTPWrap (notElem requestUri expectedAbsoluteAbsentUris)


pureHTests :: [(String, Test)]
pureHTests = [
    ("finds an image resource in a paragraph",
      TestCase (SP.scanForAssets t1md @?= [t1ImageUri])),
    ("finds a link resource in a paragraph",
      TestCase (SP.scanForAssets t2md @?= [t2LinkUri])),
    ("collects relative uris",
      TestCase (SP.scanForRelativeURIs t3md @?= [t3RelativeLinkUri]))
  ]

--    Test Case #1:  A Markdown document with a single image.
t1ImageUri :: String
t1ImageUri = "http://example.com/example.jpg"

t1md :: Pandoc   -- Pandoc of a Markdown doc with one image
t1md =  handleError $ readMarkdown def
                      $ printf "This is a paragraph.\n\n![this is an image](%s)" t1ImageUri

--    Test Case #2:  A Markdown document with a single link.
t2LinkUri :: String
t2LinkUri =  "http://example.com/something-interesting"

t2md :: Pandoc
t2md = handleError $ readMarkdown def
                      $ printf "This paragraph has [a link](%s), but then continues." t2LinkUri

--    Test Case #3:  Correctly identifies a relative URI
t3RelativeLinkUri :: String
t3RelativeLinkUri =  "/something-interesting"

t3AbsoluteLinkUri :: String
t3AbsoluteLinkUri =  "http://example.com/something-interesting"

t3md :: Pandoc
t3md =  handleError $ readMarkdown def
          $ printf "This paragraph has [a relative link](%s) and also [an absolute one](%s)."
              t3RelativeLinkUri t3AbsoluteLinkUri

--    Test Case #4:  Identifies whether a relative URI already exists
t4PresentRelativeUri :: String
t4PresentRelativeUri =  "/this-is-here-already"

t4AbsentRelativeUri :: String
t4AbsentRelativeUri =   "/this-doesnt-yet-exist"

t4md :: Pandoc
t4md =  handleError $ readMarkdown def
          $ printf "This paragraph links to ![present content](%s) and ![absent content](%s)"
              t4PresentRelativeUri t4AbsentRelativeUri

t4DummySimpleHTTP :: String -> IO (Network.Stream.Result (Response String))
t4DummySimpleHTTP = dummyHTTPWrap . (`isSuffixOf` t4PresentRelativeUri)

dummyHTTPWrap :: Bool -> IO (Network.Stream.Result (Response String))
dummyHTTPWrap True  = return $ Right $ Response (2,0,0) "OK" [] ""
dummyHTTPWrap False = return $ Right $ Response (4,0,4) "Resource not found" [] ""
