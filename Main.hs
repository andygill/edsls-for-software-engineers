-- Program to build the bibfile.

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Network.Curl
import Text.BibTeX.Entry
import qualified Text.BibTeX.Parse as P
import qualified Text.BibTeX.Format as F
import Text.Parsec

main = withCurlDo $ do
     -- First, load the bibtex file

     ts_fpg <- readBibTeX "https://raw.githubusercontent.com/ku-fpg/bibtex/master/fpg.bib"
     ts_bib <- readBibTeX "https://raw.githubusercontent.com/ku-fpg/bibtex/master/bibtex.bib"
     cs <- readCitations "paper.aux"
     let ts = [ t
              | t <- ts_fpg ++ ts_bib 
              , identifier t `Set.member` cs
              ]

     writeFile "paper.bib" $ unlines $ map F.entry ts
     return ()

readBibTeX :: String -> IO [T]
readBibTeX url = do
     (CurlOK,str) <- curlGetString url []

     case parse (P.skippingLeadingSpace P.file) url str of
       Left msg -> error (show msg)
       Right ts -> do
               putStrLn $ "read " ++ show (length ts) ++ " entries from " ++ url
               return ts

-- Find the citations in the aux file
readCitations :: String -> IO (Set String)
readCitations fileName = do
      txt <- readFile fileName
      return $ Set.fromList
             $ concat
             $ [ splitOn "," $ takeWhile (/= '}') $ drop (length citation) ln
      	       | ln <- lines txt
               , citation `isPrefixOf` ln
               ]
  where citation = "\\citation{" 
  