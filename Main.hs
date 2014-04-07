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

<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes

     let alph a b = identifier a `compare` identifier b

     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph ts
=======

     let alph a b = identifier a `compare` identifier b

     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======

     let alph a b = identifier a `compare` identifier b

     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" $ unlines $ map F.entry $ sortBy alph $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" 
     	       $ unlines 
	       $ map F.entry 
	       $ nubBy ((== EQ) . alph) 
	       $ sortBy alph 
	       $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" 
     	       $ unlines 
	       $ map F.entry 
	       $ nubBy ((== EQ) . alph)
	       $ sortBy alph 
	       $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" 
     	       $ unlines 
	       $ map F.entry 
	       $ nubBy ((== EQ) . alph)
	       $ sortBy alph 
	       $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" 
     	       $ unlines 
	       $ map F.entry		-- show the specific entry
	       $ nubBy ((== EQ) . alph)	-- remove dups
	       $ sortBy alph 	  	-- output is sorted (helps stability)
	       $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b
     writeFile "paper.bib" 
     	       $ unlines 
	       $ map F.entry		-- show the specific entry
	       $ nubBy ((== EQ) . alph)	-- remove dups
	       $ sortBy alph 	  	-- output is sorted (helps stability)
	       $ ts
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b

     let bibs = map F.entry		-- show the specific entry
	      $ nubBy ((== EQ) . alph)	-- remove dups
	      $ sortBy alph 	  	-- output is sorted (helps stability)
	      $ ts

     let outfile = "paper.bib"      
     putStrLn $ "writing " ++ show (length bibs) ++ " entries into " ++ show outfile
     writeFile outfile $ unlines $ bibs
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b

     let bibs = map F.entry		-- show the specific entry
	      $ nubBy ((== EQ) . alph)	-- remove dups
	      $ sortBy alph 	  	-- output is sorted (helps stability)
	      $ ts

     let outfile = "paper.bib"      
     putStrLn $ "writing " ++ show (length bibs) ++ " entries into " ++ show outfile
     writeFile outfile $ unlines $ bibs
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b

     let bibs = map F.entry		-- show the specific entry
	      $ nubBy (\ a b ->EQ == alph a b)	-- remove dups
	      $ sortBy alph 	  	-- output is sorted (helps stability)
	      $ ts

     let outfile = "paper.bib"      
     putStrLn $ "writing " ++ show (length bibs) ++ " entries into " ++ show outfile
     writeFile outfile $ unlines $ bibs
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b

     let bibs = map F.entry		-- show the specific entry
	      $ nubBy (\ a b -> EQ == alph a b)	-- remove dups
	      $ sortBy alph 	  	-- output is sorted (helps stability)
	      $ ts

     let outfile = "paper.bib"      
     putStrLn $ "writing " ++ show (length bibs) ++ " entries into " ++ show outfile
     writeFile outfile $ unlines $ bibs
>>>>>>> External Changes
=======
     let alph a b = identifier a `compare` identifier b

     let bibs = map F.entry		-- show the specific entry
	      $ nubBy (\ a b -> EQ == alph a b)	-- remove dups
	      $ sortBy alph 	  	-- output is sorted (helps stability)
	      $ ts

     let outfile = "paper.bib"      
     putStrLn $ "writing " ++ show (length bibs) ++ " entries into " ++ show outfile
     writeFile outfile $ unlines $ bibs
>>>>>>> External Changes
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
      let cites = Set.fromList
             $ concat
             $ [ splitOn "," $ takeWhile (/= '}') $ drop (length citation) ln
      	       | ln <- lines txt
               , citation `isPrefixOf` ln
               ]
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
<<<<<<< Local Changes
      putStrLn $ "found " ++ show (Set.length cites) ++ " citations"
            
=======
      putStrLn $ "found " ++ show (Set.length cites) ++ " citations"
      return $ cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.length cites) ++ " citations"
      return $ cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.length cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.length cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
=======
      putStrLn $ "found " ++ show (Set.size cites) ++ " citations"
      return cites      
>>>>>>> External Changes
  where citation = "\\citation{" 
  