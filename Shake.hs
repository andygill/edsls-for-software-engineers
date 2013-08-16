-- Shake script for paper

import Development.Shake
import Development.Shake.FilePath

main = shakeArgs shakeOptions $ do
    want ["paper.html"]
    "paper.html" *> \out -> do
        need ["paper.md"]
        system' "pandoc" ["paper.md","-o","paper.html"]

