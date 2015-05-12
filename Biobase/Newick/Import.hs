
module Biobase.Newick.Import where

import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text (Parser,(<?>))
import           Control.Applicative
import           Data.Char (isAlpha)
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Biobase.Newick.Types



type PNT = Parser NewickTree

newicksFromFile :: FilePath -> IO (Either String [NewickTree])
newicksFromFile f = newicksFromText <$> T.readFile f

newicksFromText :: Text -> Either String [NewickTree]
newicksFromText = A.parseOnly manyNewick

manyNewick = many (newick <* A.skipSpace) <* A.endOfInput

newick :: PNT
newick = tree <* ";" <?> "newick"

tree :: PNT
tree = A.skipSpace *> (branched <|> leaf) <?> "tree"

branched :: PNT
branched = NNode <$ "(" <*> tree `A.sepBy1` "," <* ")" <*> name <*> plength <?> "branched"

leaf :: PNT
leaf = NLeaf <$> name <*> plength <?> "leaf"

name :: Parser Text
name = A.takeWhile accept <?> "name"
  where accept a = isAlpha a || A.inClass "_." a || A.isHorizontalSpace a

plength :: Parser NewickLength
plength = (Len <$ ":" <*> A.double) <|> pure NoLen <?> "plength"



test = mapM_ (\x -> print (A.parseOnly manyNewick x) >> putStrLn "")
        [ "B:0.2;"
        , "A;"
        , ";"
        , "(,);"
        , "((raccoon:19.19959,bear:6.80041):0.84600,((sea_lion:11.99700, seal:12.00300):7.52973,((monkey:100.85930,cat:47.14069):20.59201, weasel:18.87953):2.09460):3.87382,dog:25.46154);"
        , "(Bovine:0.69395,(Gibbon:0.36079,(Orang:0.33636,(Gorilla:0.17147,(Chimp:0.19268, Human:0.11927):0.08386):0.06124):0.15057):0.54939,Mouse:1.21460):0.10;"
        , "(Bovine:0.69395,(Hylobates:0.36079,(Pongo:0.33636,(G._Gorilla:0.17147, (P._paniscus:0.19268,H._sapiens:0.11927):0.08386):0.06124):0.15057):0.54939, Rodent:1.21460);"
        , "A;"
        , "((A,B),(C,D));"
        , "(Alpha,Beta,Gamma,Delta,,Epsilon,,,);"
        ]

