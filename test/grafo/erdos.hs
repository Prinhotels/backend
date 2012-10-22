import System.IO (openFile, IOMode(ReadMode), hGetLine)
import Text.Regex (splitRegex, mkRegex)
import Data.List.Split (splitOn)
import qualified Data.Map as DM

main = do
	h <- openFile "caso1" ReadMode
	s <- fmap read $ hGetLine h
	sequence_ $ replicate s $ do
		[dlines, slines] <- fmap ((fmap read) . words) $ hGetLine h
		sequence_ $ replicate dlines $ do
			names <- fmap (splitRegex (mkRegex  ",\\s*") . head . splitOn ":") $ hGetLine h
			putStrLn $ "dline " ++ (show names)
		sequence_ $ replicate slines $ do
			l <- hGetLine h
			putStrLn $ "sline " ++ l
	putStrLn "hola"