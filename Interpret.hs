module Interpret (runFromStr) where
import Text.ParserCombinators.Parsec
import Control.Monad
import System.IO
import Data.Char

data Instruction = MovePointer Int | Modify Int | Loop [Instruction] | Input | Output

inp = char ',' *> pure Input

out = char '.' *> pure Output

moves = MovePointer . tally <$> many1 (char '<' <|> char '>')
  where
    tally = sum . map (\x -> if x == '<' then -1 else 1)

modify = Modify . tally <$> many1 (char '+' <|> char '-')
  where
    tally = sum . map (\x -> if x == '+' then 1 else -1)

loop = Loop <$> (char '[' *> many instruction <* char ']')

instruction :: GenParser Char st Instruction
instruction = inp <|> out <|> moves <|> modify <|> loop

instructions = many instruction

data Tape a = Tape [a] a [a]

type BFState = Tape Int

initState = Tape (repeat 0) 0 (repeat 0)

modTape :: (a -> a) -> Tape a -> Tape a
modTape f (Tape ls x rs) = Tape ls (f x) rs

middle :: Tape a -> a
middle (Tape _ x _) = x

left :: Tape a -> Tape a
left (Tape (l:ls) x rs) = Tape ls l (x:rs)

right :: Tape a -> Tape a
right (Tape ls x (r:rs)) = Tape (x:ls) r rs

raiseToPow :: (a -> a) -> Int -> (a -> a)
raiseToPow f = foldl1 (.) . flip replicate f

wrapAdd :: Int -> Int -> Int
wrapAdd x y = let
    result = (x + y) `mod` 256
    result' = if result >= 0 then result else 256 + result
  in result'

step :: Instruction -> BFState -> IO BFState
step (MovePointer n) st = return $ if n > 0 then raiseToPow right n st else raiseToPow left (negate n) st
step (Modify n) st = return $ modTape (wrapAdd n) st
step (Loop instructions) st = if middle st == 0 then
  return st
    else
  foldM (flip step) st instructions >>= step (Loop instructions)
step Input st = do
  a <- fmap ord getChar
  return $ modTape (const a) st
step Output st = do
  putChar . chr $ middle st
  hFlush stdout
  return st

run :: [Instruction] -> IO ()
run instructions = foldM (flip step) initState instructions >> return ()

runFromStr :: String -> IO ()
runFromStr bf = case parse instructions "stdin" bf of
    (Left err) -> print err
    (Right x) -> run x
