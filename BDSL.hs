{-# LANGUAGE MultiParamTypeClasses #-}
module BSDL where

import Control.Monad.State
import Data.Char
import Debug.Trace
import Interpret

--define tokens and monad

data Token = INC | DEC | LEFT | RIGHT | INPUT | OUTPUT | BEGIN_LOOP | END_LOOP

instance Show Token where
  show = (:[]) . toCh

compile :: [Token] -> String
compile = map toCh

toCh INC = '+'
toCh DEC = '-'
toCh LEFT = '<'
toCh RIGHT = '>'
toCh INPUT = ','
toCh OUTPUT = '.'
toCh BEGIN_LOOP = '['
toCh END_LOOP = ']'

data BFState = BFState {code :: [Token], allocated :: Int, memLocation :: Int} deriving Show
initState = BFState [] 1 0

type Brainfuck a = State BFState a

--define pointer

data Pointer t = Pointer Int

origin = Pointer 0

class Type t where
  size :: t -> Int
  typeof :: (Pointer t) -> t
  typeof = const undefined

class (Type t) => FromHaskell t a where
  from :: a -> Brainfuck (Pointer t)

--our types
data BFChar

instance Type BFChar where
  size = const 1

instance FromHaskell BFChar Char where
  from ch = do
    let ch' = ord ch
    new <- malloc char
    access new
    replicateM_ ch' inc
    return new

char = undefined :: BFChar

type Array a = [Pointer a]

arrayOf :: (Type t) => t -> Int -> Brainfuck (Array t)
arrayOf t n = mapM malloc $ replicate n t

fromArray :: (FromHaskell t a) => [a] -> Brainfuck (Array t)
fromArray = mapM from
--utils

push :: Token -> Brainfuck ()
push x = modify $ \st -> st {code = code st ++ [x]}

inc = push INC
dec = push DEC
right = do
  push RIGHT
  modify $ \st -> st {memLocation = memLocation st + 1}
left = do
  push LEFT
  modify $ \st -> st {memLocation = memLocation st - 1}
input = push INPUT
output = push OUTPUT
beginl = push BEGIN_LOOP
endl = push END_LOOP

malloc :: (Type t) => t -> Brainfuck (Pointer t)
malloc t = do
  let sz = size t
  st <- get
  put $ st { allocated = allocated st + sz}
  return . Pointer $ allocated st

access :: Pointer t -> Brainfuck ()
access (Pointer add) = do
  st <- get
  let delta = add - memLocation st
      delta' = abs delta
      dir = if delta > 0 then right else left
  replicateM_ delta' dir

putch :: Pointer BFChar -> Brainfuck ()
putch ptr = do
  access ptr
  output

putstr :: Array BFChar -> Brainfuck ()
putstr = mapM_ putch

--ur program goes here
prog = do
  greeting <- fromArray "Hello world!\n"
  putstr greeting

main = runFromStr . compile . code . snd $ runState prog initState
