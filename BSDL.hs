module BSDL where

import Control.Monad.State
import Data.Char
import Debug.Trace
import Data.Foldable
import Data.Sequence

--define tokens and monad

data Token = INC | DEC | LEFT | RIGHT | INPUT | OUTPUT | BEGIN_LOOP | END_LOOP

instance Show Token where
  show = (:[]) . toCh

compile :: Seq Token -> String
compile = fmap toCh . toList

toCh INC = '+'
toCh DEC = '-'
toCh LEFT = '<'
toCh RIGHT = '>'
toCh INPUT = ','
toCh OUTPUT = '.'
toCh BEGIN_LOOP = '['
toCh END_LOOP = ']'

data BFState = BFState {code :: Seq Token, allocated :: [Bool], memLocation :: Int} deriving Show
initState = BFState empty (True:repeat False) 0

type Brainfuck a = State BFState a
push :: Token -> Brainfuck ()
push ins = do
  st <- get
  put $ st {code = code st |> ins}

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

sub = flip replicateM_ dec
add = flip replicateM_ inc

newtype Pointer = Pointer Int

origin = 0

access :: Pointer -> Brainfuck ()
access (Pointer add) = do
  st <- get
  let delta = add - memLocation st
      delta' = abs delta
      dir = if delta > 0 then right else left
  replicateM_ delta' dir

alloc :: Int -> Brainfuck [Pointer]
alloc 0 = return []
alloc n = do
  st <- get
  let
    go (x:xs) n = if x then
        let (xs',n') = go xs (n+1)
        in (x:xs',n')
      else (True:xs,n)
    (allocated',add) = go (allocated st) 0
  put $ st {allocated = allocated'}
  zero (Pointer add)
  fmap (Pointer add:) $ alloc (n-1)

free :: Pointer -> Brainfuck ()
free (Pointer address) = do
  st <- get
  put $ st {allocated = Prelude.take address (allocated st) ++ [False] ++ Prelude.drop (address+1) (allocated st)}

zero :: Pointer -> Brainfuck ()
zero ptr = do
  access ptr
  beginl
  dec
  endl

set :: Pointer -> Int -> Brainfuck ()
set ptr n = do
  zero ptr
  access ptr
  replicateM_ n inc

value :: Int -> Brainfuck Pointer
value n = do
  [ptr] <- alloc 1
  set ptr n
  return ptr

loop :: Pointer -> Brainfuck () -> Brainfuck ()
loop cond action = do
  access cond
  beginl
  action
  access cond
  endl

copyTo :: Pointer -> Pointer -> Brainfuck ()
copyTo ptr output = do
  tmp <- value 0
  loop ptr $ do
    access ptr
    dec
    access tmp
    inc
    access output
    inc
  loop tmp $ do
    access tmp
    dec
    access ptr
    inc
  free tmp

copy :: Pointer -> Brainfuck Pointer
copy ptr = do
  result <- value 0
  copyTo ptr result
  return result

switchValues :: Pointer -> (Int -> Brainfuck ()) -> Brainfuck ()
switchValues ptr callback = do
  flag <- value 1
  copy1 <- copy ptr
  copy2 <- copy ptr
  flip mapM_ [0..100] $ \n -> do
    set flag 1
    copyTo copy1 copy2
    loop copy2 $ do
      set flag 0
      zero copy2
    loop flag $ do
      callback n
      zero flag
    access copy1
    dec
  free flag
  free copy1
  free copy2

charValue :: Char -> Brainfuck Pointer
charValue = value . ord

array :: [Int] -> Brainfuck [Pointer]
array = mapM value

str :: String -> Brainfuck [Pointer]
str = mapM charValue

putch :: Pointer -> Brainfuck ()
putch ptr = access ptr >> output

putstr :: [Pointer] -> Brainfuck () --go until we get a null terminator
putstr [] = return ()
putstr (ptr:ptrs) = do
  testval <- copy ptr
  access testval
  sub 10
  loop testval $ do
    access ptr
    output
    putstr ptrs
    zero testval
  free testval

readstr :: [Pointer] -> Brainfuck ()
readstr [] = return ()
readstr (ptr:ptrs) = do
  access ptr
  input
  testval <- copy ptr
  access testval
  sub 10
  loop testval $ do
    readstr ptrs
    zero testval
  free testval

atoi :: [Pointer] -> Brainfuck Pointer
atoi ptrs = do
  let hundreds = ptrs !! 0
      tens = ptrs !! 1
      ones = ptrs !! 2
  result <- value 0
  access hundreds
  sub 48
  loop hundreds $ do
    access hundreds
    dec
    access result
    add 100
  access tens
  sub 48
  loop tens $ do
    access tens
    dec
    access result
    add 10
  access ones
  sub 48
  loop ones $ do
    access ones
    dec
    access result
    inc
  return result

comp :: Brainfuck () -> String
comp prog = compile $ code $ snd $ runState prog initState

prog = do
  buf <- str (Prelude.replicate 255 ' ')
  query1 <- str "Enter number to index string: "
  query2 <- str "Enter string to index: "
  newline <- charValue '\n'
  putstr query1
  readstr buf
  val <- atoi buf
  putstr query2
  readstr buf
  switchValues val $ \n -> do
    putch (buf !! n)
  putch newline


main = do
  writeFile "out.b" (comp prog)
