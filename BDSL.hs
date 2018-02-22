module BSDL where

import Control.Monad.State
import Data.Char
import Debug.Trace
import Data.Foldable
import Data.Sequence hiding (take,drop,replicate,filter)

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

decr ptr = do
  access ptr
  dec

incr ptr = do
  access ptr
  inc

addr ptr n = do
  access ptr
  add n

subr ptr n = do
  access ptr
  sub n

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

copyTo :: Pointer -> Pointer -> Brainfuck ()
copyTo ptr output = do
  tmp <- value 0
  loop ptr $ do
    decr ptr
    incr tmp
    incr output
  loop tmp $ do
    decr tmp
    incr ptr
  free tmp

copy :: Pointer -> Brainfuck Pointer
copy ptr = do
  result <- value 0
  copyTo ptr result
  return result

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

iflt :: Pointer -> Pointer -> Brainfuck () -> Brainfuck ()
iflt a b callback = do
  a' <- copy a
  counter <- copy b
  temp <- value 0
  cond <- value 0
  loop counter $ do
    set cond 1
    copyTo a' temp
    loop temp $ do
      set cond 0
      set temp 0
    loop cond $ do
      callback
      set cond 0
    decr counter
    decr a'
  free a'
  free counter
  free temp
  free cond

ifgt a b c = iflt b a c

ifeq a b callback = do
  a' <- copy a
  b' <- copy b
  loop a' $ do
    decr a'
    decr b'
  cond <- value 1
  loop b' $ do
    set cond 0
    set b' 0
  loop cond $ do
    callback
    set cond 0
  free a'
  free b'
  free cond

ifeqconst a c callback = do
  a' <- copy a
  subr a' c
  test <- value 1
  loop a' $ do
    set test 0
    set a' 0
  loop test $ do
    set test 0
    callback
  free test
  free a'

mult a b = do
  a' <- value 0
  b' <- copy b
  result <- value 0
  loop b' $ do
    copyTo a a'
    loop a' $ do
      incr result
      decr a'
    decr b'
  free a'
  free b'
  return result

switchValues :: Pointer -> (Int -> Brainfuck ()) -> Brainfuck ()
switchValues ptr callback = do
  flag <- value 1
  copy1 <- copy ptr
  copy2 <- copy ptr
  flip mapM_ [0..255] $ \n -> do
    set flag 1
    copyTo copy1 copy2
    loop copy2 $ do
      set flag 0
      zero copy2
    loop flag $ do
      callback n
      zero flag
    decr copy1
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

putstr :: [Pointer] -> Brainfuck () --go until we get a newline
putstr [] = return ()
putstr (ptr:ptrs) = do
  testval <- copy ptr
  subr testval 10
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
  subr testval 10
  loop testval $ do
    readstr ptrs
    zero testval
  free testval

clearbuf :: [Pointer] -> Brainfuck ()
clearbuf = mapM_ zero

die :: String -> Brainfuck () --kills program
die err = do
  str err >>= putstr
  val <- value 1
  access val
  beginl
  endl

atoi :: [Pointer] -> Brainfuck Pointer
atoi ptrs = do
  result <- value 0
  hundreds <- charValue '0'
  tens <- charValue '0'
  ones <- charValue '0'
  nl <- charValue '\n'
  ifeq (ptrs !! 1) nl $ do
    zero ones
    copyTo (ptrs !! 0) ones
  ifeq (ptrs !! 2) nl $ do
    zero tens
    zero ones
    copyTo (ptrs !! 0) tens
    copyTo (ptrs !! 1) ones
  ifeq (ptrs !! 3) nl $ do
    zero hundreds
    zero tens
    zero ones
    copyTo (ptrs !! 0) hundreds
    copyTo (ptrs !! 1) tens
    copyTo (ptrs !! 2) ones
  subr hundreds 48
  loop hundreds $ do
    decr hundreds
    addr result 100
  subr tens 48
  loop tens $ do
    decr tens
    addr result 10
  subr ones 48
  loop ones $ do
    decr ones
    incr result
  free hundreds
  free tens
  free ones
  return result

{-itoa :: Pointer -> Brainfuck [Pointer]
itoa ptr = do
  ten <- value 10
  (q1,ones) <- moddiv ptr ten
  (hundreds,tens) <- moddiv q1 ten
  result <- str "000 "
  cond <- value 2
  ifeqconst hundreds 0 $ do
    set cond 1
    ifeqconst tens 0 $ do
      set cond 0
      copyTo ones (result !! 0)
      set (result !! 1) $ ord '\n'
    ifeqconst cond 1 $ do
      copyTo tens (result !! 0)
      copyTo ones (result !! 1)
      set (result !! 2) $ ord '\n'
  ifeqconst cond 2 $ do
    copyTo hundreds (result !! 0)
    copyTo tens (result !! 1)
    copyTo ones (result !! 2)
    set (result !! 3) $ ord '\n'
  free cond
  free ten
  free q1
  free hundreds
  free tens
  free ones
  return result-}

moddiv :: Pointer -> Pointer -> Brainfuck (Pointer,Pointer)
moddiv l r = do
  l' <- copy l
  r' <- value 0
  cond <- value 1
  divi <- value 0
  cond' <- value 1
  loop cond $ do
    set cond' 1
    iflt l' r $ do
      set cond 0
      set cond' 0
    loop cond' $ do
      copyTo r r'
      loop r' $ do
        decr l'
        decr r'
      incr divi
      set cond' 0
  ifeqconst l 0 $ do
    set divi 0
    set l' 0 --FILTHY HACK
  free r'
  free cond
  free cond'
  return (divi,l')

nextrng :: Pointer -> Brainfuck ()
nextrng p = do
  a <- value 5
  p' <- mult p a
  addr p' 10
  p'' <- mult p' p --what am i doing? even I don't know
  addr p'' 3
  copyTo p'' p
  free a
  free p'
  free p''

puts :: String -> Brainfuck ()
puts msg = do
  buf <- str msg
  putstr buf
  mapM_ free buf

getstr :: String -> Int -> Brainfuck [Pointer]
getstr msg n = do
  output <- str (replicate n ' ')
  puts msg
  readstr output
  return output

geti :: String -> Brainfuck Pointer
geti msg = do
  buf <- getstr msg 4 -- 1-3 digits + nl
  ret <- atoi buf
  mapM free buf
  return ret

printI :: Pointer -> Brainfuck ()
printI x = switchValues x (puts . show)

comp :: Brainfuck () -> String
comp prog = compile $ code $ snd $ runState prog initState

prog = do
  types <- array (replicate 100 0) -- 10x10 array of zeros
  nneighbors <- array (replicate 100 0)
  statuses <- array (replicate 100 0)
  let
    neighbors ind =
      let
        x = ind `mod` 10
        y = ind `div` 10
      in map (\(x,y) -> y*10 + x) $
        filter (\(x,y) -> x >= 0 && y >= 0 && y < 10 && x < 10)
          [(x-1,y-1),(x,y-1),(x+1,y-1),(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y)]
  nl <- charValue '\n'
  rng <- geti "Rng seed?: "
  fifty <- value 50
  ten <- value 10
  forM_ [0..99] $ \i -> do
    iflt rng fifty $ do
      set (types !! i) 1
      forM_ (neighbors i) $ \neighbor ->
        incr (nneighbors !! neighbor)
    nextrng rng
  cond <- value 1
  loop cond $ do
    forM_ [0..99] $ \i -> do
      ifeqconst (statuses !! i) 0 $ puts "X"
      ifeqconst (statuses !! i) 1 $
        forM_ [0..8] $ \n -> ifeqconst (nneighbors !! i) n $
          puts $ show n
      when (i `mod` 10 == 9) $ putch nl
      when (i `mod` 10 /= 9) $ puts " "
    x <- geti "X reveal position: "
    y <- geti "Y reveal position: "
    index <- value 0
    loop x $ do
      decr x
      incr index
    loop y $ do
      decr y
      addr index 10
    switchValues index $ \ind -> when (ind < 100) $ do
      set (statuses !! ind) 1
      ifeqconst (types !! ind) 1 $ do -- u revealed a bomb
        puts "You blew up..."
        putch nl
        set cond 0

main = do
  writeFile "out.b" (comp prog)
