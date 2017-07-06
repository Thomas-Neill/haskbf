module BSDL where
--brainfuck domain specific Language
import Control.Monad.State
import Data.Char

data BFToken = Inc | Dec | PtLeft | PtRight | StartLoop | EndLoop | Input | Output

toCh Inc = '+'
toCh Dec = '-'
toCh PtLeft = '<'
toCh PtRight = '>'
toCh StartLoop = '['
toCh EndLoop = ']'
toCh Input = ','
toCh Output = '.'

type Brainfuck a = State ([BFToken],Int) a
type Address = Int

composeL :: [Brainfuck ()] -> Brainfuck ()
composeL = foldl1 (>>)

push :: BFToken -> Brainfuck ()
push ins = modify (\(x,y) -> (x ++ [ins],y))

rep :: BFToken -> Int -> Brainfuck ()
rep ins n = modify (\(x,y) -> (x ++ replicate n ins,y))

goto = rep PtRight
back = rep PtLeft

wipe = do
  push StartLoop
  push Dec
  push EndLoop

data PointerType = Char
data Pointer = Pointer Address PointerType | Array { raw :: [Pointer] }

(@@) :: Pointer -> Int -> Pointer
(Array xs) @@ a = xs !! a

class Settable s where
  set :: Pointer -> s -> Brainfuck ()

instance Settable Char where
  set (Pointer add Char) c = do
    goto add
    wipe
    rep Inc (ord c)
    back add

size Char = 1

malloc :: PointerType -> Brainfuck Pointer
malloc tp = do
  (x,currentmem) <- get
  put (x,currentmem + size tp)
  return $ Pointer currentmem tp

listOf :: PointerType -> Int -> Brainfuck Pointer
listOf tp n = do
  new <- mapM malloc $ replicate n tp
  return $ Array new

fromList :: (Settable s) => [s] -> Brainfuck Pointer
fromList arr = do
  (Array pts) <- listOf Char (length arr)
  composeL $ zipWith set pts arr
  return $ Array pts

puts (Pointer add Char) = do
  goto add
  push Output
  back add
puts (Array pts) = do
  composeL $ map puts pts

program = do
  c <- fromList "Hello, world!"
  puts c

optimize' (PtLeft:PtRight:xs) = optimize' xs
optimize' (PtRight:PtLeft:xs) = optimize' xs
optimize' (x:xs) = x:optimize' xs
optimize' [] = []

optimize = foldl1 (.) $ replicate 100 optimize'

main = putStrLn . (map toCh) . optimize . fst . snd $ runState program ([],0)
