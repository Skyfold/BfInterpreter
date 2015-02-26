module Main where

import Data.Word
import Data.Char
import System.IO

data Tape a = Tape
            { left::[a]
            , current::a
            , right::[a]
            }

type ZipProg = Tape BFProg

data BFProg = Add
            | Minus
            | LeftBrace
            | RightBrace
            | Print
            | Input
            | GoRight
            | GoLeft
            | EndOfProg

emptyTape :: Tape Word8
emptyTape = Tape [] 0 (repeat 0)

toZip :: [a] -> Tape a
toZip (x:xs) = (Tape [] x xs)

parse :: String -> [BFProg]
parse input = case input of
    '+':xs -> Add : parse xs
    '-':xs -> Minus : parse xs
    '[':xs -> LeftBrace : parse xs
    ']':xs -> RightBrace : parse xs
    '.':xs -> Print : parse xs
    ',':xs -> Input : parse xs
    '>':xs -> GoRight : parse xs
    '<':xs -> GoLeft : parse xs
    [] -> [EndOfProg]
    _:xs -> parse xs

run :: ZipProg -> Tape Word8-> IO (Tape Word8)
run prog@(Tape _ p _) tape@(Tape ls c rs) =
    case p of
         Add -> run (nextInput prog) (Tape ls (c+1) rs)
         Minus -> run (nextInput prog) (Tape ls (c-1) rs)
         Print -> (putChar (chr (fromIntegral c))) >> run (nextInput prog) tape
         Input -> do
             char <- getChar
             run (nextInput prog) (Tape ls (fromIntegral (ord char)) rs)

         GoRight-> run (nextInput prog) (nextInput tape)
         GoLeft -> run (nextInput prog) (prevInput tape)
         LeftBrace
            | c == 0 -> run (moveToRightBrace prog) tape
            | otherwise -> run (nextInput prog) tape
         RightBrace
            | c == 0 -> run (nextInput prog) tape
            | otherwise -> run (moveToLeftBrace prog) tape
         EndOfProg -> return tape

nextInput :: Tape a -> Tape a
nextInput (Tape ls c rs) = (Tape (c:ls) (head rs) (tail rs))

prevInput :: Tape a -> Tape a
prevInput (Tape ls c rs) = (Tape (tail ls) (head ls) (c:rs))

moveToLeftBrace :: ZipProg -> ZipProg
moveToLeftBrace prog = countToBrace (prevInput prog) 0
  where
      countToBrace :: Tape BFProg -> Int -> ZipProg
      countToBrace p@(Tape ls c (r:rs)) num =
        case c of
             LeftBrace ->
               case num of
                    0 -> (Tape (c:ls) r (rs))
                    -1 -> error "missmatched braces"
                    _ -> countToBrace (prevInput p) (num-1)
             RightBrace -> countToBrace (prevInput p) (num+1)
             EndOfProg -> error "Missmatched braces"
             _ -> countToBrace (prevInput p) num


moveToRightBrace :: ZipProg -> ZipProg
moveToRightBrace prog = countToBrace (nextInput prog) 0
  where
      countToBrace :: Tape BFProg -> Int -> ZipProg
      countToBrace p@(Tape (ls) c (r:rs)) num =
        case c of
             RightBrace ->
               case num of
                    0 -> (Tape (c:ls) r (rs))
                    -1 -> error "missmatched braces"
                    _ -> countToBrace (nextInput p) (num-1)
             LeftBrace -> countToBrace (nextInput p) (num+1)
             EndOfProg -> error "Missmatched braces"
             _ -> countToBrace (nextInput p) num

main :: IO ()
main = do
    string <- getLine
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    run (toZip (parse string)) emptyTape
    return ()

