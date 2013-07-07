module Main where
import System.Random
import Control.Monad
import Control.Concurrent
import System.Process
import System.IO
import Control.Exception (finally)

data MathProblem = Plus Int Int
                 | Times Int Int
                 | Minus Int Int

instance Show MathProblem where
  show (Plus x y) = show x ++ " + " ++ show y
  show (Times x y) = show x ++ " * " ++ show y
  show (Minus x y) = show x ++ " - " ++ show y

answer :: MathProblem -> Int
answer (Plus x y) = x + y
answer (Times x y) = x * y
answer (Minus x y) = x - y

genProblem :: IO MathProblem
genProblem = do
  idx <- randomRIO (0, 2)
  let op = [Plus, Times, Minus] !! idx
  x <- randomRIO (1, 20)
  y <- randomRIO (1, 20)
  return $ op x y

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

askProblem :: IO Bool
askProblem = do prob <- genProblem
                putStr $ (show prob) ++ ": "
                hFlush stdout
                l <- getLine
                case maybeRead l of
                  Just x | x == answer prob -> return True
                  _ -> return False

alarmText = "alarm.."

playAlarm :: IO ()
playAlarm = forever . system $ "osascript -e 'set Volume 10'; say " ++ alarmText

promptLoop n =
  when (n > 0) $ do correct <- askProblem
                    promptLoop $ if correct then n - 1 else n

withCurrentVolume :: IO a -> IO a
withCurrentVolume action =
  do vol <- readProcess "osascript" ["-e", "output volume of (get volume settings)"] ""
     action `finally` system ("osascript -e 'set Volume " ++ vol ++ "'")

main :: IO ()
main = withCurrentVolume $ do alarmTid <- forkIO playAlarm
                              promptLoop 20
                              killThread alarmTid
