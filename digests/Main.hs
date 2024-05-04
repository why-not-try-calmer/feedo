module Main where

import Server (startApp)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  -- Docker / Kubernetes needs this to log output as it comes
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering