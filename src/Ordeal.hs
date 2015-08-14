{-# LANGUAGE OverloadedStrings #-}

module Ordeal
    ( launchStackGhci
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad (forever, void)
import qualified Data.Text.IO as TextIO
import           System.FSNotify (withManager, watchDir)
import           System.Process ( CreateProcess(..)
                                , StdStream(..)
                                , CmdSpec(..)
                                , createProcess)
import           System.IO ( Handle
                           , BufferMode(NoBuffering)
                           , hSetBuffering
                           , stdout
                           , stdin
                           )

processSpec :: CreateProcess
processSpec = CreateProcess {
    cmdspec         = RawCommand "stack" ["ghci"]
  , cwd             = Nothing
  , env             = Nothing
  , std_in          = CreatePipe
  , std_out         = CreatePipe
  , std_err         = Inherit
  , close_fds       = False
  , create_group    = False
  , delegate_ctlc   = False }

launchStackGhci :: IO ()
launchStackGhci = do
    (Just stdinH, Just stdoutH, _err, _processHandle) <- createProcess processSpec

    hSetBuffering stdin     NoBuffering
    hSetBuffering stdout    NoBuffering
    hSetBuffering stdinH    NoBuffering
    hSetBuffering stdoutH   NoBuffering

    let inPipe  = forever (TextIO.hGetChunk stdin   >>= TextIO.hPutStr stdinH)
    let outPipe = forever (TextIO.hGetChunk stdoutH >>= TextIO.hPutStr stdout)

    withManager $ \mgr -> do
      -- start a watching job (in the background)
      watchDir
        mgr          -- manager
        "src"          -- directory to watch
        (const True) -- predicate
        (const (  TextIO.hPutStrLn stdinH ":reload"))        -- action

    --  Async.async outPipe >>= Async.wait
      void (Async.concurrently inPipe outPipe)
