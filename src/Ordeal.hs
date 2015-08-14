{-# LANGUAGE OverloadedStrings #-}

module Ordeal
    ( launchStackGhci
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad (forever, void)
import qualified Data.Text.IO as TextIO
import           System.FSNotify (WatchConfig(..), Debounce(..), withManagerConf, defaultConfig, watchTree)
import           System.Posix.Types (CPid(..))
import           System.Posix.Signals (Signal, signalProcess, sigINT)
import           System.Process ( CreateProcess(..)
                                , StdStream(..)
                                , CmdSpec(..)
                                , ProcessHandle
                                , createProcess)
import           System.Process.Internals (ProcessHandle__(..), withProcessHandle)
import           System.IO ( BufferMode(..)
                           , hSetBuffering
                           , stdout
                           , stdin
                           )

getPid :: ProcessHandle -> IO (Maybe CPid)
getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing

signalProcessHandle :: ProcessHandle -> Signal -> IO ()
signalProcessHandle pHandle signal = do
    -- TODO: FIX ME
    Just pid <- getPid pHandle
    let pId = CPid (fromIntegral pid)
    signalProcess signal pId

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
    (Just stdinH, Just stdoutH, _err, processHandle) <- createProcess processSpec

    hSetBuffering stdin     NoBuffering
    hSetBuffering stdout    NoBuffering
    hSetBuffering stdinH    LineBuffering
    hSetBuffering stdoutH   NoBuffering

    let inPipe  = forever (TextIO.hGetChunk stdin   >>= TextIO.hPutStr stdinH)
    let outPipe = forever (TextIO.hGetChunk stdoutH >>= TextIO.hPutStr stdout)

    let reloadAction _ = do
            void (Async.async (signalProcessHandle processHandle sigINT))
            TextIO.hPutStrLn stdinH ":reload"

    withManagerConf (defaultConfig {confDebounce = Debounce 0.1}) $ \mgr -> do
      void $ watchTree
        mgr
        "."
        (const True)
        reloadAction

    --  Async.async outPipe >>= Async.wait
      void (Async.concurrently inPipe outPipe)
