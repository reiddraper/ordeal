{-# LANGUAGE OverloadedStrings #-}

module Ordeal
    ( launchStackGhci
    ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import           Control.Concurrent.Chan (newChan, readChan)
import           Control.Monad (forever, unless, void)
import           Data.Monoid ((<>), mempty)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import           System.FSNotify (WatchConfig(..), Debounce(..), withManagerConf, defaultConfig, watchTreeChan)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
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

processSpec :: String -> [String] -> CreateProcess
processSpec cmd args = CreateProcess {
    cmdspec         = RawCommand cmd args
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
    let defaultreplCommand = ("stack", ["ghci"])
    args <- getArgs
    ((cmd, cmdArgs), sCommand) <- maybe exitFailure return $
            case args of
                [userCommand] ->
                    Just (defaultreplCommand, userCommand)
                [userReplCommand, userCommand] ->
                    Just ((head (words userReplCommand), tail (words userReplCommand)), userCommand)
                _ -> Nothing
    (Just stdinH, Just stdoutH, _err, processHandle) <- createProcess (processSpec cmd cmdArgs)

    let command = Text.pack sCommand

    hSetBuffering stdin     NoBuffering
    hSetBuffering stdout    NoBuffering
    hSetBuffering stdinH    NoBuffering
    hSetBuffering stdoutH   NoBuffering

    readVar <- MVar.newEmptyMVar
    let inPipe  = forever (TextIO.hGetChunk stdin   >>= TextIO.hPutStr stdinH)
    let outPipe = forever (do
                            t <- TextIO.hGetChunk stdoutH
                            MVar.putMVar readVar t
                            TextIO.hPutStr stdout t)

    let prompt = "ghci> "

    let reloadAction _ = do
            void (Async.async (signalProcessHandle processHandle sigINT))
            TextIO.hPutStrLn stdinH ":reload"
            TextIO.hPutStrLn stdinH command

    withManagerConf (defaultConfig {confDebounce = Debounce 0.5}) $ \mgr -> do
      chan <- newChan
      void $ watchTreeChan
        mgr
        "."
        (const True)
        chan

      void (Async.async (forever (readChan chan >>= reloadAction)))

      a <- Async.async (Async.concurrently inPipe outPipe)
      let looper acc = do
            t <- MVar.takeMVar readVar
            let newAcc = acc <> t
            unless (Text.isSuffixOf prompt newAcc) (looper newAcc)
      looper mempty
      void (Async.async (forever (MVar.takeMVar readVar)))
      TextIO.hPutStrLn stdinH command
      TextIO.hPutStrLn stdout command
      void (Async.wait a)
