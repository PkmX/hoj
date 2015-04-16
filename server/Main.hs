{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, ScopedTypeVariables, TupleSections #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bool
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Foreign.C.Error (Errno(..), ePIPE)
import GHC.IO.Exception (IOException(..), IOErrorType(..), )
import qualified Data.Text as T
import Network.Wai.Handler.Warp (HostPreference, Port, defaultSettings, setPort, setHost)
import Network.Wai.Middleware.Static
import Options.Applicative as P
import System.Exit
import System.IO
import System.Posix.Files
import System.Posix.Temp
import System.Process
import Web.Scotty as Scotty

submit :: [(ByteString, ByteString)] -> Text -> Int -> ActionM ()
submit tests mainfn timelimit = do
    code <- param "code"
    userInput <- maybeParam "input"
    result <- liftIO $ withMkstemp "/tmp/hojprog-" $ \(path, h) -> do
      T.hPutStrLn stderr $ "Compiling as " <> T.pack path
      (clangExit, clangOut, clangErr) <- readProcessWithExitCodeText "clang++" [ "-std=c++14", "-O2", "-march=native", "-Wall", "-Wextra", "-pedantic", "-Wshadow", "-Wcast-qual", "-fcolor-diagnostics", "-fstack-protector-strong", "--param=ssp-buffer-size=4", "-D_FORTIFY_SOURCE=2", "-L.", "-lEasySandbox", "-xc++", "-", "-o", path ] $ if isNothing userInput then code <> mainfn else code
      hClose h
      case clangExit of
        ExitSuccess -> do
          T.hPutStrLn stderr $ "Running " <> T.pack path
          case userInput of
            Just input -> do
              (progExit, timeout, progOut, _) <- readProcessWithExitCodeEnvTimeLimitText path [] [("LD_LIBRARY_PATH", ".")] timelimit input
              let output = if | timeout -> ("Time Limit Exceeded" :: Text)
                              | progExit /= ExitSuccess -> "Runtime Error"
                              | otherwise -> either (const $ decodeLatin1 progOut) id (decodeUtf8' progOut)
              return $ object [ "compilerOutput" .= (clangOut <> clangErr)
                              , "output" .= output
                              ]
            Nothing -> do
              results <- flip mapConcurrently tests $ \(input, output) -> do
                (progExit, timeout, progOut, _) <- readProcessWithExitCodeEnvTimeLimitText path [] [("LD_LIBRARY_PATH", ".")] timelimit input
                return $ if | timeout -> ("Time Limit Exceeded" :: Text)
                            | progExit /= ExitSuccess -> "Runtime Error"
                            | progOut /= output -> if BS.words progOut /= BS.words output then "Wrong Answer" else "Presentation Error"
                            | otherwise -> "Correct"
                 
              return $ object [ "compilerOutput" .= (clangOut <> clangErr)
                              , "results" .= results
                              ]
        ExitFailure _ -> return $ object [ "compilerError" .= (clangOut <> clangErr) ]
    liftIO $ BL.hPutStrLn stderr $ encode result
    Scotty.json result
  where maybeParam p = (Just <$> param p) `rescue` const (pure Nothing)

data Args = Args { _bindAddress :: HostPreference
                 , _port :: Port
                 , _debug :: Bool
                 , _timelimit :: Int
                 , _clientdir :: FilePath
                 , _problempdf :: FilePath
                 , _mainFile :: FilePath
                 , _testFiles :: [(String, String)]
                 }

argsParser :: Parser Args
argsParser = Args <$> (fromString <$> strOption (long "bind" <> metavar "ip-address" <> help "The address to listen on" <> value "127.0.0.1"))
                  <*> option auto (long "port" <> metavar "port" <> help "The port to listen on" <> value 2345)
                  <*> switch (long "debug" <> help "Enable debugging")
                  <*> option auto (long "timelimit" <> metavar "µs" <> help "Time limit in microseconds" <> value (3 * 1000 * 1000))
                  <*> strOption (long "clientdir" <> metavar "directory" <> help "Client directory to serve" <> value "client/build/")
                  <*> strOption (long "problempdf" <> metavar "problem.pdf" <> help "The problem PDF to serve" <> value "")
                  <*> strOption (long "main" <> metavar "main.c" <> help "The main function to append" <> value "")
                  <*> (toPairList <$> many (strArgument (metavar "[test-input test-output]...")))
  where toPairList (x:y:xs) = (x, y) : toPairList xs
        toPairList [_] = error "Un-paired test cases"
        toPairList [] = [] -- error "Empty test cases"

main :: IO ()
main = do
  Args addr port debug timelimit clientdir problempdf mainFile testFiles <- execParser $ info (helper <*> argsParser) (fullDesc <> P.header "Haskell Online Judge System")
  when debug $ putStrLn $ "Test cases: " <> show testFiles
  tests <- forM testFiles $ \(input, output) -> (,) <$> BS.readFile input <*> BS.readFile output
  mainfn <- if null mainFile then pure "" else T.readFile mainFile

  let options = Options { verbose = bool 0 1 debug , settings = setHost addr $ setPort port defaultSettings } 
  scottyOpts options $ do
    unless debug $ defaultHandler $ const $ text "500 Internal Server Error"
    middleware $ staticPolicy $ addBase clientdir
    post "/submit" $ submit tests mainfn timelimit
    get  "/problem.pdf" $ file problempdf >> setHeader "Content-Type" "application/pdf"

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m mf = maybe (return ()) mf m

withCreateProcess :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) -> IO a
withCreateProcess cp = bracket (createProcess cp) cleanup
  where cleanup (hin, hout, herr, _) = whenJust hin (ignoreSigPipe . hClose) >> forM_ [hout, herr] (`whenJust` hClose)

withMkstemp :: String -> ((FilePath, Handle) -> IO a) -> IO a
withMkstemp s = bracket (mkstemp s) (\(path, h) -> hClose h >> silentRemoveLink path)
  where silentRemoveLink p = removeLink p `catch` ignoreIOException
        ignoreIOException (_ :: IOException) = return ()

readProcessWithExitCodeText :: MonadIO m => FilePath -> [String] -> Text -> m (ExitCode, Text, Text)
readProcessWithExitCodeText cmd args input = liftIO $ readProcessWithExitCode cmd args (T.unpack input) <&> \(ret, out, err) -> (ret, T.pack out, T.pack err)

readProcessWithExitCodeEnvTimeLimitText :: MonadIO m => FilePath -> [String] -> [(String, String)] -> Int -> ByteString -> m (ExitCode, Bool, ByteString, ByteString)
readProcessWithExitCodeEnvTimeLimitText cmd args env timelimit input = liftIO $
    withCreateProcess cp $ \(Just hin, Just hout, Just herr, ph) -> do
      timeoutVar <- newTVarIO False
      void $ forkIO $ do
        threadDelay timelimit
        terminateProcess ph
        atomically $ writeTVar timeoutVar True
      ignoreSigPipe $ do BS.hPut hin input
                         hClose hin
      ec <- waitForProcess ph
      out <- dropSecComp <$> BS.hGetContents hout
      err <- dropSecComp <$> BS.hGetContents herr
      timeout <- atomically $ readTVar timeoutVar
      return (ec, timeout, out, err)
  where dropSecComp = BS.drop (length ("<<entering SECCOMP mode>>\n" :: String))
        cp = CreateProcess { cmdspec = RawCommand cmd args
                           , cwd = Nothing
                           , env = Just env
                           , std_in = CreatePipe
                           , std_out = CreatePipe
                           , std_err = CreatePipe
                           , close_fds = True
                           , create_group = False
                           , delegate_ctlc = False
                           }

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handle $ \e -> case e of IOError { ioe_type = ResourceVanished , ioe_errno = Just ioe } | Errno ioe == ePIPE -> return ()
                                         _ -> throw e
