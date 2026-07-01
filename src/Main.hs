module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef
import System.IO
import Control.Monad (unless, forM_)

import Syntax (Stmt (..), parseStmt)
import Compile (compile)
import Term (Term, StopReason (..), normalize, normalizeTrace)
import Pretty (prettyTerm, prettyTermDeBruijn)

-- | REPL state: named bindings, each stored as an already-closed,
-- already-*normalized* de Bruijn term, plus display/eval settings.
data ReplState = ReplState
  { env       :: Map String Term
  , stepMode  :: Bool   -- ^ when True, show every reduction step
  , showIdx   :: Bool   -- ^ when True, also show raw de Bruijn form
  , stepLimit :: Int    -- ^ max beta-reduction steps before giving up
  , sizeLimit :: Int    -- ^ max AST node count a term may grow to mid-reduction
  }

initialState :: ReplState
initialState = ReplState Map.empty False False 10000 100000

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  putStrLn "Untyped lambda calculus REPL"
  putStrLn "Type an expression, `let name = expr`, or :help"
  ref <- newIORef initialState
  loop ref

loop :: IORef ReplState -> IO ()
loop ref = do
  putStr "\955> "
  eof <- isEOF
  unless eof $ do
    line <- getLine
    quit <- handleLine ref line
    unless quit (loop ref)

-- | Handle one line of input. Returns True if the REPL should exit.
handleLine :: IORef ReplState -> String -> IO Bool
handleLine ref raw
  | trimmed == ""               = pure False
  | trimmed `elem` [":q", ":quit"] = putStrLn "bye." >> pure True
  | trimmed `elem` [":h", ":help"] = putStrLn helpText >> pure False
  | trimmed == ":env"           = showEnv ref >> pure False
  | trimmed == ":step on"       = modifyIORef' ref (\s -> s { stepMode = True }) >> putStrLn "step mode on" >> pure False
  | trimmed == ":step off"      = modifyIORef' ref (\s -> s { stepMode = False }) >> putStrLn "step mode off" >> pure False
  | trimmed == ":debruijn on"   = modifyIORef' ref (\s -> s { showIdx = True }) >> putStrLn "de Bruijn display on" >> pure False
  | trimmed == ":debruijn off"  = modifyIORef' ref (\s -> s { showIdx = False }) >> putStrLn "de Bruijn display off" >> pure False
  | otherwise = do
      case parseStmt trimmed of
        Left err -> putStrLn ("parse error:\n" ++ err)
        Right stmt -> runStmt ref stmt
      pure False
  where
    trimmed = dropWhileEnd' (== ' ') (dropWhile (== ' ') raw)
    dropWhileEnd' p = foldr (\c acc -> if p c && null acc then [] else c : acc) []

runStmt :: IORef ReplState -> Stmt -> IO ()
runStmt ref (SLet name expr) = do
  st <- readIORef ref
  case compile (env st) expr of
    Left err -> putStrLn ("compile error: " ++ show err)
    Right term -> do
      let (normal, reason) = normalize (stepLimit st) (sizeLimit st) term
      case reason of
        Normalized -> do
          writeIORef ref st { env = Map.insert name normal (env st) }
          putStrLn (name ++ " = " ++ prettyTerm normal)
        _ ->
          do
            writeIORef ref st { env = Map.insert name term (env st) }
            putStrLn
              ( name ++ " " ++ stopReasonNote (stepLimit st) (sizeLimit st) reason
                  ++ "; saved the original (unreduced) expression instead of a "
                  ++ "partial result. (" ++ render st term ++ ")"
              )
runStmt ref (SExpr expr) = do
  st <- readIORef ref
  case compile (env st) expr of
    Left err -> putStrLn ("compile error: " ++ show err)
    Right term ->
      if stepMode st
        then do
          let (steps, reason) = normalizeTrace (stepLimit st) (sizeLimit st) term
          forM_ (zip [0 :: Int ..] steps) $ \(i, t) ->
            putStrLn ("  " ++ show i ++ ": " ++ render st t)
          case reason of
            Normalized -> pure ()
            _ -> putStrLn ("  (" ++ stopReasonNote (stepLimit st) (sizeLimit st) reason ++ ")")
        else do
          let (normal, reason) = normalize (stepLimit st) (sizeLimit st) term
          putStrLn (render st normal ++ limitNote (stepLimit st) (sizeLimit st) reason)

render :: ReplState -> Term -> String
render st t
  | showIdx st = prettyTerm t ++ "    [" ++ prettyTermDeBruijn t ++ "]"
  | otherwise  = prettyTerm t

stopReasonNote :: Int -> Int -> StopReason -> String
stopReasonNote steps _ StepLimitHit = "did not normalize within " ++ show steps ++ " steps"
stopReasonNote _ size SizeLimitHit  = "stopped: a reduction step exceeded the " ++ show size ++ "-node size cap"
stopReasonNote _ _ Normalized       = "normalized"

limitNote :: Int -> Int -> StopReason -> String
limitNote _ _ Normalized = ""
limitNote steps size reason = "  (" ++ stopReasonNote steps size reason ++ ")"

showEnv :: IORef ReplState -> IO ()
showEnv ref = do
  st <- readIORef ref
  if Map.null (env st)
    then putStrLn "(empty)"
    else forM_ (Map.toList (env st)) $ \(k, v) ->
      putStrLn (k ++ " = " ++ prettyTerm v)

helpText :: String
helpText =
  unlines
    [ "  <expr>            evaluate an expression, e.g. (\\x -> x) (\\y -> y)"
    , "  let name = expr   bind a name in the environment"
    , "  :env               list current bindings"
    , "  :step on|off       show every beta-reduction step"
    , "  :debruijn on|off   also show raw de Bruijn index form"
    , "  :help              this message"
    , "  :quit              exit"
    , ""
    , "  Lambdas: \\x -> body   or   \\x y z -> body   or   \206\187x. body"
    ]
