{-# OPTIONS -fglasgow-exts #-}
module Turbinado.Server.Exception (
          TurbinadoException(..)
        , catchTurbinado
        , throwTurbinado
        , throwTurbinadoTo
        , module Control.OldException 
        ) where

import Data.Typeable
import Control.OldException
import Control.Concurrent(ThreadId)

catchTurbinado :: IO a -> (TurbinadoException -> IO a) -> IO a
catchTurbinado = catchDyn

throwTurbinado :: TurbinadoException -> a
throwTurbinado = throwDyn

throwTurbinadoTo :: ThreadId -> TurbinadoException -> IO ()
throwTurbinadoTo = throwDynTo

data TurbinadoException =
--      ParseFailed String
--    | DisillusionedProgrammer
--    | NotImplemented
--    | TurbinadoFileTypeNotSupported
--    | HTTPUnauthorized
--    | HTTPForbidden
--    | HTTPMethodNotAllowed
--    | HTTPChunkedTransferError
      CompilationFailed [String]
    | FileNotFound FilePath
    | LoadApplicationFailed FilePath
    | AppCompilationFailed [String]
    | NoURISpecified
    | TimedOut
    | BadRequest String
    | PageEvalFailed Exception
    | UnexpectedException Exception
    | IllformatedCodeStoreFile
        deriving (Typeable, Show)
