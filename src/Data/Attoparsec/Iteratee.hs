{-# LANGUAGE DeriveDataTypeable #-}
module Data.Attoparsec.Iteratee
  ( ParseError(..), parserToIteratee ) where


------------------------------------------------------------------------------
import           Control.Exception
import qualified Data.Attoparsec as Atto
import           Data.Attoparsec hiding (many, Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Iteratee
import           Data.Typeable
------------------------------------------------------------------------------

data ParseError
    = ParseError {errorContexts :: [String], errorMessage :: String}
    deriving (Show, Typeable)

instance Exception ParseError

parserToIteratee :: (Monad m) =>
                    Parser a
                 -> Iteratee ByteString m a
parserToIteratee p = icont (f (parse p)) Nothing
                     where f k (EOF Nothing) = case feed (k B.empty) B.empty of
                               Atto.Fail _ err dsc -> throwErr (toException $ ParseError err dsc)
                               Atto.Partial _ -> throwErr (toException EofException)
                               Atto.Done rest v
                                   | B.null rest -> idone v (EOF Nothing)
                                   | otherwise -> idone v (Chunk rest)
                           f _ (EOF (Just e)) = throwErr e
                           f k (Chunk s)
                               | B.null s = icont (f k) Nothing
                               | otherwise = case k s of
                                   Atto.Fail _ err dsc -> throwErr (toException $ ParseError err dsc)
                                   Atto.Partial k' -> icont (f k') Nothing
                                   Atto.Done rest v -> idone v (Chunk rest)
