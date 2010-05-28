module Data.Attoparsec.Iteratee
  ( parserToIteratee ) where


------------------------------------------------------------------------------
import qualified Data.Attoparsec as Atto
import           Data.Attoparsec hiding (many, Result(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Iteratee
import           Data.Iteratee.WrappedByteString
import           Data.Word
------------------------------------------------------------------------------

type Stream         = StreamG WrappedByteString Word8
type IterV      m   = IterGV WrappedByteString Word8 m


parserToIteratee :: (Monad m) =>
                    Parser a
                 -> IterateeG WrappedByteString Word8 m a
parserToIteratee p = IterateeG $ f (\s -> parse p s)
  where
    f :: (Monad m) =>
         (ByteString -> Atto.Result a)
      -> Stream
      -> m (IterV m a)
    f k (EOF Nothing)  = finalChunk $ feed (k S.empty) S.empty
    f _ (EOF (Just e)) = reportError e
    f k (Chunk s)      = let s' = S.concat $ L.toChunks $ fromWrap s
                         in if S.null s'
                             then return $ Cont (IterateeG $ f k) Nothing
                             else chunk s' k


    finalChunk :: (Monad m) => Atto.Result a -> m (IterV m a)
    finalChunk (Atto.Fail _ _ m) =
        return $ Cont (error $ show m)
                      (Just $ Err m)

    finalChunk (Atto.Done rest r) =
        return $ Done r (Chunk $ toWrap $ L.fromChunks [rest])

    finalChunk (Atto.Partial _) =
        return $ Cont (error "parser did not produce a value")
                      (Just $ Err "parser did not produce a value")

    reportError e = return $ Cont (error $ show e) (Just e)

    chunk :: (Monad m) =>
             ByteString
          -> (ByteString -> Atto.Result a)
          -> m (IterV m a)
    chunk s k = do
        let r = k s
        case r of
          (Atto.Fail _ _ m) -> return $
                             Cont (throwErr (Err m)) (Just $ Err m)
          (Atto.Done rest x) -> return $ Done x (Chunk $ toWrap $ L.fromChunks [rest])
          (Atto.Partial z) -> return $
                              Cont (IterateeG $ f z) Nothing


-- | lazy bytestring -> wrapped bytestring
toWrap :: L.ByteString -> WrappedByteString Word8
toWrap = WrapBS . S.concat . L.toChunks
{-# INLINE toWrap #-}

-- | wrapped bytestring -> lazy bytestring
fromWrap :: WrappedByteString Word8 -> L.ByteString
fromWrap = L.fromChunks . (:[]) . unWrap
{-# INLINE fromWrap #-}
