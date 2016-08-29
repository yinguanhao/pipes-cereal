{-# Language RankNTypes, LambdaCase, DeriveDataTypeable #-}
{- |
Decode @pipes@ byte streams with @cereal@ parsers.

In this module, the following type synonym compatible with the @lens@,
@lens-family@ and @lens-family-core@ libraries is used but not exported:

@
type Lens' a b = forall f . 'Functor' f => (b -> f b) -> (a -> f a)
@
-}
module Pipes.Cereal (
  -- * Encoders
  encode,
  encodePut,
  -- * Parsers
  decode,
  decodeEx,
  decodeGet,
  decodeGetEx,
  -- * Lenses
  decoded,
  decodedEx,
  -- * Decode multiple values
  decodedGet,
  decodedGetEx,
  -- * @DecodingError@
  DecodingError(..),

  -- * Re-exports

  {- |
  "Data.ByteString" re-exports 'ByteString'.

  "Data.Serialize" re-exports 'Serialize', 'Get' and 'Put'.

  "Pipes" re-exports 'Producer'.

  "Pipes.Parse" re-exports 'Parser', 'StateT', 'runStateT', 'evalStateT' and
  'execStateT'.

  "Control.Monad.Catch" re-exports 'MonadThrow'.
  -}
  module Data.ByteString,
  module Data.Serialize,
  module Pipes,
  module Pipes.Parse,
  module Control.Monad.Catch
) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import qualified Control.Monad.Catch as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize (Serialize, Get, Put)
import qualified Data.Serialize as S
import Data.Typeable (Typeable)
import Pipes (Producer)
import qualified Pipes as P
import Pipes.Parse (Parser, StateT, runStateT, evalStateT, execStateT)
import qualified Pipes.Parse as P

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- | Encode a value.
encode :: (Monad m, Serialize a) => a -> Producer ByteString m ()
encode = P.yield . S.encode

encodePut :: Monad m => Put -> Producer ByteString m ()
encodePut = P.yield . S.runPut

-- | Like 'decode' but uses an explicit 'Get'.
decodeGet :: Monad m => Get a -> Parser ByteString m (Either String a)
decodeGet g = go (S.runGetPartial g)
  where
    go f = do
      x <- draw1
      case f x of
        S.Done a x' -> unDraw1 x' >> return (Right a)
        S.Partial f' -> go f'
        S.Fail err x' -> unDraw1 x' >> return (Left err)

-- | Like 'decodeGet', but throws a 'DecodingError' if decoding failed.
decodeGetEx :: MonadThrow m => Get a -> Parser ByteString m a
decodeGetEx g = decodeGet g >>= \case
  -- The error message is too verbose and not very useful, omit for now.
  Left err -> C.throwM $ DecodingError err
  Right a -> return a

-- | Parse a value from a byte stream.
decode :: (Monad m, Serialize a) => Parser ByteString m (Either String a)
decode = decodeGet S.get

-- | Like 'decode', but throws a 'DecodingError' if decoding failed.
decodeEx :: (MonadThrow m, Serialize a) => Parser ByteString m a
decodeEx = decodeGetEx S.get

-- | Return 'B.empty' iff end of input.
draw1 :: Monad m => Parser ByteString m ByteString
draw1 = P.draw >>= \case
  Nothing -> return B.empty
  Just x
    | B.null x -> draw1
    | otherwise -> return x

-- | 'unDraw' if not null.
unDraw1 :: Monad m => ByteString -> Parser ByteString m ()
unDraw1 x
  | B.null x = return ()
  | otherwise = P.unDraw x

-- | Improper lens between stream of bytes and stream of decoded values.
decoded :: (Monad m, Serialize a) => Lens' (Producer ByteString m r)
  (Producer a m (Either (String, Producer ByteString m r) r))
decoded k p = fmap bw (k ((decodedGet S.get) p))
  where
    bw p1 = do
      er <- P.for p1 encode
      case er of
        Left (_, p2) -> p2
        Right r -> return r

decodedEx :: (MonadThrow m, Serialize a) => Lens' (Producer ByteString m r)
  (Producer a m r)
decodedEx k p = fmap bw (k ((decodedGetEx S.get) p))
  where
    bw p1 = P.for p1 encode

-- | Turns a stream of bytes into a stream of decoded values, using an
-- explicit 'Get'.
decodedGet :: Monad m => Get a -> Producer ByteString m r ->
  Producer a m (Either (String, Producer ByteString m r) r)
decodedGet g p = do
  P.lift (next1 p) >>= \case
    Left r -> return (Right r)
    Right (b, p') -> P.lift (runStateT (decodeGet g) (P.yield b >> p')) >>= \case
      (Left err, p'') -> return (Left (err, p''))
      (Right a, p'') -> P.yield a >> decodedGet g p''

-- | Like 'P.next', but skip null chunks
next1 :: Monad m => Producer ByteString m r ->
  m (Either r (ByteString, Producer ByteString m r))
next1 p0 = P.next p0 >>= \case
  l@(Left _) -> return l
  r@(Right (b, p1))
    | B.null b -> next1 p1
    | otherwise -> return r

-- | Like 'decodedGet', but throws a 'DecodingError' if decoding failed.
decodedGetEx :: MonadThrow m => Get a -> Producer ByteString m r ->
  Producer a m r
decodedGetEx g p = decodedGet g p >>= \case
  Right r -> return r
  Left (err, _) -> P.lift . C.throwM $ DecodingError err

data DecodingError = DecodingError String
  deriving (Typeable)

-- Don't derive Show.
instance Show DecodingError where
  show (DecodingError s) = "decoding error: " ++ s

instance Exception DecodingError
