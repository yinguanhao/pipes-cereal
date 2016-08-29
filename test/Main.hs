{-# Language OverloadedStrings, LambdaCase #-}
module Main(main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Either
import Data.IORef
import Data.Serialize hiding (decode)
import Test.Hspec
import Pipes
import Pipes.Cereal
import Pipes.Parse
import Pipes.Prelude (toListM)

main :: IO ()
main = hspec $ do
  describe "decodeGet" $ do
    it "should work with exactly matched chunks" $ do
      let p7 = decodeGet (getBytes 7)
          p = replicateM 7 p7
      (rs, pro1) <- runStateT p pro
      take 6 rs `shouldBe` map Right l1
      let [r6] = drop 6 rs
      r6 `shouldSatisfy` isLeft
      remaning <- toListM pro1
      B.concat remaning `shouldBe` "6xxx"

    it "should work with leftovers" $ do
      let p13 = decodeGet (getBytes 13)
          p = liftM4 (,,,) p13 p13 p13 p13
      ((b1, b2, b3, b4), pro1) <- runStateT p pro
      (b1, b2, b3) `shouldBe` (Right "0xxxxxx1xxxxx",
                               Right "x2xxxxxx3xxxx",
                               Right "xx4xxxxxx5xxx")
      b4 `shouldSatisfy` isLeft
      x <- toListM pro1
      B.concat x `shouldBe` "xxx6xxx"

  describe "decodedGet" $ do
    it "should work with exactly matched chunks" $ do
      let pro1 = decodedGet (getBytes 7) pro
      (bs, pro2) <- runStateT drawAll pro1
      bs `shouldBe` l1
      (Left (Left (_, pro3))) <- next pro2
      x <- toListM pro3
      x `shouldBe` ["6xxx"]

    it "should work with leftovers" $ do
      let pro1 = decodedGet (getBytes 13) pro
      (bs, pro2) <- runStateT drawAll pro1
      bs `shouldBe` [ "0xxxxxx1xxxxx",
                      "x2xxxxxx3xxxx",
                      "xx4xxxxxx5xxx" ]
      (Left (Left (_, pro3))) <- next pro2
      bs1 <- toListM pro3
      B.concat bs1 `shouldBe` "xxx6xxx"

  describe "decodeGetEx" $ do
    it "should throw iff decoding failed" $ do
      (r, pro1) <- runStateT (decodeGetEx (getBytes 42)) pro
      r `shouldBe` B.concat l1
      runStateT (decodeGetEx (getBytes 5)) pro1 `shouldThrow` anyDecodingError

  describe "decodedGetEx" $ do
    it "should throw iff decoding failed" $ do
      rs <- newIORef []
      let p = decodedGetEx (getBytes 7) pro
      flip shouldThrow anyDecodingError $
        runEffect $ for p $ \x -> lift $ modifyIORef' rs (x:)
      readIORef rs `shouldReturn` reverse l1

    it "should not throw if given an empty stream" $ do
      let p = decodedGetEx (getBytes 7) (return ())
      r <- tryDecodingError $ runEffect $ for p $ \_ -> return ()
      r `shouldSatisfy` isRight

    it "should not throw if stream ends right after a successful match" $ do
      let p1 = decodedGetEx (getBytes 7) (mapM_ yield l1)
      r <- tryDecodingError $ runEffect $ for p1 $ \_ -> return ()
      r `shouldSatisfy` isRight

anyDecodingError :: DecodingError -> Bool
anyDecodingError _ = True

tryDecodingError :: IO a -> IO (Either DecodingError a)
tryDecodingError = try

l1 :: [ByteString]
l1 = [
  "0xxxxxx",
  "1xxxxxx",
  "2xxxxxx",
  "3xxxxxx",
  "4xxxxxx",
  "5xxxxxx"
  ]

pro :: Monad m => Producer ByteString m ()
pro = do
  mapM_ yield l1
  yield "6xxx"
