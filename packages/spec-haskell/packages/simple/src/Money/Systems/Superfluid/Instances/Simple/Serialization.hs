{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Money.Systems.Superfluid.Instances.Simple.Serialization (SimpleSerialized) where

import qualified Data.Binary                                       as B (Binary (..))
import qualified Data.Binary.Get                                   as B (Get, runGet)
import qualified Data.Binary.Put                                   as B (PutM, runPut)
import           Data.ByteString.Lazy                              (ByteString)
import           Data.Coerce

import           Money.Systems.Superfluid.Instances.Simple.SuperfluidTypes (SFDouble (..), SimpleSuperfluidTypes)
import qualified Money.Systems.Superfluid.System.Serialization             as S

newtype SimpleSerialized = SimpleSerialized ByteString
newtype SimpleGetter a = SimpleGetter (B.Get a) deriving (Functor, Applicative, Monad)
newtype SimplePutter a = SimplePutter (B.PutM a) deriving (Functor, Applicative, Monad)

instance S.Getter SimpleGetter SimpleSuperfluidTypes where
    getFloat = SFDouble <$> SimpleGetter B.get
    getLQ = SimpleGetter B.get
    getTS = SimpleGetter B.get

instance S.Putter SimplePutter SimpleSuperfluidTypes where
    putFloat x = SimplePutter . B.put $ (coerce x :: Double)
    putLQ = SimplePutter . B.put
    putTS = SimplePutter . B.put

instance S.Serialized SimpleSerialized SimpleSuperfluidTypes where
    runGetter taggedProxy (SimpleSerialized s) = B.runGet m s where (SimpleGetter m) = S.getter taggedProxy
    runPutter a = SimpleSerialized $ B.runPut m where (SimplePutter m) = S.putter a
