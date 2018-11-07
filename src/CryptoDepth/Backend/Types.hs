{-# LANGUAGE DeriveGeneric #-}
module CryptoDepth.Backend.Types where

import Data.Aeson
import GHC.Generics             (Generic)
import qualified Data.Text      as T
import Data.Word                (Word64)
import Data.List.NonEmpty       (NonEmpty)


type Venue = T.Text
type Sym   = T.Text

-- |
data SymSum = SymSum
    { ssSymbol  :: Sym
    , ssBuyQty  :: Word64
    , ssSellQty :: Word64
    } deriving (Show, Eq, Generic)

instance ToJSON SymSum where
  toJSON = genericToJSON
    defaultOptions { fieldLabelModifier = drop 2 }

instance FromJSON SymSum where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = drop 2 }

-- |
data SymPath = SymPath
    { spPath :: NonEmpty (Sym, Venue, Sym)
    , spQty  :: Word64
    } deriving (Show, Eq, Generic)

instance ToJSON SymPath where
  toJSON = genericToJSON
    defaultOptions { fieldLabelModifier = drop 2 }

instance FromJSON SymPath where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = drop 2 }

-- |
data BuySellPaths = BuySellPaths
    { bspBuy    :: [SymPath]
    , bspSell   :: [SymPath]
    } deriving (Show, Eq, Generic)

instance ToJSON BuySellPaths where
  toJSON = genericToJSON
    defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON BuySellPaths where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = drop 3 }
