{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module CryptoDepth.Backend.Api where

import CryptoDepth.Backend.Types

import Servant.API
import qualified Data.Text      as T


type Api = Numeraires :<|> SymSums :<|> SymPaths

-- | Which currencies can we measure volume in?
type Numeraires
    = "numeraires"
    :> Get '[JSON] [T.Text]

--  Base path for sums and paths
-- type SymBase
--     =  -- Which currency to measure volume in? (e.g. USD, EUR, GBP)
--        Capture "numeraire"  T.Text
--        -- Slippage divisor (slippage_percent = 1/"slippage"). E.g. 100=1% slippage, 200=0.5%, 20=5% etc.
--     :> Capture "slippage"   Word
--        -- Symbol in question (e.g BTC, ETH, LTC)
--     :> Capture "symbol"     Sym

-- | Symbol sums
type SymSums
    = -- Which currency to measure volume in? (e.g. USD, EUR, GBP)
       Capture "numeraire"  T.Text
       -- Slippage divisor (slippage_percent = 1/"slippage"). E.g. 100=1% slippage, 200=0.5%, 20=5% etc.
    :> Capture "slippage"   Word
       -- Symbol in question (e.g BTC, ETH, LTC)
    :> Capture "symbol"     Sym
    :> "sums"
    :> Capture "page"       Word
    :> Get '[JSON] [SymSum]

-- | Symbol paths
type SymPaths
    = -- Which currency to measure volume in? (e.g. USD, EUR, GBP)
       Capture "numeraire"  T.Text
       -- Slippage divisor (slippage_percent = 1/"slippage"). E.g. 100=1% slippage, 200=0.5%, 20=5% etc.
    :> Capture "slippage"   Word
       -- Symbol in question (e.g BTC, ETH, LTC)
    :> Capture "symbol"     Sym
    :> "paths"
    :> Get '[JSON] BuySellPaths
