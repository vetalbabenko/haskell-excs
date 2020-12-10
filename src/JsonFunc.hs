{-# LANGUAGE OverloadedStrings #-}
module JsonFunc where
import Blocks
import Data.Aeson
import qualified Data.Text as T
import Control.Monad

-- Instances for correctly convert block to json
instance ToJSON Block where
 toJSON Block {index=i, transactions=ts, timestamp=t, previousHash=ph, nonce=n} =
    object [ "index"        .= i
           , "transactions" .= ts
           , "timestamp"    .= t
           , "previousHash" .= ph
           , "nonce"        .= n
           ]
           
instance FromJSON Block where
 parseJSON (Object v) =
    Block <$> v .: "index"
           <*> v .: "transactions"
           <*> v .: "timestamp"
           <*> v .: "previousHash"
           <*> v .: "nonce"
 parseJSON _ = mzero           
           