module TimeMachine where

data TimeMachine  = TimeMachine
  { manufacturer :: Manufacturer
  , model        :: Model
  , name         :: Name
  , canTravelTo  :: CanTravelTo
  , price        :: Price
  } deriving Show

newtype Manufacturer = Manufacturer String deriving Show

newtype Model = Model Integer deriving Show

newtype Name = Name String deriving Show

data CanTravelTo = Past | Future | PastAndFuture deriving Show

newtype Price = Price Double deriving Show

seasonDiscount :: [TimeMachine] -> Double -> [TimeMachine]
seasonDiscount tms discount = map applyDiscount tms
  where applyDiscount = \tm@TimeMachine { price = Price p } -> tm { price = Price (p * discount) }