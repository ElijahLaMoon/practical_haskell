module Chapter4.Client where

data Client = GovOrg      { clientId    :: Integer
                          , clientName  :: String }
            | Company     { clientId    :: Integer
                          , clientName  :: String
                          , person      :: Person
                          , duty        :: String }
            | Individual  { clientId    :: Integer
                          , person      :: Person }
            deriving Show

data Person = Person { firstName :: String
                     , lastName  :: String
                     } deriving Show