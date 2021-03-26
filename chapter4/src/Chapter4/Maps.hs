{-# LANGUAGE LambdaCase #-}

module Chapter4.Maps where

import qualified Data.Map as M

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k a = M.alter (\case Just _  -> Just a
                            Nothing -> Just a) k

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete = M.alter $ const Nothing

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f = M.alter (\case (Just v) -> Just $ f v
                          Nothing  -> Nothing)