{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Spork.Generics (is) where

import Data.Data

--data PhpValue = VoidValue | IntValue Integer | BoolValue Bool 
--              deriving (Typeable, Data)


is :: Data a => a-> String -> Bool
is x connm = connm == (showConstr $ toConstr x)
