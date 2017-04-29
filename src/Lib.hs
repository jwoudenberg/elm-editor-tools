{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ParseElmModule.findDefinition
    , ResolveModule.resolveModule
    , Error.Error(..)
    ) where

import Error
import ParseElmModule
import ResolveModule
