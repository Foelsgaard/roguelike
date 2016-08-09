{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Options
       ( getOptions
       , boardFile
       )
       where

import Options.Generic

data Options
  = Options
    { board    :: FilePath
    }
  deriving (Generic, Show)

instance ParseRecord Options

getOptions :: IO Options
getOptions = getRecord "Roguelike"

boardFile :: Options -> FilePath
boardFile = board
