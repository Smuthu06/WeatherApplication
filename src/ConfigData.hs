{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module ConfigData
    ( readSections,
      readKeys,
      printItems,
      readInt
    ) where

import qualified Data.Text as T
import           Data.Ini ( keys, readIniFile, sections, Ini )
import           Control.Monad.Trans.State ( StateT(runStateT), get )
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)


-- | Read section information from given Ini file path
readSections :: StateT FilePath IO [T.Text]
readSections = do
    -- get the config path from the StateT
    path <- get
    config <- liftIO $ readIniFile path
    -- readIniFile :: IO(Either String Ini)
    -- liftIO used to Lift the IO monad
    case config of
        -- If error return empty
        Left _ -> pure []
        -- Sucessfull read return sections from ini
        Right ini -> pure (sections ini)

-- | Read all key from the given section
readKeys :: T.Text -> StateT FilePath IO [T.Text]
readKeys section = do
    -- get the path from State
    path <- get
    -- read configuration ini
    config <- liftIO $ readIniFile path
    case config of
        -- if error return empty
        Left _ -> pure []
        -- sucessfull read return the key information for given section
        Right ini -> pure (rKey section ini)

rKey :: T.Text -> Ini -> [T.Text]
rKey sec ini = case keys sec ini of
  Left _ -> []
  Right txts -> txts

-- | Print the given list of element into user selectable format
printItems :: [T.Text] -> String
printItems items = foldr (\(n,i) b
                    -- Convert the Item in to Printable String
                    -> "\n"++ show n ++ ". " ++ T.unpack i ++ "\n" ++ b)
                    "" $ zip [1..] items

-- | Read user input as Integer
readInt :: IO Int
readInt = readLn