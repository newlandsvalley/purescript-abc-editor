module FileIO.FileIO
  ( FILEIO
  , Filespec
  , loadTextFile
  , saveTextFile ) where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, makeAff)


type Filespec =
  {
    contents :: String
  , name :: String
  }

-- | File IO Effect
foreign import data FILEIO :: !

foreign import loadTextFileImpl :: forall e. (Filespec -> Eff e Unit) -> Eff e Unit

loadTextFile :: forall e. Aff e Filespec
loadTextFile = makeAff (\error success -> loadTextFileImpl success)

foreign import saveTextFile :: forall eff. Filespec -> Eff (fileio :: FILEIO | eff) Unit
