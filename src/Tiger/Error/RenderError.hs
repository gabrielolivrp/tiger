module Tiger.Error.RenderError where

import Data.ByteString
import Prettyprinter

class RenderError a where
  renderError :: a -> ErrorRendered

data ErrorRendered = ErrorRendered
  { errTitle :: !String,
    errText :: !String
  }

instance Pretty ErrorRendered where
  pretty err =
    vsep
      [ pretty . errTitle $ err,
        pretty . errText $ err
      ]
