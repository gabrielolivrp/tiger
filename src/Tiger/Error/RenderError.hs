module Tiger.Error.RenderError
  ( RenderError (..),
  )
where

import Data.ByteString

class RenderError a where
  renderError :: a -> ByteString
