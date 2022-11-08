module Jarvis.Prelude where

hush :: Either b a -> Maybe a
hush = either (const Nothing) Just

-- TODO: Is there some existing way to do this?
-- TODO: Better operator (or name)
-- TODO: MaybeT should work (it has an instance_ of `Alternative`) but the
--       problem is the `m` in Polysemy xD
-- Cannot use `liftM2 (<|>)` because it will run both monadic actions
(<||>) :: (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
mfx <||> mfy = do
  fx <- mfx
  case fx of
    Just x -> pure $ Just x
    Nothing -> mfy
