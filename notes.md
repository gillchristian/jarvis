- [ ] `! status` command to list all active Vaults / Notes and their corresponding channels
- [ ] "Effect" for the DB so we don't run on IO ? -> [CalamityBot/Db/Eff](https://github.com/simmsb/calamity-bot/blob/master/src/CalamityBot/Db/Eff.hs)

Getting a single message from Discord, attempts cache first otherwise fetches it

```haskell
import Jarvis.Prelude
import Calamity.Cache.Eff (getMessage)

oneMsg :: Sql.Connection -> IO (Maybe QueuedMsg)
oneMsg conn = do
  res <- Sql.query_ conn "SELECT * FROM message_queue LIMIT 1;"
  pure $ listToMaybe res

getLastMsg conn = do
  mbMsg <- P.embed $ oneMsg conn
  case mbMsg of
    Nothing -> DiP.info @Text "NO QUEUED MESSAGE -----"
    Just (QueuedMsg _ m c _) -> do
      res <- getMessage m <||> (hush <$> C.invoke (GetMessage c m))

      case res of
        Just fullMsg -> DiP.info $ "MSG: '" <> fullMsg ^. #content <> "'"
        Nothing -> DiP.error @Text "Could not get message"
```
