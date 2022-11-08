{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Calamity.Types.Upgradeable (Upgradeable(upgrade))
import Calamity (EventType (..), Token (..), Partial (PartialChannel))
import qualified Calamity as C
import qualified Calamity.Cache.InMemory as MemCache
import qualified Calamity.Commands as Cmds
import Calamity.Commands.Context (useFullContext)
import qualified Calamity.Metrics.Noop as Metrics
import Calamity.Types (Channel, Message, Snowflake)
import Control.Exception (SomeException (SomeException))
import Control.Lens
import Control.Monad
import qualified Data.Aeson as Json
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, listToMaybe, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T hiding (empty)
import qualified Data.Text.IO as TIO
import Data.Text.ToText
import Database.SQLite.Simple (SQLData (SQLText))
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField (FieldParser, FromField (fromField))
import qualified Database.SQLite.Simple.FromField as Sql
import Database.SQLite.Simple.FromRow (FromRow, RowParser)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import qualified Df1
import qualified Di
import qualified Di.Core as DiC
import qualified DiPolysemy as DiP
import qualified Jarvis.Query as Q
import qualified Polysemy as P
import qualified Polysemy.Trace as Trace
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Calamity.Commands (Named, KleenePlusConcat)

-- TODO: See here: https://github.com/MorrowM/pandabot-discord/blob/4faa64623e9f79c3853b727b2a154bc5da478bbb/src/Pandabot/Bot/Config.hs
prefix :: Text
prefix = "!"

-- TODO: get log level from config
-- TODO: only filter token log instead?
filterDi :: DiC.Di Di.Level path msg -> DiC.Di Di.Level path msg
-- filterDi = DiC.filter $ \_ _ _ -> True
filterDi = DiC.filter $ \l _ _ -> l > Df1.Debug

runBot :: IO ()
runBot = do
  home <- Dir.getHomeDirectory
  token <- T.strip <$> TIO.readFile (home </> ".jarvistoken")

  conn <- initDB

  Di.new $ \di ->
    void
      . P.runFinal
      . P.embedToFinal @IO
      . DiP.runDiToIO di
      . DiP.local filterDi
      . MemCache.runCacheInMemory
      . Metrics.runMetricsNoop
      . Trace.traceToIO
      . useFullContext
      . Cmds.useConstantPrefix prefix
      . C.runBotIO (BotToken token) C.defaultIntents
      $ do
        DiP.info @Text "Setting up commands and handlers..."

        void $ C.react @'MessageCreateEvt $ \(msg, usr, _member) -> do
          let isBot = usr ^? (_Just . #bot . _Just) == Just True
              isCommand = T.isPrefixOf prefix $ msg ^. #content


          when (not isBot && not isCommand) $ do
            mbNote <- P.embed $ getChannelNote conn $ msg ^. #channelID
            mbVault <- P.embed $ getChannelVault conn $ msg ^. #channelID

            for_ mbNote $ \note ->
              P.embed $ insertMsg conn $ QueuedMsg (msg ^. #id) (msg ^. #content) note mbVault ModeJournal

        void $ C.react @'MessageUpdateEvt $ \(msg, _, usr, _member) -> do
          let isBot = usr ^? (_Just . #bot . _Just) == Just True
              isCommand = T.isPrefixOf prefix $ msg ^. #content

          queuedMsg <- P.embed $ getMsg conn $ msg ^. #id

          when (not isBot && not isCommand && isJust queuedMsg) $ do
            mbNote <- P.embed $ getChannelNote conn $ msg ^. #channelID
            mbVault <- P.embed $ getChannelVault conn $ msg ^. #channelID

            for_ mbNote $ \note ->
              P.embed $ insertMsg conn $ QueuedMsg (msg ^. #id) (msg ^. #content) note mbVault ModeJournal

        void $ C.react @'MessageDeleteEvt $ \msg ->
          P.embed $ delMsg conn $ msg ^. #id

        Cmds.addCommands $ do
          void Cmds.helpCommand

          void $ Cmds.command @'[Text] "clear" $ \ctx what ->
            case what of
              "note" -> do
                P.embed $ clearNoteForChannel conn $ ctx ^. #message . #channelID
                void . C.tell @Text ctx $ "Note cleared :ok_hand:"
              "vault" -> do
                P.embed $ clearVaultForChannel conn $ ctx ^. #message . #channelID
                void . C.tell @Text ctx $ "Vault cleared :ok_hand:"
              unknown -> 
                void . C.tell @Text ctx $ "Note sure what _" <> unknown <> "_ is.\nI can clear 'vault' and 'note' :wink:"

          void $ Cmds.command @'[Named "note" (KleenePlusConcat Text)] "note" $ \ctx note -> do
            let cnId = ctx ^. #message . #channelID

            mbChan <- upgrade cnId

            let cnName = maybe "<missing-channel-name>" channelName $ fmap _what mbChan
                cn = ChannelNote cnId cnName note

            P.embed $ upsertChannelNote conn cn

            let cn = ChannelNote cnId note

            void . C.tell @Text ctx $ "Got it, will save messages to note: **" <> note <> "**"

          void $ Cmds.command @'[Named "vault" (KleenePlusConcat Text)] "vault" $ \ctx vault -> do
            let cnId = ctx ^. #message . #channelID

            mbChan <- upgrade cnId
                
            let cnName = maybe "<missing-channel-name>" channelName $ fmap _what mbChan
                cn = ChannelVault cnId cnName vault

            P.embed $ upsertChannelVault conn cn
            P.embed $ clearNoteForChannel conn cnId

            void . C.tell @Text ctx
              $ "Got it, will save messages to vault: **" <> vault <> "**\n\nMake sure to set a note as well ..."

          -- --- Misc Commands ---------
          void $ Cmds.command @'[] "status" $ \ctx -> do
            pendingByVaultAndNote <- P.embed $ channelStats conn
            mbNote <- P.embed $ getChannelNote conn $ ctx ^. #message . #channelID
            mbVault <- P.embed $ getChannelVault conn $ ctx ^. #message . #channelID


            void . C.tell ctx $
              T.unlines $ catMaybes $
                [ ("The channel's note is **" <>) . (<> "**") <$> mbNote
                , ("The channel's vault is **" <>) . (<> "**") <$> mbVault
                , case (mbNote, mbVault) of
                    (Nothing, Nothing) -> Just "_No vault and note set_"
                    _ -> Nothing
                , if null pendingByVaultAndNote
                     then Just "\n_No pending messages_ :rocket:"
                     else Just "\n**Pending messages**:"
                ] ++ map fmtPendingVaultNote pendingByVaultAndNote

fmtPendingVaultNote :: (Maybe Text, Text, Int) -> Maybe Text
fmtPendingVaultNote (_, _, 0) = Nothing
fmtPendingVaultNote (Just vault, note, count) =
  Just $ vault <> " / " <> note <> " => " <> pluralizeMsg count
fmtPendingVaultNote (Nothing, note, count) =
  Just $ "<any vault> / " <> note <> " => " <> pluralizeMsg count

pluralizeMsg :: Int -> Text
pluralizeMsg 1 = "1 message"
pluralizeMsg count = T.pack (show count) <> " messages"

channelName :: Partial Channel -> Text
channelName (PartialChannel _ name _ _) = name

-- -----------------------------------------------------------------------------
initDB :: IO Sql.Connection
initDB = do
  home <- Dir.getHomeDirectory

  conn <- Sql.open $ home </> ".jarvisdata"

  Sql.execute_ conn Q.createChannelActiveNoteTable
  Sql.execute_ conn Q.createChannelActiveVaultTable
  Sql.execute_ conn Q.createMsgQueueTable

  pure conn

upsertChannelVault :: Sql.Connection -> ChannelVault -> IO ()
upsertChannelVault conn c@(ChannelVault chanId chanName vault) = do
  res :: [ChannelVault] <- Sql.query conn Q.selectChannelVault $ Sql.Only $ Json.encode chanId
  case res of
    [] -> Sql.execute conn Q.insertChannelVault c
    _ -> Sql.execute conn Q.updateVaultForChannel (vault, Json.encode chanId, chanName)

upsertChannelNote :: Sql.Connection -> ChannelNote -> IO ()
upsertChannelNote conn c@(ChannelNote chanId chanName note) = do
  res :: [ChannelNote] <- Sql.query conn Q.selectChannelNote $ Sql.Only $ Json.encode chanId
  case res of
    [] -> Sql.execute conn Q.insertChannelNote c
    _ -> Sql.execute conn Q.updateNoteForChannel (note, Json.encode chanId, chanName)

getChannelNote :: Sql.Connection -> Snowflake Channel -> IO (Maybe Text)
getChannelNote conn chan = do
  res :: [ChannelNote] <- Sql.query conn Q.selectChannelNote $ Sql.Only $ Json.encode chan
  pure $ noteName <$> listToMaybe res

getChannelVault :: Sql.Connection -> Snowflake Channel -> IO (Maybe Text)
getChannelVault conn chan = do
  res :: [ChannelVault] <- Sql.query conn Q.selectChannelVault $ Sql.Only $ Json.encode chan
  pure $ vaultName <$> listToMaybe res

channelStats :: Sql.Connection -> IO [(Maybe Text, Text, Int)]
channelStats conn = Sql.query_ conn Q.channelStats

clearNoteForChannel :: Sql.Connection -> Snowflake Channel -> IO ()
clearNoteForChannel conn = Sql.execute conn Q.delNoteOfChannel . Sql.Only . Json.encode

clearVaultForChannel :: Sql.Connection -> Snowflake Channel -> IO ()
clearVaultForChannel conn = Sql.execute conn Q.delVaultOfChannel . Sql.Only . Json.encode

insertMsg :: Sql.Connection -> QueuedMsg -> IO ()
insertMsg conn = Sql.execute conn Q.insertMsg

getMsg :: Sql.Connection -> Snowflake Message -> IO (Maybe QueuedMsg)
getMsg conn msgId =
  fmap listToMaybe <$> Sql.query conn Q.getMsg $ Sql.Only $ Json.encode msgId

delMsg :: Sql.Connection -> Snowflake Message -> IO ()
delMsg conn = Sql.execute conn Q.delMsg . Sql.Only . Json.encode

-- -----------------------------------------------------------------------------

data ChannelVault = ChannelVault
  { vaultChanId :: Snowflake Channel,
    vaultChanName :: Text,
    vaultName :: Text
  }
  deriving (Eq, Show)

instance ToRow ChannelVault where
  toRow :: ChannelVault -> [SQLData]
  toRow (ChannelVault chanId chanName note) =
    toRow (Json.encode chanId, chanName, note)

instance FromRow ChannelVault where
  fromRow :: RowParser ChannelVault
  fromRow = do
    ChannelVault
      -- TODO: use something better than error here /shrug
      <$> (fromMaybe (Prelude.error "Could not decode channel_id") . Json.decode <$> Sql.field)
      <*> Sql.field
      <*> Sql.field

data ChannelNote = ChannelNote
  { noteChanId :: Snowflake Channel,
    noteChanName :: Text,
    noteName :: Text
  }
  deriving (Eq, Show)

instance ToRow ChannelNote where
  toRow :: ChannelNote -> [SQLData]
  toRow (ChannelNote chanId chanName note) =
    toRow (Json.encode chanId, chanName, note)

instance FromRow ChannelNote where
  fromRow :: RowParser ChannelNote
  fromRow = do
    ChannelNote
      -- TODO: use something better than error here /shrug
      <$> (fromMaybe (Prelude.error "Could not decode channel_id") . Json.decode <$> Sql.field)
      <*> Sql.field
      <*> Sql.field

data MsgMode
  = ModeJournal
  deriving (Eq, Show)

instance ToText MsgMode where
  toText ModeJournal = "journal"

instance ToField MsgMode where
  toField = SQLText . toText

instance FromField MsgMode where
  fromField :: FieldParser MsgMode
  fromField fieldValue =
    case Sql.fieldData fieldValue of
      SQLText "journal" -> Ok ModeJournal
      other -> Errors [SomeException $ Sql.Incompatible (show other) "MsgMode" ""]

data QueuedMsg = QueuedMsg
  { msgMsg :: Snowflake Message,
    msgContent :: Text,
    msgNote :: Text,
    msgVault :: Maybe Text,
    msgMode :: MsgMode
  }
  deriving (Show, Eq)

instance ToRow QueuedMsg where
  toRow :: QueuedMsg -> [SQLData]
  toRow (QueuedMsg msg content note vault mode) =
    toRow (Json.encode msg, content, note, vault, mode)

instance FromRow QueuedMsg where
  fromRow :: RowParser QueuedMsg
  fromRow =
    QueuedMsg
      <$> (fromMaybe (Prelude.error "Could not decode message_id") . Json.decode <$> Sql.field)
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
