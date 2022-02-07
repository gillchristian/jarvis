{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Calamity (ChannelRequest (..), EventType (..), RawEmoji (..), Token (..))
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
import Data.Maybe (fromMaybe, listToMaybe)
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
            note <- P.embed $ getChannelNote conn $ msg ^. #channelID
            P.embed $ insertMsg conn $ QueuedMsg Pending (msg ^. #id) (msg ^. #channelID) note

            void . C.invoke $ CreateReaction msg msg $ UnicodeEmoji "⌛"

        void $ C.react @'MessageDeleteEvt $ \msg ->
          P.embed $ delMsg conn $ msg ^. #id

        Cmds.addCommands $ do
          void Cmds.helpCommand

          void $ Cmds.command @'[Text] "note" $ \ctx note -> do
            let cn = ChannelNote (ctx ^. #message . #channelID) note
            P.embed $ upsertChannelNote conn cn
            P.embed $ setNoteOnNotelessMsgs conn cn

            void . C.tell @Text ctx $ "Got it, will save messages to '" <> note <> "'"

            void . C.invoke $ CreateReaction (ctx ^. #channel) (ctx ^. #message) (UnicodeEmoji "✅")

          void $ Cmds.command @'[] "clear-note" $ \ctx -> do
            P.embed $ clearNoteForChannel conn $ ctx ^. #message . #channelID

            void . C.tell @Text ctx $ "Note cleared :ok_hand:"

          void $ Cmds.command @'[] "status" $ \ctx -> do
            (pending, noteless) <- P.embed $ channelStats conn $ ctx ^. #message . #channelID

            case pending of
              0 -> void . C.tell @String ctx $ "All messages processed :writing_hand:"
              1 -> void . C.tell ctx $ show pending <> " message pending"
              n -> void . C.tell ctx $ show n <> " messages pending"

            case noteless of
              0 -> void . C.tell @String ctx $ "All messages have a note assigned :rocket:"
              1 -> void . C.tell ctx $ show pending <> " message without a note assigned"
              n -> void . C.tell ctx $ show n <> " messages without a note assigned"

            mbNote <- P.embed $ getChannelNote conn $ ctx ^. #message . #channelID

            for_ mbNote $ \note ->
              void . C.tell ctx $ "The channel note is **" <> note <> "**"

-- -----------------------------------------------------------------------------
initDB :: IO Sql.Connection
initDB = do
  home <- Dir.getHomeDirectory

  conn <- Sql.open $ home </> ".jarvisdata"

  Sql.execute_ conn Q.createChannelActiveNotes
  Sql.execute_ conn Q.createMessageQueueTable

  pure conn

upsertChannelNote :: Sql.Connection -> ChannelNote -> IO ()
upsertChannelNote conn c@(ChannelNote chan note) = do
  res :: [ChannelNote] <- Sql.query conn Q.selectChannelNote $ Sql.Only $ Json.encode chan
  case res of
    [] -> Sql.execute conn Q.insertChannelNote c
    _ -> Sql.execute conn Q.updateNoteForChannel (note, Json.encode chan)

getChannelNote :: Sql.Connection -> Snowflake Channel -> IO (Maybe Text)
getChannelNote conn chan = do
  res :: [ChannelNote] <- Sql.query conn Q.selectChannelNote $ Sql.Only $ Json.encode chan
  pure $ chanNote <$> listToMaybe res

pendingMsgs :: Sql.Connection -> IO [QueuedMsg]
pendingMsgs conn = Sql.query_ conn Q.allPendingMsgs

allChannelNotes :: Sql.Connection -> IO [ChannelNote]
allChannelNotes conn = Sql.query_ conn Q.allChannelNotes

setNoteOnNotelessMsgs :: Sql.Connection -> ChannelNote -> IO ()
setNoteOnNotelessMsgs conn (ChannelNote chan note) =
  Sql.execute conn Q.updateNoteForMsg (note, Json.encode chan)

channelStats :: Sql.Connection -> Snowflake Channel -> IO (Int, Int)
channelStats conn chan = do
  res <- Sql.query conn Q.channelStats $ Sql.Only $ Json.encode chan
  case res of
    (Just pending, Just noteless) : _ -> pure (pending, noteless)
    _ -> pure (0, 0)

clearNoteForChannel :: Sql.Connection -> Snowflake Channel -> IO ()
clearNoteForChannel conn = Sql.execute conn Q.delNoteOfChannel . Sql.Only . Json.encode

insertMsg :: Sql.Connection -> QueuedMsg -> IO ()
insertMsg conn = Sql.execute conn Q.insertMsg

delMsg :: Sql.Connection -> Snowflake Message -> IO ()
delMsg conn = Sql.execute conn Q.delMsg . Sql.Only . Json.encode

-- -----------------------------------------------------------------------------

data ChannelNote = ChannelNote
  { chanChan :: Snowflake Channel,
    chanNote :: Text
  }
  deriving (Eq, Show)

instance ToRow ChannelNote where
  toRow :: ChannelNote -> [SQLData]
  toRow (ChannelNote chan note) = toRow (Json.encode chan, note)

instance FromRow ChannelNote where
  fromRow :: RowParser ChannelNote
  fromRow = do
    ChannelNote
      -- TODO: use something better than error here /shrug
      <$> (fromMaybe (Prelude.error "Could not decode channel_id") . Json.decode <$> Sql.field)
      <*> Sql.field

data MsgStatus
  = Pending
  | Saving
  | Done
  deriving (Eq, Show)

instance ToText MsgStatus where
  toText Pending = "pending"
  toText Saving = "saving"
  toText Done = "done"

instance ToField MsgStatus where
  toField = SQLText . toText

instance FromField MsgStatus where
  fromField :: FieldParser MsgStatus
  fromField fieldValue =
    case Sql.fieldData fieldValue of
      SQLText "pending" -> Ok Pending
      SQLText "saving" -> Ok Saving
      SQLText "done" -> Ok Done
      other -> Errors [SomeException $ Sql.Incompatible (show other) "MsgStatus" ""]

data QueuedMsg = QueuedMsg
  { msgStatus :: MsgStatus,
    msgMsg :: Snowflake Message,
    msgChan :: Snowflake Channel,
    msgNote :: Maybe Text
  }
  deriving (Show, Eq)

instance ToRow QueuedMsg where
  toRow :: QueuedMsg -> [SQLData]
  toRow (QueuedMsg sts msg chan note) =
    toRow (sts, Json.encode msg, Json.encode chan, note)

instance FromRow QueuedMsg where
  fromRow :: RowParser QueuedMsg
  fromRow = do
    QueuedMsg
      <$> Sql.field
      -- TODO: use something better than error here /shrug
      <*> (fromMaybe (Prelude.error "Could not decode message_id") . Json.decode <$> Sql.field)
      -- TODO: use something better than error here /shrug
      <*> (fromMaybe (Prelude.error "Could not decode channel_id") . Json.decode <$> Sql.field)
      <*> Sql.field
