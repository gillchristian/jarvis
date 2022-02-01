module Lib where

import Calamity (ChannelRequest (..), EventType (..), RawEmoji (..), Token (..))
import qualified Calamity as C
import qualified Calamity.Cache.InMemory as MemCache
import qualified Calamity.Commands as Cmds
import Calamity.Commands.Context (useFullContext)
import qualified Calamity.Metrics.Noop as Metrics
import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Df1
import qualified Di
import qualified Di.Core as DiC
import DiPolysemy
import qualified Polysemy as P
import qualified Polysemy.Trace as Trace
import qualified System.Directory as Dir
import System.FilePath ((</>))

-- TODO: Get prefix from context.
--       See here: https://github.com/MorrowM/pandabot-discord/blob/4faa64623e9f79c3853b727b2a154bc5da478bbb/src/Pandabot/Bot/Config.hs
prefix :: Text
prefix = "!"

-- TODO: get log level from config
filterDi :: DiC.Di Di.Level path msg -> DiC.Di Di.Level path msg
filterDi = DiC.filter $ \l _ _ -> l > Df1.Debug

runBot :: IO ()
runBot = do
  home <- Dir.getHomeDirectory
  token <- T.strip <$> TIO.readFile (home </> ".jarvistoken")

  Di.new $ \di ->
    void
      . P.runFinal
      . P.embedToFinal @IO
      . runDiToIO di
      . DiPolysemy.local filterDi
      . MemCache.runCacheInMemory
      . Metrics.runMetricsNoop
      . Trace.traceToIO
      . useFullContext
      . Cmds.useConstantPrefix prefix
      . C.runBotIO (BotToken token) C.defaultIntents
      $ do
        info @Text "Setting up commands and handlers..."

        void $ C.react @'MessageCreateEvt $ \(msg, usr, _member) -> do
          let isBot = usr ^? (_Just . #bot . _Just) == Just True
              isCommand = T.isPrefixOf prefix $ msg ^. #content

          when (not isBot && not isCommand) $ do
            -- TODO: get the channel's note
            -- TODO: queue msg
            Trace.trace $ T.unpack $ msg ^. #content
            void . C.invoke $ CreateReaction msg msg (UnicodeEmoji "⌛")

        void $ C.react @'MessageUpdateEvt $ \(_oldMsg, newMsg, usr, _member) -> do
          let isBot = usr ^? (_Just . #bot . _Just) == Just True
              isCommand = T.isPrefixOf prefix $ newMsg ^. #content

          when (not isBot && not isCommand) $ do
            -- TODO: get the channel's note
            -- TODO: update queued up msg (only if not processed yet)
            Trace.trace $ T.unpack $ newMsg ^. #content
            void . C.invoke $ CreateReaction newMsg newMsg (UnicodeEmoji "⌛")

        Cmds.addCommands $ do
          void Cmds.helpCommand

          Cmds.command @'[Text] "note" $ \ctx note -> do
            -- TODO: save note for channel
            -- TODO: update all queued up msgs with this new note (the ones that don't have one)
            void . C.tell @Text ctx $ "Got it, will save messages to " <> note

            void . C.invoke $
              CreateReaction (ctx ^. #channel) (ctx ^. #message) (UnicodeEmoji "✅")
