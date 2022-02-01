module Lib where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (useFullContext)
import Calamity.Metrics.Noop
import Control.Lens
import Control.Monad
import Data.Default
import Data.Generics.Labels ()
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Di
import DiPolysemy
import qualified Polysemy as P
import qualified Polysemy.Trace as Trace

-- TODO: get prefix from context. See here: https://github.com/MorrowM/pandabot-discord/blob/4faa64623e9f79c3853b727b2a154bc5da478bbb/src/Pandabot/Bot/Config.hs
prefix :: Text
prefix = "!"

runBot :: IO ()
runBot = do
  token <- T.strip <$> TIO.readFile "token.txt"
  Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . Trace.traceToIO
    . useFullContext
    . useConstantPrefix prefix
    . runBotIO (BotToken token) defaultIntents
    $ do
      info @Text "Setting up commands and handlers..."

      react @'MessageCreateEvt $ \(msg, usr, _member) -> do

        let isBot = usr ^? (_Just . #bot . _Just) == Just True
            isCommand = T.isPrefixOf prefix $ msg ^. #content

        when (not isBot && not isCommand) $ do
          -- TODO: get the channel's note
          -- TODO: queue msg
          Trace.trace $ T.unpack $ msg ^. #content
          void . invoke $ CreateReaction msg msg (UnicodeEmoji "⌛")

      react @'MessageUpdateEvt $ \(_oldMsg, newMsg, usr, _member) -> do
        let isBot = usr ^? (_Just . #bot . _Just) == Just True
            isCommand = T.isPrefixOf prefix $ newMsg ^. #content

        when (not isBot && not isCommand) $ do
          -- TODO: get the channel's note
          -- TODO: update queued up msg
          Trace.trace $ T.unpack $ newMsg ^. #content
          void . invoke $ CreateReaction newMsg newMsg (UnicodeEmoji "⌛")

      addCommands $ do
        helpCommand

        command @'[Text] "note" $ \ctx note -> do
          -- TODO: save note for channel
          -- TODO: update all queued up msgs with this new note (the ones that don't have one)
          void . tell @Text ctx $ "Got it, will save messages to " <> note

          void . invoke $
            CreateReaction (ctx ^. #channel) (ctx ^. #message) (UnicodeEmoji "✅")
