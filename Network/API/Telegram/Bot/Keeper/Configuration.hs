module Network.API.Telegram.Bot.Keeper.Configuration (Settings (..), settings) where

import "base" Data.Int (Int)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Control.Applicative (pure, (<*>))
import "base" Data.Maybe (Maybe (Nothing))
import "base" System.IO (IO)
import "base" Prelude (negate)
import "optparse-applicative" Options.Applicative
	(Parser, execParser, argument, auto, info, fullDesc, metavar, str)
import "telega" Network.API.Telegram.Bot (Token (Token))
import Network.API.Telegram.Bot.Object.Chat (Chat, ID (CHAT))
import "text" Data.Text (pack)

data Settings = Settings Token (ID Chat)

options :: Parser Settings
options = Settings <$> token <*> chat_id where

	token :: Parser Token
	token = Token . pack <$> argument str (metavar "TELEGRAM_TOKEN")

	chat_id :: Parser (ID Chat)
	chat_id = CHAT . negate <$> argument auto (metavar "CHAT_ID")

settings :: IO Settings
settings = execParser $ info options fullDesc
