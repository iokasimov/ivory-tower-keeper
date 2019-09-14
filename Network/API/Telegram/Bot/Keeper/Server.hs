{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.API.Telegram.Bot.Keeper.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Bool ((||))
import "base" Data.Eq ((/=))
import "base" Data.Foldable (for_)
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "lens" Control.Lens ((^.))
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.API.Telegram.Bot (Telegram, Token (Token), telegram)
import "telega" Network.API.Telegram.Bot.Object (Callback (Datatext), Origin (Group), Content (Command))
import "telega" Network.API.Telegram.Bot.Object.Chat (Chat)
import "telega" Network.API.Telegram.Bot.Object.Member (Until (Forever), Kick (Kick))
import "telega" Network.API.Telegram.Bot.Object.Update (Update (Membership))
import "telega" Network.API.Telegram.Bot.Object.Update.Message (Message (Direct), Delete (Delete))
import "telega" Network.API.Telegram.Bot.Object.Update.Moving (Moving (Joined))
import "telega" Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (ident), Persistable (persist, persist_), ID)

import Network.API.Telegram.Bot.Keeper.Configuration (Settings (Settings))

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server (Settings token chat_id) secret update =
	if secret /= token || update ^. access /= chat_id then throwError err403 else
		liftIO . void . async . telegram token chat_id $ webhook update

webhook :: Update -> Telegram (ID Chat) ()
webhook (Membership _ (Joined ps chat_id _)) = for_ ps $
	\p -> persist_ $ Kick chat_id (ident p) Forever
webhook _ = pure ()
