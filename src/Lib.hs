{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- https://github.com/sdiehl/protolude
-- import Protolude

module Lib
    ( getWaka, runMain
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans   (MonadIO (..))
import           Data.Aeson.Lens       (key, nth, _Number, _String)
import           Data.Bool             (bool)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as LBS
import           Data.Monoid           ((<>))
import qualified Data.Scientific       as Scientific
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Time.Calendar    as Cal
import qualified Data.Time.Clock       as Clock
import           Data.Time.Format      (defaultTimeLocale, formatTime,
                                        iso8601DateFormat)
import           Data.Time.LocalTime
import           Network.HTTP.Client   (HttpException (StatusCodeException),
                                        Response)
import           Network.Wreq          (FormParam ((:=)))
import qualified Network.Wreq          as Wr
import           System.Environment    (getArgs)
import           Text.Printf           (printf)

-- import    Data.Either.Combinators
-- import Data.Either.Utils (maybeToEither)
-- import           Formatting          (int, sformat, (%))
-- import qualified Formatting.Time     as Ft (dayOfMonth, month, year)

-- TODO: get tokens etc from commandline/env vars. use optparse
-- TODO: paramaterise user names, goals etc
-- TODO: tests
-- TODO: more newtypes instead of strings, hours etc
-- TODO: range of days at once

newtype Token =
    Token ByteString
    deriving (Show,Eq)

data Settings = Settings
    { _wakaToken :: Token
    , _beeToken :: Token
    } deriving (Eq,Show)

-- iso8601 :: Clock.UTCTime -> String
-- iso8601 = formatTime defaultTimeLocale "%F"

extractSeconds :: Response LBS.ByteString -> Maybe Scientific.Scientific
extractSeconds r = r ^? Wr.responseBody
                 . key "data"
                 . nth 0
                 . key "grand_total"
                 . key "total_seconds"
                 . _Number

-- http://stackoverflow.com/questions/34588488/lift-either-to-exceptt-automatically
-- http://stackoverflow.com/questions/26357269/should-i-use-either-monad-or-errort-monad-trasformer?rq=1
-- http://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers?rq=1

data MyError
    = StatusError Int
                  ByteString
    | KeyError Text
    | NoChange Text
    deriving (Show)


handleResp
    :: (MonadError MyError m)
    => Response body -> m (Response body)
handleResp r =
    case () of
        _
          | (sc >= 200) && (sc < 300) -> return r
          | otherwise -> throwError $ StatusError sc (r ^. Wr.responseStatus . Wr.statusMessage)
            where sc = r ^. Wr.responseStatus . Wr.statusCode

getWaka
    :: (MonadError MyError m, MonadIO m)
    => Token -> String -> m (Response LBS.ByteString)
getWaka (Token token) date = do
    -- settings <- ask
    let url = summariesURL <> date <> "&end=" <> date
        opts = defOpts & Wr.auth ?~ Wr.basicAuth "" token
    r <- liftIO $ Wr.getWith opts url
    handleResp r

defOpts :: Wr.Options
defOpts =
    Wr.defaults & Wr.checkStatus ?~
    \_ _ _ ->
         Nothing

getLastBeeminderDateSafe :: (MonadError MyError m, MonadIO m) => Token ->  m Text
getLastBeeminderDateSafe (Token token) = do
    r <-
        liftIO $ Wr.getWith defOpts $
        C.unpack (beeminderUrl <> token)
    res <- handleResp r
    maybeToError
        (KeyError "Key not found")
        (res ^? Wr.responseBody . nth 0 . key "daystamp" . _String)

data BeePost =
    BeePost Float
            Text
            Int
    deriving (Show)

postBeeminderSafe
    :: (MonadError MyError m, MonadIO m)
    => Token -> Float -> Text -> m BeePost
postBeeminderSafe (Token token) hours daystamp = do
    r <-
        liftIO $
        Wr.postWith
            defOpts
            (C.unpack (beeminderUrl <> token))
            ["daystamp" := (daystamp :: Text), "value" := (hours :: Float)]
    res <- handleResp r
    return $ BeePost hours daystamp (res ^. Wr.responseStatus . Wr.statusCode)

printLift x = liftIO $ print x

-- liftReaderT :: m a -> ReaderT r m a
-- liftReaderT m = ReaderT (\_ -> m)

doApp :: MyApp BeePost
doApp = do
    Settings wakaToken beeToken <- ask
    today <- liftIO theDate
    res <- getWaka wakaToken (show (yesterday today))
    hours <-
        maybeToError (KeyError "key fail") $
        secondsToHours <$> extractSeconds res
    hours <-
        extractSeconds res <&> secondsToHours &
        maybeToError (KeyError "key fail")
    lastDate <- getLastBeeminderDateSafe beeToken
    let yestDayStamp =
            T.pack $ formatTime defaultTimeLocale "%Y%m%d" (yesterday today)
        postIf
          | lastDate /= yestDayStamp =
              postBeeminderSafe
                  beeToken
                  (Scientific.toRealFloat hours)
                  yestDayStamp
          | otherwise = throwError $ NoChange "Dates same"
    postIf

newtype MyApp a = Mya
    { runA :: ReaderT Settings (ExceptT MyError IO) a
    } deriving (Functor,Applicative,Monad,MonadIO,MonadError MyError,MonadReader Settings)

runMyApp :: MyApp a -> Settings -> IO (Either MyError a)
runMyApp k settings = runExceptT (runReaderT (runA k) settings)

runMain :: IO (Either MyError BeePost)
runMain = runMyApp doApp (Settings wakaToken beeToken)


summariesURL = "https://wakatime.com/api/v1/users/current/summaries?start="

-- TODO: look at https://hackage.haskell.org/package/errors-2.1.2/docs/Control-Error-Util.html

beeminderUrl :: ByteString
beeminderUrl = "https://www.beeminder.com/api/v1/users/minimal/goals/wakatime/datapoints.json?auth_token="


theDate :: IO (Integer,Int,Int) -- :: (year,month,day)
theDate = fmap (Cal.toGregorian . Clock.utctDay) Clock.getCurrentTime

yesterday :: (Integer, Int, Int) -> Cal.Day
yesterday today =
    let (year,month,day) = today
    in Cal.addDays (-1) (Cal.fromGregorian year month day)

-- dayToBeeString :: (Integer, Int, Int) -> String
-- dayToBeeString (year, month, day) = printf "%i%i%i" year month day

-- > now <- getCurrentTime
-- > later <- getCurrentTime
-- > format (dayOfMonth % "/" % month % "/" % year) now now now
-- doesn’t put 09 in month


-- {username: "",
-- auth_token: ""}

wakaToken = Token ""
beeToken = Token ""

-- maybeToEither = flip maybe Right . Left
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither leftValue = maybe (Left leftValue) Right

maybeToError :: (MonadError a m) => a -> Maybe b -> m b
maybeToError leftValue = maybe (throwError leftValue) return

liftEither :: (Monad m) => Either a b -> ExceptT a m b
liftEither = either throwError return

-- WARNING: Although Scientific is an instance of Fractional, the
-- methods are only partially defined! Specifically recip and / will
-- diverge (i.e. loop and consume all space) when their outputs have
-- an infinite decimal expansion.
-- fromRationalRepetend Nothing (1 / 28)
-- > Right (3.571428e-2,Just 2)

secondsToHours :: Fractional a => a -> a
secondsToHours = (*) 2.777777777777778e-4

-- For convenience, lens gives us two variants on view/(^.) which
-- pre-wrap our subparts in useful monoids. We have preview/(^?) which
-- prewraps the subparts in First and toListOf/(^..) which prewraps
-- the subparts in [].

-- > "foo" ^? each
-- Just 'f'

-- > "foo" ^.. each
-- "foo"

-- print (r ^. responseBody . key "url" . _String)
-- print (r ^. responseBody . key "grand_total" . _String)


brexit :: a
brexit = brexit
