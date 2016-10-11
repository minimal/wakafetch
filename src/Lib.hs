{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- https://github.com/sdiehl/protolude
-- import Protolude

module Lib
    ( someFunc, date, stuff, getLastBeeminderDate, getSafeWaka, getData, runMainT
    ) where

import qualified Control.Exception    as E
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans  (MonadIO (..))
import           Data.Aeson
import           Data.Aeson.Lens      (key, nth, _Number, _String)
import           Data.Bool            (bool)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid          ((<>))
import qualified Data.Scientific      as Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Time.Calendar   as Cal
import qualified Data.Time.Clock      as Clock
import           Data.Time.Format     (defaultTimeLocale, formatTime,
                                       iso8601DateFormat)
import           Data.Time.LocalTime
import           GHC.Generics
import           Network.HTTP.Client  (HttpException (StatusCodeException),
                                       Response)
import           Network.Wreq         (FormParam ((:=)))
import qualified Network.Wreq         as Wr
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

-- import    Data.Either.Combinators
--import Data.Either.Utils (maybeToEither)
-- import           Formatting          (int, sformat, (%))
-- import qualified Formatting.Time     as Ft (dayOfMonth, month, year)

newtype Token = Token ByteString

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
--- http://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers?rq=1
getSafeWaka :: Token
            -> String
            -> IO (Either ByteString (Response LBS.ByteString))
getSafeWaka (Token token) date =
    (Right <$> Wr.getWith opts (summariesURL <> date <> "&end=" <> date)) `E.catch`
        handler
  where
    opts = Wr.defaults & Wr.auth ?~ Wr.basicAuth "" token
    handler (StatusCodeException s _ _) = return $ Left (s ^. Wr.statusMessage)


data MyError
    = StatusError Int
    | KeyError Text
    | NoChange Text
    deriving (Show)

-- guardedAction :: (MonadIO m, MonadError String m) => IO a -> m a
-- guardedAction action = do
--  result <- liftIO $ E.tryIOError action
--  case result of
--    Left  e -> throwError (show e)
--    Right r -> return r


handleResp r =
    case r ^. Wr.responseStatus . Wr.statusCode of
        200 -> return r
        x -> throwError $ StatusError x

getData
    :: (MonadError MyError m, MonadIO m)
    => Token -> String -> m (Response LBS.ByteString)
getData (Token token) date = do
    let url = summariesURL <> date <> "&end=" <> date
        opts = defOpts & Wr.auth ?~ Wr.basicAuth "" token
    r <- liftIO $ Wr.getWith opts url
    handleResp r

defOpts :: Wr.Options
defOpts =
    Wr.defaults & Wr.checkStatus ?~
    \_ _ _ ->
         Nothing

getLastBeeminderDateSafe :: (MonadError MyError m, MonadIO m) => m Text
getLastBeeminderDateSafe = do
    r <- liftIO $ Wr.getWith defOpts beeminderUrl
    r <- handleResp r
    maybeToError
        (KeyError "Key not found")
        (r ^? Wr.responseBody . nth 0 . key "daystamp" . _String)

postBeeminderSafe :: (MonadError MyError m, MonadIO m) => Float -> Text -> m Int
postBeeminderSafe hours daystamp = do
    r <-
        liftIO $
        Wr.postWith
            defOpts
            beeminderUrl
            ["daystamp" := (daystamp :: Text), "value" := (hours :: Float)]
    r <- handleResp r
    return (r ^. Wr.responseStatus . Wr.statusCode)

runMainT :: IO ()
runMainT = do
    let token = atoken
    r <-
        runExceptT $
        do today <- liftIO date
           res <- getData token (show (yesterday today))
           hours <-
               maybeToError (KeyError "key fail") $
               secondsToHours <$> extractSeconds res
           lastDate <- getLastBeeminderDateSafe
           let yest = yesterday today
               yestDayStamp =
                   T.pack $ formatTime defaultTimeLocale "%Y%m%d" yest
           if yestDayStamp == lastDate
               then throwError $ NoChange "Dates same"
               else postBeeminderSafe
                        (Scientific.toRealFloat hours)
                        yestDayStamp
    print r


summariesURL = "https://wakatime.com/api/v1/users/current/summaries?start="

-- TODO: look at https://hackage.haskell.org/package/errors-2.1.2/docs/Control-Error-Util.html


-- TODO: make return either
getWakaSeconds :: Token
     -> String -> IO (Maybe Scientific.Scientific)
getWakaSeconds token date = do
    r2 <- getSafeWaka token date
    case r2 of
        Right _ -> return ()
        Left msg -> print msg
    let res =
            case r2 of
                (Right a) -> extractSeconds a
                (Left msg) -> Nothing
    return res

beeminderUrl = "https://www.beeminder.com/api/v1/users/minimal/goals/wakatime/datapoints.json?auth_token="

getLastBeeminderDate = do
    r <- Wr.get beeminderUrl
    return (r ^? Wr.responseBody . nth 0 . key "daystamp" . _String)


postBeeminder hours daystamp = do
    r <-
        Wr.post
            beeminderUrl
            ["daystamp" := (daystamp :: Text), "value" := (hours :: Float)]
    return (r ^. Wr.responseStatus . Wr.statusCode)



date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = fmap (Cal.toGregorian . Clock.utctDay) Clock.getCurrentTime

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

atoken = Token ""

-- maybeToEither = flip maybe Right . Left
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither leftValue = maybe (Left leftValue) Right

maybeToError :: (MonadError a m) => a -> Maybe b -> m b
maybeToError leftValue = maybe (throwError leftValue) return

liftEither :: (Monad m, MonadError a (Either a)) => Either a b -> ExceptT a m b
liftEither = either throwError return

yesterdayHours :: IO (Either ByteString Scientific.Scientific)
yesterdayHours = do
    today <- date
    res <- getSafeWaka atoken $ show (yesterday today)
    return $
        do r <- res
           seconds <- maybeToEither "key not found" $ extractSeconds r
           return $ secondsToHours seconds

shouldPost lastBeeminderDate today = do
  beedate <- lastBeeminderDate
  let yest = yesterday today
      yestDayStamp = T.pack $ formatTime defaultTimeLocale "%Y%m%d" yest
      in if beedate /= yestDayStamp
            then Just yestDayStamp
            else Nothing

someFunc :: IO ()
someFunc = do
    -- [prefixarg] <- getArgs
    let token = atoken
    today <- date
    seconds <- getWakaSeconds token $ show (yesterday today)
    now <- Clock.getCurrentTime
    let hours =
            case seconds of
                Just a -> secondsToHours a
                Nothing -> 0.0
    print hours
    lastBeeminderDate <- getLastBeeminderDate
    print "last bee"
    print lastBeeminderDate
    let yest = yesterday today
        yestDayStamp = T.pack $ formatTime defaultTimeLocale "%Y%m%d" yest
        doPost =
            case lastBeeminderDate of
                Just a -> a /= yestDayStamp
                Nothing -> False
    print yestDayStamp
    resCode <-
        if doPost
            then postBeeminder (Scientific.toRealFloat hours) yestDayStamp
            else return 0
    printf "done %i\n" resCode -- then check beeminder doesn't have an entry for this day, then post

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

stuff :: IO ()
stuff = do
    now <- Clock.getCurrentTime

    today <- date
    let yest = yesterday today
    print yest
    print $ formatTime defaultTimeLocale "%Y%m%d" yest
    -- r <- postBeeminder 0.1 "20160928"
    -- print r
    (year,month,day) <- date
    now <- Clock.getCurrentTime
    print 5
    --let yesterday = Cal.addDays (-1) (Cal.fromGregorian year month day)
    --print $ iso8601 (Clock.utctDay yesterday)

brexit :: a
brexit = brexit
