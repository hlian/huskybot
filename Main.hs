{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Array.IO
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text.Format as F
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO
import           Data.Text.Lazy.Read
import           Data.Word8
import qualified Network.HTTP.Conduit as H
import           Network.HTTP.Types (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq hiding (params)
import           Prelude hiding (readFile, writeFile, putStrLn)
import           System.Environment (getArgs)
import           System.Random

newtype Channel = Channel Text deriving (Eq, Ord, Show)
newtype Emoji = Emoji Text
type Permutation a = IOArray Int a
data Notice = Notice Channel Text Emoji

instance ToJSON Notice where
  toJSON (Notice (Channel channel) text (Emoji emoji)) =
    object [ "channel" .= T.concat ["#", channel]
           , "icon_emoji" .= T.concat [":", emoji, ":"]
           , "parse" .= String "full"
           , "username" .= String "tacobot"
           , "text" .= text
           ]

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO (Permutation a)
shuffle xs = do
        ar <- copy xs
        shuffled <- forM [0..tops] $ \i -> do
            j <- randomRIO (i, tops)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
        copy shuffled
  where
    tops = length xs
    copy :: [a] -> IO (Permutation a)
    copy src = newListArray (0, tops) src

data DB = DB Int (Permutation Int)

(#<) :: B.ByteString -> Text
(#<) = decodeUtf8 . L.fromChunks . return

bingOf :: Int -> Text
bingOf page = F.format bingTemplate (F.Only page)
  where bingTemplate = "http://www.bing.com/images/async?q=siberian+husky+-fanpop&async=content&first={}&count=100&adlt=strict"
        -- Slack doesn't like fanpop images
        -- Turn on strict SafeSearch

urlsParser :: Parser [B.ByteString]
urlsParser = p
  where
    p = many' q
    q = takeTill (\c -> c == _i) *> (string "imgurl:&quot;" *> filename <|> (P.take 1 *> q))
    filename = B.append <$> (takeTill $ \c -> c == _j) <*> (string "jpg" <|> (P.take 1 *> filename))

urlAt :: Int -> Int -> IO Text
urlAt page word = do
  r <- get (T.unpack $ bingOf page)
  let body = r ^. responseBody
  handle $ parse urlsParser (B.concat . L.toChunks $ body)
  where
    handle (Fail i ctxs s) =
      error $ show (i, ctxs, s)

    handle (Partial f) =
      handle (f "")

    handle (Done _ r@(_:_)) = do     
      let word' = word `div` (length r)
      return $ (#<) (r !! word')

    handle (Done i []) =
      error ("nooooo: " ++ (T.unpack $ (#<) $ B.take 100 i))

postPayload :: Text -> Notice -> IO ()
postPayload token notice = do
  args <- getArgs
  case args == ["-n"] of
    True -> do
      putStrLn (T.concat ["+ Pretending (-n) to post ", (decodeUtf8 . encode) notice])
    False -> do
      response <- conduit_get request
      putStrLn (decodeUtf8 (H.responseBody response))
    where
      url = T.append "https://trello.slack.com/services/hooks/incoming-webhook?token=" token
      baseRequest = H.parseUrl (T.unpack url)
      request = fmap (H.urlEncodedBody [("payload", bytes)]) baseRequest
      conduit_get r = r >>= (H.withManager . H.httpLbs)
      bytes = (B.concat . L.toChunks . encode) notice

newtype User = User Text deriving (Eq, Ord)
data Person = Person User Channel

personOfRequest :: Request -> Either Text Person
personOfRequest raw = do
  channel <- channelOf <$> case p "channel_name" of
    Right "directmessage" -> Left ("Can't randomtaco in a direct message (yet)!")
    Right "privategroup" -> Left ("Can't randomtaco in a private group (yet)!")
    x -> x
  user <- userOf <$> p "user_name"
  return $ Person user channel

  where
    textOf = decodeUtf8 . L.fromChunks . return
    params = M.fromList [(textOf k, textOf <$> v) | (k, v) <- queryString raw]
    p key = case M.lookup key params of
      Just (Just "") -> Left (T.concat ["Empty key in params: ", key])
      Just (Just value) -> Right value
      _  -> Left (T.concat ["Unable to find key in params: ", key])
    userOf = User . T.filter (/= '@')
    channelOf = Channel

application :: MVar DB -> Application
application db req respond = do
  (start, lottery) <- MVar.modifyMVar db $ \(DB start_ x) -> do
    lottery_ <- readArray x (start_ `mod` 100000)
    writeFile "start" (F.format "{}" (F.Only start_ + 1))
    let atom = (start_, lottery_)
    return (DB (start_ + 1) x, atom)

  token_ <- readFile "token"
  let token = T.filter (/= '\n') token_

  putStrLn $ T.concat [
    "+ Incoming request: "
    , (#<)$ rawPathInfo req
    , (#<)$ rawQueryString req
    , " - "
    , (T.pack . show) start
    , " - "
    , (T.pack . show) lottery
    ]

  case personOfRequest req of
    Right (Person (User user) channel) -> do
      let (page, word) = (start `mod` 100 + 1, start `div` 100)
      url <- urlAt page word
      let response = F.format "@{} Here is your `/randomtaco`. {}" (user, url)
      postPayload token (Notice channel response (Emoji "dog"))

  respondWithEmpty

  where
    ourHeaders = [("Content-Type", "text/plain")]
    respondWithEmpty = (respond . responseLBS status200 ourHeaders) ""

main :: IO ()
main = do
  start_ <- readFile "start"
  let (Right (start, _)) = decimal $ T.filter (/= '\n') start_

  setStdGen (mkStdGen 1000)
  permutation <- shuffle [0..100000]

  db <- MVar.newMVar (DB start permutation)

  let port = 82
  putStrLn (F.format "+ Listening on port {}" (F.Only port))
  run port (application db)

  return ()
