{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, PatternGuards #-}


import           BasePrelude hiding (readFile, writeFile)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as M
import           Control.Lens ((^.))
import           Data.Array.IO (IOArray, readArray, writeArray, newListArray)
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.IO (readFile, writeFile)
import           Data.Text.Read (decimal)
import           Data.Word8
import           Network.Linklater
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq hiding (params)
import           System.Random (setStdGen, mkStdGen, randomRIO)

type Permutation a = IOArray Int a

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
(#<) = decodeUtf8

present :: (Show a) => a -> Text
present = T.pack . show

bingOf :: Int -> Text
bingOf page =
  -- Slack doesn't like fanpop images
  -- Turn on strict SafeSearch
  "http://www.bing.com/images/async?q=siberian+husky+-fanpop&async=content&first=" <> present page <> "&count=100&adlt=strict"

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
  toText $ parse urlsParser (B.concat . L.toChunks $ body)
  where
    toText (Fail i ctxs s) =
      error $ show (i, ctxs, s)

    toText (Partial f) =
      toText (f "")

    toText (Done _ r@(_:_)) = do
      let word' = word `div` (length r)
      return $ (#<) (r !! word')

    toText (Done i []) =
      error ("nooooo: " ++ (T.unpack $ (#<) $ B.take 100 i))

husky' :: Config -> Channel -> User -> Int -> IO Text
husky' config channel user lottery = do
  let (page, word) = (lottery `mod` 100 + 1, lottery `div` 100)
  url <- urlAt page word
  _ <- (`say` config) $ FormattedMessage
    (EmojiIcon "dog")
    "huskybot"
    channel
    [FormatAt user, FormatLink url ("Husky #" <> present lottery)]
  return ""

husky :: MVar DB -> Config -> Command -> IO Text
husky db config command = case command of
  (Command "husky" user channel Nothing) -> do
    putStrLn (show command)
    lottery <- lotteryIO
    husky' config channel user lottery
    where
      lotteryIO =
        M.modifyMVar db $ \(DB start x) -> do
          lottery_ <- readArray x (start `mod` 100000)
          writeFile "start" (present $ start + 1)
          return (DB (start + 1) x, lottery_)
  (Command "husky" user channel (Just text)) -> case decimal text of
    Right (lottery, _) ->
      if lottery < 100000 then husky' config channel user lottery else tooHighIO
    Left _ ->
      defaultIO
  where
    defaultIO =
      return "Type `/husky` to receive your very own, completely unique Siberian Husky."
    tooHighIO =
      return "https://upload.wikimedia.org/wikipedia/commons/8/86/William_Howard_Taft.jpg"

main :: IO ()
main = do
  setStdGen (mkStdGen seed)
  permutation <- shuffle [0..100000]

  start_ <- readFile "start"
  config <- configIO
  let (Right (start, _)) = decimal $ T.filter (/= '\n') start_

  db <- M.newMVar (DB start permutation)
  putStrLn ("+ Listening on port " <> show port)
  run port (slashSimple $ husky db config)
  where
    configIO =
      (Config . T.filter (/= '\n')) <$> readFile "token"
    port = 3335
    seed = 1000
