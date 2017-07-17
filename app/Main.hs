{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Word8
import qualified Options.Applicative as OA
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.List as DCL
import Data.Conduit.Binary as DCB hiding (mapM_, takeWhile)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Text.Blaze.Svg
import Text.Blaze.Svg11
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Utf8

data Config = Config String
  deriving (Show, Eq)

opts :: OA.Parser Config
opts = Config
  <$> OA.strArgument (OA.metavar "FILE" <> OA.help "strace output file")

optsInfo :: OA.ParserInfo Config
optsInfo = OA.info opts $ OA.fullDesc
  <> OA.footer "Use `strace -tt -T -o <LOG FILE>` to generate the log file"

data STraceLine = STraceLine
  { stlStartTimeStr  :: B.ByteString
  , stlStartTimeUSec :: Integer
  , stlCall          :: B.ByteString
  , stlDetails       :: B.ByteString
  , stlDurationStr   :: B.ByteString
  , stlDurationUSec  :: Integer
  } deriving (Show, Eq)

startTimeParser :: AP.Parser Integer
startTimeParser = hhmmssuu
  <$> APC.decimal                               <* AP.word8 _colon
  <*> APC.decimal                               <* AP.word8 _colon
  <*> APC.decimal                               <* AP.word8 _period
  <*> APC.decimal                               <* APC.endOfInput
  where hhmmssuu hh mm ss uu = hh * 60 * 60 * 1000000
                                  + mm * 60 * 1000000
                                       + ss * 1000000
                                            + uu

durationParser :: AP.Parser Integer
durationParser = do
  AP.word8 _less
  ssuu
    <$> APC.decimal <* AP.word8 _period
    <*> APC.decimal <* AP.word8 _greater <* APC.endOfInput
  where ssuu ss uu = ss * 1000000 + uu

interpretLine :: B.ByteString -> Maybe STraceLine
interpretLine line = let
  (startTime, afterStartTime) = B.span    (/= _space)                line
  (call,      afterCall)      = B.span    (/= _parenleft) $ B.drop 1 afterStartTime
  (details,   duration)       = B.spanEnd (/= _space)                afterCall
  maybeFromEither = either (const Nothing) Just
  maybeParse p = maybeFromEither . AP.parseOnly p

  in do
    startTimeUSecs <- maybeParse startTimeParser startTime
    durationUSecs  <- maybeParse durationParser  duration
    return $ STraceLine
      { stlStartTimeStr  = startTime
      , stlStartTimeUSec = startTimeUSecs
      , stlCall          = call
      , stlDetails       = details
      , stlDurationStr   = duration
      , stlDurationUSec  = durationUSecs
      }

offsetStartTimes :: Monad m => Conduit STraceLine m STraceLine
offsetStartTimes = await >>= \case
  Nothing   -> return ()
  Just stl0 -> let applyOffset stl = stl { stlStartTimeUSec = stlStartTimeUSec stl 
                                                            - stlStartTimeUSec stl0 }
               in do
    yield $ applyOffset stl0
    DCL.map applyOffset

within :: Monad m => String -> String -> Conduit a m B.ByteString -> Conduit a m B.ByteString
within before after inner = do
  yield $ toBS before
  inner
  yield $ toBS after
  where toBS = T.encodeUtf8 . T.pack

toSVG :: Monad m => Conduit STraceLine m B.ByteString
toSVG = within "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"><svg width=\"1300\" height=\"8000\" xmlns=\"http://www.w3.org/2000/svg\"><style>rect{fill:red;}</style>"
               "</svg>" $ do

  yield $ T.encodeUtf8 $ T.concat
    [ "<style>"
    , "line.tick{stroke:#808080;stroke-width:0.8px}"
    , "rect{fill:#808080;}"
    , ".clock_gettime rect{fill:#000080;}"
    , ".close         rect{fill:#000000;}"
    , ".epoll_ctl     rect{fill:#00c000;}"
    , ".epoll_wait    rect{fill:#008000;}"
    , ".fsync         rect{fill:#ff8080;}"
    , ".write         rect{fill:#ff0000;}"
    , ".sendfile      rect{fill:#ff8000;}"
    , ".splice        rect{fill:#80ff00;}"
    , "</style>"
    ]

  let vizWidth    = 1200  :: Double
      vizHeight   = 8000  :: Double
      usPerLine   = 50000 :: Double
      lineSpacing = 10    :: Double
      lineHeight  = 7     :: Double
      oneLineCoord us = fromIntegral us / usPerLine * vizWidth
      multiLineCoord c = let
        line  = floor c `div` floor vizWidth
        coord = c - vizWidth * fromIntegral line
        in (line, coord)
      centrePixel c = fromIntegral (floor c) + 0.5 :: Double

  mapM_ yield $ BL.toChunks $ renderSvg $ do
    let xTicks = takeWhile (<= vizWidth) $ map (centrePixel . oneLineCoord) $ iterate (+1000) 0
    forM_ xTicks $ \tick -> line ! A.x1 (toValue tick)
                                 ! A.x2 (toValue tick)
                                 ! A.y1 (toValue (0 :: Double))
                                 ! A.y2 (toValue vizHeight)
                                 ! A.class_ "tick"

    let yTicks = takeWhile (<= vizHeight) $ map (centrePixel . (* lineSpacing) . fromIntegral . fst . multiLineCoord . oneLineCoord)
                                          $ iterate (+1000000) 0
    forM_ yTicks $ \tick -> line ! A.x1 (toValue (0 :: Double))
                                 ! A.x2 (toValue (vizWidth + 100))
                                 ! A.y1 (toValue tick)
                                 ! A.y2 (toValue tick)
                                 ! A.class_ "tick"

  awaitForever $ \stl -> mapM_ yield $ BL.toChunks $ renderSvg $
    g ! A.class_ (toValue $ T.decodeUtf8 $ stlCall stl) $ do
      title (toMarkup $ T.decodeUtf8
          $  stlDurationStr stl
          <> B.singleton _space
          <> stlStartTimeStr stl
          <> B.singleton _space
          <> stlCall stl <> stlDetails stl)

      let oneLineStart    = oneLineCoord $ stlStartTimeUSec stl
          durationCoord   = oneLineCoord $ stlDurationUSec  stl


          (startLine, startCoord) = multiLineCoord   oneLineStart
          (endLine,   _)          = multiLineCoord $ oneLineStart + durationCoord

          callLines = takeWhile (<= endLine) $ iterate (+1) startLine

      forM_ callLines $ \thisLine -> let
        offset = fromIntegral (thisLine - startLine) * vizWidth
        clippedWidth = min durationCoord $ vizWidth - (startCoord - offset)
        in rect ! A.x     (toValue $ startCoord - offset)
                ! A.width (toValue clippedWidth)
                ! A.y      (toValue $ lineSpacing * fromIntegral thisLine + 2)
                ! A.height (toValue lineHeight)

main :: IO ()
main = do
  Config fileName <- OA.execParser optsInfo

  runResourceT $ runConduit
      $  sourceFile fileName
     =$= DCB.lines
     =$= DCL.mapMaybe interpretLine
     =$= offsetStartTimes
     =$= toSVG
     =$= sinkFile (fileName <> ".svg")
