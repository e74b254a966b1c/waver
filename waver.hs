{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString, empty)
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Binary
import Prelude hiding (take)

data SubchunkHead = SubchunkHead {subchunkId     :: ByteString,
                                  subchunkSize   :: Int} -- Word32
    deriving Show

data FmtBody = FmtBody {audioFormat     :: Int, -- Word16
                        numChannels     :: Int, -- Word16
                        sampleRate      :: Int, -- Word32
                        byteRate        :: Int, -- Word32
                        blockAlign      :: Int, -- Word16
                        bitsPerSample   :: Int, -- Word16
                        extraParams     :: ByteString}
    deriving Show

data UnknownBody = UnknownBody {bytes :: ByteString}
    deriving Show

data WAV = WAV {waveSize           :: Int, -- Word32
                fmt                :: (SubchunkHead, FmtBody),
                samples            :: (SubchunkHead, [[Int]]),
                unknownSubchunks   :: [(SubchunkHead, UnknownBody)]}
    deriving Show

iWord8 = fromIntegral <$> anyWord8
iWord16le = fromIntegral <$> anyWord16le
iWord32le = fromIntegral <$> anyWord32le
iWord64le = fromIntegral <$> anyWord64le

riffHead = string "RIFF" *> iWord32le <* string "WAVE"

subchunkHead cond = (\x -> if cond x then return x else fail "Unexpected subchunk header") =<<
                    SubchunkHead <$> (take 4) <*> iWord32le

genericSubchunkParser headerCondition bodyParser = subchunkHead headerCondition >>=
                                                   \header -> bodyParser header >>= return . (,) header

subchunkParser id =  genericSubchunkParser $ (id ==) . subchunkId

unknownSubchunk saveBody = genericSubchunkParser isUnknownSubchunk unknownBody
    where isUnknownSubchunk = not . (flip elem) knownSubchunks . subchunkId
          knownSubchunks = ["fmt ", "data"]
          unknownBody header
              | saveBody = UnknownBody <$> (take $ subchunkSize header)
              | otherwise = UnknownBody empty <$ (take $ subchunkSize header)

fmtSubchunk = subchunkParser "fmt " fmtBody
    where fmtBody header = FmtBody <$>
                           iWord16le <*>       -- audioFormat
                           iWord16le <*>       -- numChannels
                           iWord32le <*>       -- sampleRate
                           iWord32le <*>       -- byteRate
                           iWord16le <*>       -- blockAlign
                           iWord16le <*>       -- bitsPerSample
                           extraBytes header   -- extraParams
          extraBytes h = take $ subchunkSize h - 16

pcmBlock (FmtBody audioFormat numChannels _ _ blockAlign bitsPerSample _) totalBytes
    | audioFormat /= 1 = fail "Audio format is not PCM."
    | bitsPerSample > 64 = fail "Bits per sample > maximum supported of 64."
    | totalBytes `mod` bytesPerBlock /= 0 = fail "Number of bytes in data subchunk not divisible by block size."
    | otherwise = (count numChannels sample <* count paddingBytes (word8 0), totalBytes `div` bytesPerBlock)
    where bytesPerSample = (bitsPerSample + 7) `div` 8
          bytesUsedPerBlock = bytesPerSample * numChannels
          paddingBytes = (-bytesUsedPerBlock) `mod` blockAlign
          bytesPerBlock = bytesUsedPerBlock + paddingBytes
          sample = case bytesPerSample of
                       1 -> iWord8
                       2 -> iWord16le
                       4 -> iWord32le
                       8 -> iWord64le
                       _ -> fail "PCM bits per sample not supported"

parseData (parser, num) input
    | num > 0 = case parse parser input of
                    Fail _ _ y -> error y
                    Done input' x -> x : parseData (parser, num - 1) input'
    | otherwise = []

parseWave saveUnknown input =
    case parse ((,,,,) <$> riffHead <*> unknowns <*> fmtSubchunk <*> unknowns <*> subchunkHead ((== "data") . subchunkId)) input of
        Fail _ _ y -> error y
        Done input' x -> buildWave x $ parseData (blockParser x) input'
    where unknowns = many' $ unknownSubchunk saveUnknown
          blockParser (_, _, fmt, _, h) = pcmBlock (snd fmt) (subchunkSize h)
          buildWave (r, u1, f, u2, h) pcms = WAV r f (h, pcms) $ u1 ++ u2

checkSizes (WAV size fmt samples u) numBytes
    | size + 8 /= numBytes = (False, "bytes declared in riff header = " ++ (show $ size + 8) ++ " bytes in input = " ++ (show numBytes))
    | subchunksLen /= size = (False, "bytes in subchunks = " ++ (show subchunksLen) ++ " bytes declared in riff header = " ++ (show numBytes))
    | otherwise = (True, "")
    where len = (8 +) . subchunkSize . fst
          subchunksLen = (len fmt) + (len samples) + (foldl (\a b -> a + (len b)) 0 u)



main = do
    contents <- BL.getContents
    let result = parseWave False contents
    putStrLn $ show (checkSizes result $ fromIntegral $ BL.length contents)
    putStrLn $ show $ unknownSubchunks result
