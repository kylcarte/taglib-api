{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}


module Audio.TagLib.API
  ( withMetadata
  , Tag()
  , AudioProperties()
  , TagLib (..)
  , TLEnv (..)
  , getTitle   , setTitle  
  , getArtist  , setArtist 
  , getAlbum   , setAlbum  
  , getComment , setComment
  , getGenre   , setGenre  
  , getYear    , setYear   
  , getTrack   , setTrack  
  , getLength
  , getBitrate
  , getSampleRate
  , getChannels
  , io
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Word (Word8)
import Foreign.C.String (CString,withCString)
import Foreign.C.Types (CInt(..),CChar(..))
import Foreign.Marshal.Array (lengthArray0,copyArray)
import Foreign.Ptr (Ptr,nullPtr)
import qualified Control.Exception as E
import qualified Data.ByteString as SI
import qualified Data.ByteString.Internal as SI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- Base --------------------------------------------------------------------{{{1

-- | Process the Tag pointer for a file.
withMetadata :: FilePath -> TagLib a -> IO (Maybe a)
withMetadata path m =
  withCString path                             $ \ c_path ->
  E.bracket (c_taglib_file_new c_path) cleanup $ \ c_file ->
    whenMaybe (c_file /= nullPtr) $ do
      res <- c_taglib_file_is_valid c_file
      whenMaybe (res /= 0) $ do
        env <- TLEnv               <$>
          c_taglib_file_tag c_file <*>
          c_taglib_file_audioproperties c_file
        Just <$> eval m env
        
  where
  eval :: TagLib a -> TLEnv -> IO a
  eval = runReaderT . runTagLib
  cleanup c_file = do
    c_taglib_file_save c_file
    c_taglib_free_strings
    c_taglib_file_free c_file

whenMaybe :: (Monad m) => Bool -> m (Maybe a) -> m (Maybe a)
whenMaybe b m = if b
  then m
  else return Nothing

-- | Abstract Tag object.
data Tag
type TagP = Ptr Tag
type SetString_Tag = CString -> TagP -> IO ()
type SetInt_Tag = CInt -> TagP -> IO ()
type GetString_Tag = TagP -> IO (Ptr Word8)
type GetInt_Tag = TagP -> IO CInt

data AudioProperties
type APP = Ptr AudioProperties
type GetInt_AP = APP -> IO CInt

-- Files -------------------------------------------------------------------{{{1

data TagLib_File

foreign import ccall "taglib_file_new"
  c_taglib_file_new :: CString -> IO (Ptr TagLib_File)

foreign import ccall "taglib_file_free"
  c_taglib_file_free :: Ptr TagLib_File -> IO ()

foreign import ccall "taglib_file_save"
  c_taglib_file_save :: Ptr TagLib_File -> IO ()

foreign import ccall "taglib_file_is_valid"
  c_taglib_file_is_valid :: Ptr TagLib_File -> IO CInt

foreign import ccall "taglib_file_tag"
  c_taglib_file_tag :: Ptr TagLib_File -> IO (TagP)

foreign import ccall "taglib_file_audioproperties"
  c_taglib_file_audioproperties :: Ptr TagLib_File -> IO (APP)

foreign import ccall "taglib_tag_free_strings"
  c_taglib_free_strings :: IO ()

-- Monad -------------------------------------------------------------------{{{1

newtype TagLib a = TagLib { runTagLib :: ReaderT TLEnv IO a }
instance Functor TagLib where
  fmap f (TagLib m) = TagLib $ f <$> m
instance Monad TagLib where
  return = TagLib . return
  (TagLib m) >>= f = TagLib $ m >>= runTagLib . f
instance Applicative TagLib where
  pure = return
  (<*>) = ap

io :: IO a -> TagLib a
io = TagLib . ReaderT . const

getTagPtr :: TagLib TagP
getTagPtr = TagLib $ asks tagPtr

getAPPtr :: TagLib APP
getAPPtr = TagLib $ asks apPtr

data TLEnv = TLEnv
  { tagPtr :: Ptr Tag
  , apPtr  :: Ptr AudioProperties
  }

-- FFI Wrappers ------------------------------------------------------------{{{1

packString_Tag :: SetString_Tag -> T.Text -> TagLib ()
packString_Tag k txt = do
  c_tag <- getTagPtr
  io $ SI.useAsCString bs $ flip k c_tag
  where
  bs = T.encodeUtf8 txt

packInt_Tag :: SetInt_Tag -> Int -> TagLib ()
packInt_Tag k int = do
  c_tag <- getTagPtr
  io $ k (toEnum int) c_tag

unpackString_Tag :: GetString_Tag -> TagLib T.Text
unpackString_Tag k = do
  c_tag <- getTagPtr
  io $ do
    c_str <- k c_tag
    len   <- lengthArray0 0 c_str
    T.decodeUtf8 <$> SI.create len (\ dst -> copyArray dst c_str len)

unpackInt_Tag :: GetInt_Tag -> TagLib Int
unpackInt_Tag k = do
  c_tag <- getTagPtr
  io $ fromIntegral <$> k c_tag

unpackInt_AP :: GetInt_AP -> TagLib Int
unpackInt_AP k = do
  c_ap <- getAPPtr
  io $ fromIntegral <$> k c_ap

-- Tag Setters -------------------------------------------------------------{{{1

setTitle :: T.Text -> TagLib ()
setTitle = packString_Tag c_taglib_tag_set_title

setArtist :: T.Text -> TagLib ()
setArtist = packString_Tag c_taglib_tag_set_artist

setAlbum :: T.Text -> TagLib ()
setAlbum = packString_Tag c_taglib_tag_set_album

setComment :: T.Text -> TagLib ()
setComment = packString_Tag c_taglib_tag_set_comment

setGenre :: T.Text -> TagLib ()
setGenre = packString_Tag c_taglib_tag_set_genre

setYear :: Int -> TagLib ()
setYear = packInt_Tag c_taglib_tag_set_year

setTrack :: Int -> TagLib ()
setTrack = packInt_Tag c_taglib_tag_set_track



foreign import ccall "taglib_tag_set_title"
  c_taglib_tag_set_title :: SetString_Tag

foreign import ccall "taglib_tag_set_artist"
  c_taglib_tag_set_artist :: SetString_Tag

foreign import ccall "taglib_tag_set_album"
  c_taglib_tag_set_album :: SetString_Tag

foreign import ccall "taglib_tag_set_comment"
  c_taglib_tag_set_comment :: SetString_Tag

foreign import ccall "taglib_tag_set_genre"
  c_taglib_tag_set_genre :: SetString_Tag

foreign import ccall "taglib_tag_set_year"
  c_taglib_tag_set_year :: SetInt_Tag

foreign import ccall "taglib_tag_set_track"
  c_taglib_tag_set_track :: SetInt_Tag

-- Tag Getters -------------------------------------------------------------{{{1

getTitle :: TagLib T.Text
getTitle  = unpackString_Tag c_taglib_tag_title

getArtist :: TagLib T.Text
getArtist  = unpackString_Tag c_taglib_tag_artist

getAlbum :: TagLib T.Text
getAlbum  = unpackString_Tag c_taglib_tag_album

getComment :: TagLib T.Text
getComment  = unpackString_Tag c_taglib_tag_comment

getGenre :: TagLib T.Text
getGenre  = unpackString_Tag c_taglib_tag_genre

getYear :: TagLib Int
getYear  = unpackInt_Tag c_taglib_tag_year

getTrack :: TagLib Int
getTrack  = unpackInt_Tag c_taglib_tag_track



foreign import ccall "taglib_tag_title"
  c_taglib_tag_title :: GetString_Tag

foreign import ccall "taglib_tag_artist"
  c_taglib_tag_artist :: GetString_Tag

foreign import ccall "taglib_tag_album"
  c_taglib_tag_album :: GetString_Tag

foreign import ccall "taglib_tag_comment"
  c_taglib_tag_comment :: GetString_Tag

foreign import ccall "taglib_tag_genre"
  c_taglib_tag_genre :: GetString_Tag

foreign import ccall "taglib_tag_year"
  c_taglib_tag_year :: GetInt_Tag

foreign import ccall "taglib_tag_track"
  c_taglib_tag_track :: GetInt_Tag

-- AudioProperties Getters -------------------------------------------------{{{1

getLength :: TagLib Int
getLength = unpackInt_AP c_taglib_audioproperties_length

getBitrate :: TagLib Int
getBitrate = unpackInt_AP c_taglib_audioproperties_bitrate

getSampleRate :: TagLib Int
getSampleRate = unpackInt_AP c_taglib_audioproperties_samplerate

getChannels :: TagLib Int
getChannels = unpackInt_AP c_taglib_audioproperties_channels



foreign import ccall "taglib_audioproperties_length"
  c_taglib_audioproperties_length :: GetInt_AP

foreign import ccall "taglib_audioproperties_bitrate"
  c_taglib_audioproperties_bitrate :: GetInt_AP

foreign import ccall "taglib_audioproperties_samplerate"
  c_taglib_audioproperties_samplerate :: GetInt_AP

foreign import ccall "taglib_audioproperties_channels"
  c_taglib_audioproperties_channels :: GetInt_AP

