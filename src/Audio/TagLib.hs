{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Audio.TagLib
  ( module Audio.TagLib
  , TagLib (..)
  , FileId ()
  , io
  ) where

import Control.Applicative
import Foreign.C.String (CString,withCString,peekCString)
import Foreign.C.Types (CInt(..),CChar(..))
import Foreign.Ptr (Ptr,nullPtr)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Audio.TagLib.Internal

-- Main Interface {{{

-- | Run a @TagLib@ block. Save and free any files
--   left open when the block is finished, and free
--   all strings produced by taglib.
taglib :: TagLib a -> IO a
taglib m = do
  (res,fs) <- evalTagLib initialEnv m'
  mapM_ cleanupFile fs
  c_taglib_free_strings
  return res
  where
  m' = do
    res <- m 
    fs <- openFilePtrs
    return (res,fs)

-- | Open a file and return a corresponding @FileId@.
--   Internally, this grabs the Tag and AudioProperties
--   pointers to the TagLib_File.
openFile :: FilePath -> TagLib FileId
openFile fp = do
  f <- io $ withCString fp $ \c_path -> do
    c_file <- c_taglib_file_new c_path
    if (c_file == nullPtr)
    then E.throw (UnableToOpen fp) 
    else do
      res <- c_taglib_file_is_valid c_file
      if (res == 0)
      then E.throw (InvalidFile fp)
      else TagLibFile c_file        <$>
           c_taglib_file_tag c_file <*>
           c_taglib_file_audioproperties c_file
  fid <- nextId
  addNewFile fid f
  return fid

-- }}}

-- FFI Wrappers {{{

-- | Given a @IO@ action which expects a @Tag@ pointer and @CString@,
--   lifts it into an @TagLib@ action, expecting @Text@.
packStringTag :: SetStringTag -> FileId -> T.Text -> TagLib ()
packStringTag k fid txt = do
  c_tag <- fromFile tagPtr fid
  io $ BS.useAsCString bs $ k c_tag
  where
  bs :: BS.ByteString
  bs = T.encodeUtf8 txt

-- | Given a @IO@ action which expects a @Tag@ pointer and @CInt@,
--   lifts it into an @TagLib@ action, expecting a @Int@.
packIntTag :: SetIntTag -> FileId -> Int -> TagLib ()
packIntTag k fid int = do
  c_tag <- fromFile tagPtr fid
  io $ k c_tag $ toEnum int

-- | Given a @IO@ action which expects a @Tag@ pointer and
--   results in a @CString@, lifts it into a @TagLib@ action,
--   resulting in @Text@.
unpackStringTag :: GetStringTag -> FileId -> TagLib T.Text
unpackStringTag k fid = do
  c_tag <- fromFile tagPtr fid
  io $ do
    c_str <- k c_tag
    T.pack <$> peekCString c_str

-- | Given a @IO@ action which expects a @Tag@ pointer and
--   results in a @CInt@, lifts it into a @TagLib@ action,
--   resulting in @Int@.
unpackIntTag :: GetIntTag -> FileId -> TagLib Int
unpackIntTag k fid = do
  c_tag <- fromFile tagPtr fid
  io $ fromIntegral <$> k c_tag

-- | Given a @IO@ action which expects a @AudioProperties@ pointer and
--   results in a @CInt@, lifts it into a @TagLib@ action,
--   resulting in @Int@.
unpackIntAP :: GetIntAP -> FileId -> TagLib Int
unpackIntAP k fid = do
  c_ap <- fromFile audioPropPtr fid
  io $ fromIntegral <$> k c_ap

-- }}}

-- FFI Types {{{

-- | FFI Type Synonyms
type SetStringTag = Ptr Tag -> CString -> IO ()
type SetIntTag = Ptr Tag -> CInt -> IO ()
type GetStringTag = Ptr Tag -> IO (Ptr CChar)
type GetIntTag = Ptr Tag -> IO CInt
type GetIntAP = Ptr AudioProperties -> IO CInt

-- }}}

-- Tag Setters {{{

-- | Set the track title.
setTitle :: FileId ->  T.Text -> TagLib ()
setTitle = packStringTag c_taglib_tag_set_title

-- | Set the artist name.
setArtist :: FileId ->  T.Text -> TagLib ()
setArtist = packStringTag c_taglib_tag_set_artist

-- | Set the album name.
setAlbum :: FileId ->  T.Text -> TagLib ()
setAlbum = packStringTag c_taglib_tag_set_album

-- | Set the comment field.
setComment :: FileId ->  T.Text -> TagLib ()
setComment = packStringTag c_taglib_tag_set_comment

-- | Set the genre field.
setGenre :: FileId ->  T.Text -> TagLib ()
setGenre = packStringTag c_taglib_tag_set_genre

-- | Set the release year.
setYear :: FileId ->  Int -> TagLib ()
setYear = packIntTag c_taglib_tag_set_year

-- | Set the track number.
setTrack :: FileId ->  Int -> TagLib ()
setTrack = packIntTag c_taglib_tag_set_track


foreign import ccall "taglib_tag_set_title"
  c_taglib_tag_set_title :: SetStringTag

foreign import ccall "taglib_tag_set_artist"
  c_taglib_tag_set_artist :: SetStringTag

foreign import ccall "taglib_tag_set_album"
  c_taglib_tag_set_album :: SetStringTag

foreign import ccall "taglib_tag_set_comment"
  c_taglib_tag_set_comment :: SetStringTag

foreign import ccall "taglib_tag_set_genre"
  c_taglib_tag_set_genre :: SetStringTag

foreign import ccall "taglib_tag_set_year"
  c_taglib_tag_set_year :: SetIntTag

foreign import ccall "taglib_tag_set_track"
  c_taglib_tag_set_track :: SetIntTag

-- }}}

-- Tag Getters {{{

-- | Get the track title.
getTitle :: FileId -> TagLib T.Text
getTitle  = unpackStringTag c_taglib_tag_title

-- | Get the artist name.
getArtist :: FileId -> TagLib T.Text
getArtist  = unpackStringTag c_taglib_tag_artist

-- | Get the album name.
getAlbum :: FileId -> TagLib T.Text
getAlbum  = unpackStringTag c_taglib_tag_album

-- | Get the contents of the comment field.
getComment :: FileId -> TagLib T.Text
getComment  = unpackStringTag c_taglib_tag_comment

-- | Get the contents of the genre field.
getGenre :: FileId -> TagLib T.Text
getGenre  = unpackStringTag c_taglib_tag_genre

-- | Get the release year.
getYear :: FileId -> TagLib Int
getYear  = unpackIntTag c_taglib_tag_year

-- | Get the track number.
getTrack :: FileId -> TagLib Int
getTrack  = unpackIntTag c_taglib_tag_track


foreign import ccall "taglib_tag_title"
  c_taglib_tag_title :: GetStringTag

foreign import ccall "taglib_tag_artist"
  c_taglib_tag_artist :: GetStringTag

foreign import ccall "taglib_tag_album"
  c_taglib_tag_album :: GetStringTag

foreign import ccall "taglib_tag_comment"
  c_taglib_tag_comment :: GetStringTag

foreign import ccall "taglib_tag_genre"
  c_taglib_tag_genre :: GetStringTag

foreign import ccall "taglib_tag_year"
  c_taglib_tag_year :: GetIntTag

foreign import ccall "taglib_tag_track"
  c_taglib_tag_track :: GetIntTag

-- }}}

-- AudioProperties Getters {{{

-- | Retrieves the duration of the given file, in seconds.
getLength :: FileId -> TagLib Int
getLength = unpackIntAP c_taglib_audioproperties_length

-- | Retrieves the bitrate of the given file, in kb/s.
getBitrate :: FileId -> TagLib Int
getBitrate = unpackIntAP c_taglib_audioproperties_bitrate

-- | Retrieves the sample rate of the given file, in Hz.
getSampleRate :: FileId -> TagLib Int
getSampleRate = unpackIntAP c_taglib_audioproperties_samplerate

-- | Retrieves the number of channels in the given file.
getChannels :: FileId -> TagLib Int
getChannels = unpackIntAP c_taglib_audioproperties_channels


foreign import ccall "taglib_audioproperties_length"
  c_taglib_audioproperties_length :: GetIntAP

foreign import ccall "taglib_audioproperties_bitrate"
  c_taglib_audioproperties_bitrate :: GetIntAP

foreign import ccall "taglib_audioproperties_samplerate"
  c_taglib_audioproperties_samplerate :: GetIntAP

foreign import ccall "taglib_audioproperties_channels"
  c_taglib_audioproperties_channels :: GetIntAP

-- }}}

