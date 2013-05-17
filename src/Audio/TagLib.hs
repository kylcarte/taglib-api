{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}


module Audio.TagLib (
    Tag()
  , AudioProperties()
  , TagLib (..)
  , TLEnv (..)
  , withFiles  , withFile
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

import Control.Applicative (Applicative(..),(<$>),(<*>))
import Control.Monad ((>=>),ap)
import Control.Monad.Trans.Reader (ReaderT (..),asks)
import Data.Maybe (listToMaybe)
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

-- | Process a computation requiring a @Ptr Tag@ and @Ptr AudioProperties@
--   using a given file, producing one result for each @FilePath@ given.
withFiles :: [FilePath] -> TagLib a -> IO (Maybe [a])
withFiles paths m =
  withCStrings paths $ \ c_paths ->
    bracket c_paths  $ 
      buildEnv >=> evalTagLib m 

-- | Retrieve the @Tag@ and @AudioProperties@ pointers
--   from a @TagLibFile@ pointer.
buildEnv :: Ptr TagLibFile -> IO TLEnv
buildEnv c_file = TLEnv    <$>
  c_taglib_file_tag c_file <*>
  c_taglib_file_audioproperties c_file

-- | Process a computation for exactly one file, as per @withFiles@.
withFile :: FilePath -> TagLib a -> IO (Maybe a)
withFile path m = do
  res <- withFiles [path] m
  case res of
    Just r -> return $ listToMaybe r
    Nothing -> return Nothing

-- | Save any changes made to file, and free all associated memory.
cleanupFile :: Ptr TagLibFile -> IO ()
cleanupFile c_file  = do
  c_taglib_file_save c_file
  c_taglib_free_strings
  c_taglib_file_free c_file

-- | For all @CString@s, each representing a @FilePath@, and a computation
--   expecting a pointer to a file, run the computation on all files,
--   if and only if all strings are valid files which are correctly opened
--   by tag_c's @taglib_file_new@.
bracket :: [CString] -> (Ptr TagLibFile -> IO a) -> IO (Maybe [a])
bracket c_paths f = loop c_paths id
  where
  loop ps k = case ps of
    []    -> fmap Just $ mapM f $ k []
    p:ps' -> E.bracket (c_taglib_file_new p) cleanupFile $ \c_file ->
      whenMaybe (c_file /= nullPtr) $ do
        res <- c_taglib_file_is_valid c_file
        whenMaybe (res /= 0) $ loop ps' (k . (c_file:))

-- | Pluralized @withCString@.
withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss f = loop ss id
  where
  loop l k = case l of
    []   -> f $ k []
    s:l' -> withCString s $ \c_str -> loop l' (k . (c_str:))

-- | Simple helper. Continue with given computation upon a condition.
whenMaybe :: (Monad m) => Bool -> m (Maybe a) -> m (Maybe a)
whenMaybe b m = if b
  then m
  else return Nothing

-- | Abstract Tag object.
data Tag
type TagP = Ptr Tag
type SetStringTag = CString -> TagP -> IO ()
type SetIntTag = CInt -> TagP -> IO ()
type GetStringTag = TagP -> IO (Ptr Word8)
type GetIntTag = TagP -> IO CInt

data AudioProperties
type APP = Ptr AudioProperties
type GetIntAP = APP -> IO CInt

data TagLibFile

-- Files -------------------------------------------------------------------{{{1

foreign import ccall "taglib_file_new"
  c_taglib_file_new :: CString -> IO (Ptr TagLibFile)

foreign import ccall "taglib_file_free"
  c_taglib_file_free :: Ptr TagLibFile -> IO ()

foreign import ccall "taglib_file_save"
  c_taglib_file_save :: Ptr TagLibFile -> IO ()

foreign import ccall "taglib_file_is_valid"
  c_taglib_file_is_valid :: Ptr TagLibFile -> IO CInt

foreign import ccall "taglib_file_tag"
  c_taglib_file_tag :: Ptr TagLibFile -> IO TagP

foreign import ccall "taglib_file_audioproperties"
  c_taglib_file_audioproperties :: Ptr TagLibFile -> IO APP

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

evalTagLib :: TagLib a -> TLEnv -> IO a
evalTagLib = runReaderT . runTagLib

-- | lift an @IO@ action into @TagLib@.
io :: IO a -> TagLib a
io = TagLib . ReaderT . const

rdr :: ReaderT TLEnv IO a -> TagLib a
rdr = TagLib

-- | Environment type for @TagLib@.
data TLEnv = TLEnv
  { tagPtr :: Ptr Tag
  , apPtr  :: Ptr AudioProperties
  }

-- | Retrieve the current @Tag@ pointer.
getTagPtr :: TagLib TagP
getTagPtr = rdr $ asks tagPtr

-- | Retrieve the current @AudioProperties@ pointer.
getAPPtr :: TagLib APP
getAPPtr = rdr $ asks apPtr

-- FFI Wrappers ------------------------------------------------------------{{{1

-- | Given a @IO@ action which expects a @Tag@ pointer and @CString@,
--   lifts it into an @TagLib@ action, expecting @Text@.
packStringTag :: SetStringTag -> T.Text -> TagLib ()
packStringTag k txt = do
  c_tag <- getTagPtr
  io $ SI.useAsCString bs $ flip k c_tag
  where
  bs = T.encodeUtf8 txt

-- | Given a @IO@ action which expects a @Tag@ pointer and @CInt@,
--   lifts it into an @TagLib@ action, expecting a @Int@.
packIntTag :: SetIntTag -> Int -> TagLib ()
packIntTag k int = do
  c_tag <- getTagPtr
  io $ k (toEnum int) c_tag

-- | Given a @IO@ action which expects a @Tag@ pointer and
--   results in a @CString@, lifts it into a @TagLib@ action,
--   resulting in @Text@.
unpackStringTag :: GetStringTag -> TagLib T.Text
unpackStringTag k = do
  c_tag <- getTagPtr
  io $ do
    c_str <- k c_tag
    len   <- lengthArray0 0 c_str
    T.decodeUtf8 <$> SI.create len (\ dst -> copyArray dst c_str len)

-- | Given a @IO@ action which expects a @Tag@ pointer and
--   results in a @CInt@, lifts it into a @TagLib@ action,
--   resulting in @Int@.
unpackIntTag :: GetIntTag -> TagLib Int
unpackIntTag k = do
  c_tag <- getTagPtr
  io $ fromIntegral <$> k c_tag

-- | Given a @IO@ action which expects a @AudioProperties@ pointer and
--   results in a @CInt@, lifts it into a @TagLib@ action,
--   resulting in @Int@.
unpackIntAP :: GetIntAP -> TagLib Int
unpackIntAP k = do
  c_ap <- getAPPtr
  io $ fromIntegral <$> k c_ap

-- Tag Setters -------------------------------------------------------------{{{1

setTitle :: T.Text -> TagLib ()
setTitle = packStringTag c_taglib_tag_set_title

setArtist :: T.Text -> TagLib ()
setArtist = packStringTag c_taglib_tag_set_artist

setAlbum :: T.Text -> TagLib ()
setAlbum = packStringTag c_taglib_tag_set_album

setComment :: T.Text -> TagLib ()
setComment = packStringTag c_taglib_tag_set_comment

setGenre :: T.Text -> TagLib ()
setGenre = packStringTag c_taglib_tag_set_genre

setYear :: Int -> TagLib ()
setYear = packIntTag c_taglib_tag_set_year

setTrack :: Int -> TagLib ()
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

-- Tag Getters -------------------------------------------------------------{{{1

getTitle :: TagLib T.Text
getTitle  = unpackStringTag c_taglib_tag_title

getArtist :: TagLib T.Text
getArtist  = unpackStringTag c_taglib_tag_artist

getAlbum :: TagLib T.Text
getAlbum  = unpackStringTag c_taglib_tag_album

getComment :: TagLib T.Text
getComment  = unpackStringTag c_taglib_tag_comment

getGenre :: TagLib T.Text
getGenre  = unpackStringTag c_taglib_tag_genre

getYear :: TagLib Int
getYear  = unpackIntTag c_taglib_tag_year

getTrack :: TagLib Int
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

-- AudioProperties Getters -------------------------------------------------{{{1

-- | Retrieves the duration of the given file, in seconds.
getLength :: TagLib Int
getLength = unpackIntAP c_taglib_audioproperties_length

-- | Retrieves the bitrate of the given file, in kb/s.
getBitrate :: TagLib Int
getBitrate = unpackIntAP c_taglib_audioproperties_bitrate

-- | Retrieves the sample rate of the given file, in Hz.
getSampleRate :: TagLib Int
getSampleRate = unpackIntAP c_taglib_audioproperties_samplerate

-- | Retrieves the number of channels in the given file.
getChannels :: TagLib Int
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

