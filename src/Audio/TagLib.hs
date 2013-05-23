{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Audio.TagLib where
 {- Tag()
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
  , inBase
  ) where

import Control.Applicative (Applicative(..),(<$>),(<*>))
import Control.Monad ((>=>),ap)
import Control.Monad.Trans.Reader (ReaderT (..),asks)
import Data.Maybe (listToMaybe)
-}

import MonadLib

import Control.Applicative
import qualified Control.Exception as E
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable
import Data.Word (Word8)
import Foreign.C.String (CString,withCString)
import Foreign.C.Types (CInt(..),CChar(..))
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.Marshal.Array (lengthArray0,copyArray)
import qualified Data.ByteString as SI
import qualified Data.ByteString.Internal as SI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data TagLibException
  = NoSuchFileId
  | InvalidFile FilePath
  | UnableToOpen FilePath
  deriving (Show, Typeable)

instance E.Exception TagLibException

newtype TagLib a = TagLib
  { unTagLib :: StateT TagLibEnv IO a
  } deriving (Functor, Monad, Applicative)

instance BaseM TagLib IO where
  inBase = TagLib . inBase

instance StateM TagLib TagLibEnv where
  get = TagLib get
  set = TagLib . set

data TagLibEnv = TagLibEnv
  { taglibFilesOpen :: M.Map FileId TagLibFile
  , taglibNextId    :: Integer
  }

initEnv :: TagLibEnv
initEnv = TagLibEnv M.empty 0

data TagLibFile = TagLibFile
  { filePtr      :: Ptr File
  , tagPtr       :: Ptr Tag
  , audioPropPtr :: Ptr AudioProperties
  }

newtype FileId = FileId Integer deriving (Eq,Ord)

data File
data Tag
data AudioProperties

taglib :: TagLib a -> IO a
taglib m = do
  (res,fs) <- eval m'
  mapM_ cleanup fs
  c_taglib_free_strings
  return res
  where
  getOpenFilePtrs = gets $ map filePtr . M.elems . taglibFilesOpen
  eval = evalStateT initEnv . unTagLib
  m' = do
    res <- m 
    fs <- getOpenFilePtrs
    return (res,fs)
  cleanup f = do
    c_taglib_file_save f
    c_taglib_file_free f

openFile :: FilePath -> TagLib FileId
openFile fp = do
  f <- inBase $ withCString fp $ \c_path -> do
    c_file <- c_taglib_file_new c_path
    if (c_file == nullPtr)
    then E.throw (UnableToOpen fp) 
    else do
      res <- c_taglib_file_is_valid c_file
      if (res == 0)
      then E.throw (InvalidFile fp)
      else TagLibFile c_file <$>
           c_taglib_file_tag c_file <*>
           c_taglib_file_audioproperties c_file
  i <- getNextId
  sets_ $ \e -> e { taglibFilesOpen = M.insert i f $ taglibFilesOpen e }
  return i

getNextId :: TagLib FileId
getNextId = do
  i <- gets taglibNextId
  sets_ $ \e -> e { taglibNextId = taglibNextId e + 1 }
  return $ FileId i

{-
  c_taglib_file_tag c_file <*>
  c_taglib_file_audioproperties c_file

bracket :: [CString] -> (Ptr TagLibFile -> IO a) -> IO (Maybe [a])
bracket c_paths f = loop c_paths id
  where
  loop ps k = case ps of
    []    -> fmap Just $ mapM f $ k []
    p:ps' -> E.bracket (c_taglib_file_new p) cleanupFile $ \c_file ->
      whenMaybe (c_file /= nullPtr) $ do
        res <- c_taglib_file_is_valid c_file
        whenMaybe (res /= 0) $ loop ps' (k . (c_file:))
-}

fromFile :: (TagLibFile -> a) -> FileId -> TagLib a
fromFile acc fid = do
  mf <- M.lookup fid <$> gets taglibFilesOpen
  case mf of
    Just f -> return (acc f)
    Nothing -> inBase $ E.throw NoSuchFileId

gets :: (Functor m, StateM m s) => (s -> a) -> m a
gets f = f <$> get

evalStateT :: (Monad m) => s -> StateT s m a -> m a
evalStateT s m = do
  (a,s') <- runStateT s m
  return a

type SetStringTag = CString -> Ptr Tag -> IO ()
type SetIntTag = CInt -> Ptr Tag -> IO ()
type GetStringTag = Ptr Tag -> IO (Ptr Word8)
type GetIntTag = Ptr Tag -> IO CInt
type GetIntAP = Ptr AudioProperties -> IO CInt

-- File FFI ----------------------------------------------------------------{{{1

foreign import ccall "taglib_file_new"
  c_taglib_file_new :: CString -> IO (Ptr File)

foreign import ccall "taglib_file_free"
  c_taglib_file_free :: Ptr File -> IO ()

foreign import ccall "taglib_file_save"
  c_taglib_file_save :: Ptr File -> IO ()

foreign import ccall "taglib_file_is_valid"
  c_taglib_file_is_valid :: Ptr File -> IO CInt

foreign import ccall "taglib_file_tag"
  c_taglib_file_tag :: Ptr File -> IO (Ptr Tag)

foreign import ccall "taglib_file_audioproperties"
  c_taglib_file_audioproperties :: Ptr File -> IO (Ptr AudioProperties)

foreign import ccall "taglib_tag_free_strings"
  c_taglib_free_strings :: IO ()

-- }}}
-- FFI Wrappers ------------------------------------------------------------{{{1

-- | Given a @IO@ action which expects a @Tag@ pointer and @CString@,
--   lifts it into an @TagLib@ action, expecting @Text@.
packStringTag :: SetStringTag -> FileId -> T.Text -> TagLib ()
packStringTag k fid txt = do
  c_tag <- fromFile tagPtr fid
  inBase $ SI.useAsCString bs $ flip k c_tag
  where
  bs = T.encodeUtf8 txt

-- | Given a @IO@ action which expects a @Tag@ pointer and @CInt@,
--   lifts it into an @TagLib@ action, expecting a @Int@.
packIntTag :: SetIntTag -> FileId -> Int -> TagLib ()
packIntTag k fid int = do
  c_tag <- fromFile tagPtr fid
  inBase $ k (toEnum int) c_tag

-- | Given a @IO@ action which expects a @Tag@ pointer and
--   results in a @CString@, lifts it into a @TagLib@ action,
--   resulting in @Text@.
unpackStringTag :: GetStringTag -> FileId -> TagLib T.Text
unpackStringTag k fid = do
  c_tag <- fromFile tagPtr fid
  inBase $ do
    c_str <- k c_tag
    len   <- lengthArray0 0 c_str
    bs <- SI.create len (\ dst -> copyArray dst c_str len)
    return (T.decodeUtf8 bs)

-- | Given a @IO@ action which expects a @Tag@ pointer and
--   results in a @CInt@, lifts it into a @TagLib@ action,
--   resulting in @Int@.
unpackIntTag :: GetIntTag -> FileId -> TagLib Int
unpackIntTag k fid = do
  c_tag <- fromFile tagPtr fid
  inBase $ fromIntegral <$> k c_tag

-- | Given a @IO@ action which expects a @AudioProperties@ pointer and
--   results in a @CInt@, lifts it into a @TagLib@ action,
--   resulting in @Int@.
unpackIntAP :: GetIntAP -> FileId -> TagLib Int
unpackIntAP k fid = do
  c_ap <- fromFile audioPropPtr fid
  inBase $ fromIntegral <$> k c_ap

-- }}}
-- Tag Setters -------------------------------------------------------------{{{1

setTitle :: FileId ->  T.Text -> TagLib ()
setTitle = packStringTag c_taglib_tag_set_title

setArtist :: FileId ->  T.Text -> TagLib ()
setArtist = packStringTag c_taglib_tag_set_artist

setAlbum :: FileId ->  T.Text -> TagLib ()
setAlbum = packStringTag c_taglib_tag_set_album

setComment :: FileId ->  T.Text -> TagLib ()
setComment = packStringTag c_taglib_tag_set_comment

setGenre :: FileId ->  T.Text -> TagLib ()
setGenre = packStringTag c_taglib_tag_set_genre

setYear :: FileId ->  Int -> TagLib ()
setYear = packIntTag c_taglib_tag_set_year

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

-- Tag Getters -------------------------------------------------------------{{{1

getTitle :: FileId -> TagLib T.Text
getTitle  = unpackStringTag c_taglib_tag_title

getArtist :: FileId -> TagLib T.Text
getArtist  = unpackStringTag c_taglib_tag_artist

getAlbum :: FileId -> TagLib T.Text
getAlbum  = unpackStringTag c_taglib_tag_album

getComment :: FileId -> TagLib T.Text
getComment  = unpackStringTag c_taglib_tag_comment

getGenre :: FileId -> TagLib T.Text
getGenre  = unpackStringTag c_taglib_tag_genre

getYear :: FileId -> TagLib Int
getYear  = unpackIntTag c_taglib_tag_year

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

-- AudioProperties Getters -------------------------------------------------{{{1

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

{-
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

-}


