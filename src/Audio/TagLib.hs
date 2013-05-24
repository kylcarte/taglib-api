{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Audio.TagLib
  ( taglib
  , io
  , openFile
  , TagLib (..)
  , FileId ()
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
  ) where

import Control.Monad.State

import Control.Applicative
import Data.Typeable (Typeable())
import Foreign.C.String (CString,withCString,peekCString)
import Foreign.C.Types (CInt(..),CChar(..))
import Foreign.Ptr (Ptr,nullPtr)
import qualified Control.Exception as E
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Types {{{

-- | Monad for performing TagLib operations
newtype TagLib a = TagLib { unTagLib :: StateT TagLibEnv IO a }

instance Functor TagLib where
  fmap f (TagLib m) = TagLib $ fmap f m

instance Monad TagLib where
  return           = TagLib . return
  (TagLib m) >>= f = TagLib $ m >>= unTagLib . f

instance Applicative TagLib where
  pure  = return
  (<*>) = ap

-- | Internal representation of an open file
data TagLibFile = TagLibFile
  { filePtr      :: Ptr File
  , tagPtr       :: Ptr Tag
  , audioPropPtr :: Ptr AudioProperties
  }

-- | A handle for an open file
newtype FileId = FileId Integer deriving (Eq,Ord)

-- | Abstract C Types
data File
data Tag
data AudioProperties

-- | FFI Type Synonyms
type SetStringTag = Ptr Tag -> CString -> IO ()
type SetIntTag = Ptr Tag -> CInt -> IO ()
type GetStringTag = Ptr Tag -> IO (Ptr CChar)
type GetIntTag = Ptr Tag -> IO CInt
type GetIntAP = Ptr AudioProperties -> IO CInt

-- }}}

-- Env {{{

-- | A collection of open files, and a generator for unique file ID's
data TagLibEnv = TagLibEnv
  { taglibFilesOpen :: M.Map FileId TagLibFile
  , taglibNextId    :: Integer
  }

-- | A fresh Env
initialEnv :: TagLibEnv
initialEnv = TagLibEnv M.empty 0

-- | Record modify for taglibFilesOpen
onFilesOpen :: (M.Map FileId TagLibFile -> M.Map FileId TagLibFile)
  -> TagLibEnv -> TagLibEnv
onFilesOpen f e = e { taglibFilesOpen = f $ taglibFilesOpen e }

-- | Record modify for taglibNextId
onNextId :: (Integer -> Integer)
  -> TagLibEnv -> TagLibEnv
onNextId f e = e { taglibNextId = f $ taglibNextId e }

-- }}}

-- Exceptions {{{

-- | Exceptions that might be thrown
data TagLibException
  = NoSuchFileId
  | InvalidFile FilePath
  | UnableToOpen FilePath
  deriving (Show, Typeable)

instance E.Exception TagLibException

-- }}}

-- Main Interface {{{

-- | Run a @TagLib@ block. Save and free any files
--   left open when the block is finished, and free
--   all strings produced by taglib.
taglib :: TagLib a -> IO a
taglib m = do
  (res,fs) <- eval m'
  mapM_ cleanup fs
  c_taglib_free_strings
  return res
  where
  eval = flip evalStateT initialEnv . unTagLib
  m' = do
    res <- m 
    fs <- openFilePtrs
    return (res,fs)
  cleanup f = do
    c_taglib_file_save f
    c_taglib_file_free f

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
      else TagLibFile c_file <$>
           c_taglib_file_tag c_file <*>
           c_taglib_file_audioproperties c_file
  i <- nextId
  addNewFile i f
  return i

-- | Embed an IO action in the TagLib context.
io :: IO a -> TagLib a
io m = TagLib $ StateT $ \e -> (,) <$> m <*> pure e

-- }}}

-- Monadic Operations {{{

-- | Put a new file into the Env
addNewFile :: FileId -> TagLibFile -> TagLib ()
addNewFile i f = TagLib $ modify $ onFilesOpen $ M.insert i f

-- | Get a fresh FileId, maintaining the internal generator
nextId :: TagLib FileId
nextId = do
  i <- fromEnv taglibNextId
  TagLib $ modify $ onNextId (+1)
  return $ FileId i

-- | Get the list of currently opened files.
openFilePtrs :: TagLib [Ptr File]
openFilePtrs = fromEnv $ map filePtr . M.elems . taglibFilesOpen

-- | Call a function requiring the Env
fromEnv :: (TagLibEnv -> a) -> TagLib a
fromEnv f = TagLib $ gets f

-- | Call a function requiring a file.
--   Throws an exception should the FileId not point
--   to a currently open file.
fromFile :: (TagLibFile -> a) -> FileId -> TagLib a
fromFile acc fid = do
  mf <- M.lookup fid <$> fromEnv taglibFilesOpen
  case mf of
    Just f -> return (acc f)
    Nothing -> io $ E.throw NoSuchFileId

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

-- File FFI {{{

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

