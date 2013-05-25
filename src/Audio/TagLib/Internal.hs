{-# LANGUAGE DeriveDataTypeable #-}

module Audio.TagLib.Internal where

import Control.Monad.State

import Control.Applicative
import Data.Typeable (Typeable())
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..),CChar(..))
import Foreign.Ptr (Ptr)
import qualified Control.Exception as E
import qualified Data.Map as M

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
  | FileClosed
  deriving (Show, Typeable)

instance E.Exception TagLibException

-- }}}

-- Monadic Operations {{{

-- | Put a new file into the Env
addNewFile :: FileId -> TagLibFile -> TagLib ()
addNewFile fid f = TagLib $ modify $ onFilesOpen $ M.insert fid f

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

-- | Embed an IO action in the TagLib context.
io :: IO a -> TagLib a
io m = TagLib $ StateT $ \e -> (,) <$> m <*> pure e

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

-- Unmanaged Interface {{{

-- | Free all the strings that TagLib has allocated.
--   Use only when handling your own memory.
--   Otherwise, 'taglib' will take care of this for you.
freeTagLibStrings :: IO ()
freeTagLibStrings = c_taglib_free_strings

-- | Remove a file from the Env
removeFile :: FileId -> TagLib ()
removeFile fid = TagLib $ modify $ onFilesOpen $ M.delete fid

-- | Run a @TagLib@ action without managing allocated resources.
--   Reading tags from a file will work regardless of whether
--   'cleanupFile' is used, but writing tags will not.
--   TagLib's strings must still be freed if a memory leak is to
--   be avoided.
runTagLib :: TagLibEnv -> TagLib a -> IO (a,TagLibEnv)
runTagLib env m = runStateT (unTagLib m) env

-- | Run an unmanaged @TagLib@ action, discarding the final Env.
evalTagLib :: TagLibEnv -> TagLib a -> IO a
evalTagLib env = fmap fst . runTagLib env

-- | Save and close a file, in case you want to manage your own memory.
--   TagLib's strings are still freed by 'taglib'.
closeFile :: FileId -> TagLib ()
closeFile fid = do
  fptr <- fromFile filePtr fid 
  removeFile fid
  io $ cleanupFile fptr

-- | The base IO action necessary to deallocate all resources 
--   associated with a single file.
cleanupFile :: Ptr File -> IO ()
cleanupFile f = do
  c_taglib_file_save f
  c_taglib_file_free f

-- }}}

