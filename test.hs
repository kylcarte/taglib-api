
import Control.Monad.Trans.Reader
import Control.Monad.Trans.List

newtype Test a = Test { runTest :: ListT (ReaderT Env IO) a }

type Env = ()


