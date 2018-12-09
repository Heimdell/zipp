
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

    = Extensible zipper.

    == Slogan

    Traverse any datatype you want! Even `Int`! No constraints on traversed object!*

    /* You still need to provide `Lens`es, `Traversal`s or `Direction`s, though./

    == Example

    > data Tree = Bin { _l, _r :: Tree, _naem :: String } | Nip { _naem :: String }
    >
    > makeLenses ''Tree
    >
    > data Dir = L | R deriving Show
    >
    > test = do
    >   let tree = Bin (Bin (Nip "lol") (Nip "kek") "lal") (Bin (Nip "foo") (Nip "bar") "qux") "all"
    >
    >   let left  = fromTraversal L l
    >   let right = fromTraversal R r
    >
    >   (tree', res) <- with' tree $ do
    >       go left
    >       go left
    >       change (naem .~ "HELLO")
    >       up
    >       up
    >       res <- peek naem
    >       go right
    >       go right
    >       return res
    >
    >   print res

    The 'tree'' should be a 'tree' with "lol" replaced by \"HELLO" and the 'res' would be "bar".

    This example doesn't include `Handlers` for the sake of brevity. Basically, the are post-actions to
    be done after you do something. They return a `Bool` and if its `False`, their effect on the object
    is discarded (but not the effect on the additional state!).

    == Capabilites

    This is an implementation of iterator that can

    1. `go` down the structure using a `Lens` (that might fail to deliver the destination).
    2. Go `up` to the parent node.
    3. Perform `change` on a node.
    4. Do `plugState` to plug in some change on additional state.

    Also, each of these `Action`s triggers a handler you provided.

    You run it `with` some object, `Config`uration (additional state and `Handlers`) and an `Action`
    to be performed. After its done with its actions, it reconstructs the object as a whole
    - also returning final form of additional state and an action result.

    You can also run it using `with'`. In that case you don't have to provide
    additional state (will be `()`) and handlers (will be inert).

    It also runs on top of any monad you like, and so do its handlers!
    Just make sure you can `throwM` in it.

    It also doesn't put any constrains on traversed object whatsoever.

    You can traverse `Int` if you so choose, just find some `Direction`s or `Lens`es over it.

    == Handlers.

    Handlers are used if you want to update your structure after changes in some non-trivial way.

    1. `onUp` will fire /before/ you go `up`, it will receive `Bool` argument specifying if there were any changes.
    2. `onDown` will fire /before/ you `go` somewhere. It will also receive the name of the direction.
    3. `onChange` will fire /after/ each change to the locus - so you can post-update your structure.

    == Dependencies

    It doesn't depend on @lens@ package, I used @microlens-platform@ instead.
-}

module Control.Zipp
    ( -- * Zipper
      Action
    , with, Config(..)
    , with'

      -- * Basic actions
    , up, CantGoUp(..)
    , go, CantGoThere (..)
    , peek
    , change
    , perform
    , handle

      -- * Direction
    , Direction(..)

      -- * Access to state extension
    , plugState
    , Handler
    , Handlers(..)
    , HandlersEnv, here, situation

      -- * Helpers
    , fromTraversal
    , reconstruct

      -- * Lifts
    , lift
    , liftIO
    )
  where

import           Control.Monad        (when)
import           Control.Monad.Catch  (Exception, MonadCatch (..),
                                       MonadThrow (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import           Control.Monad.State  (MonadState (..), MonadTrans (..),
                                       StateT (..), execStateT, liftIO)

import           Data.Typeable        (Typeable)

import           Lens.Micro.Platform  (SimpleGetter, Traversal', makeLenses,
                                       singular, to, use, (%=), (&), (.=), (.~),
                                       (^.), (^?))

data ZipperState ext dir a m = ZipperState
    { _ext   :: ext
    , _locus :: a
    , _loci  :: [Layer dir a m]
    , _dirty :: Bool
    }

data Layer dir a m = Layer
    { _cameFrom :: Direction dir a m
    , _place    :: a
    , _update   :: Bool
    }

-- | The state the handler has access to.
data HandlersEnv ext a = HandlersEnv
    { _here      :: a
    , _situation :: ext
    }

-- | Basically, a move command to plug in `go`, with a "decomposed lens" inside.
--
--   The "lens" is a pair of getter and setter, operating in the same monad whole system
--   stands on. That's because /maybe/ your structure is lazy-loaded from some database,
--   and you need a `Monad` for it.
--
data Direction dir a m = Direction
    { designation :: dir
    , tearOur     :: a -> m a
    , jamIn       :: a -> a -> m a
    }

-- | Type for pluggable reaction.
--
--   It is a stateful computation, that tells us if there's a need to mark object we're traversing dirty.
--
type Handler ext a m = StateT (HandlersEnv ext a) m Bool

-- | Bag of handlers.
data Handlers ext dir a m = Handlers
    { onUp     :: Bool -> Handler ext a m  -- ^ /Before/ we go up or finally exit.
    , onDown   :: dir  -> Handler ext a m  -- ^ /Before/ we go specified direction.
    , onChange ::         Handler ext a m  -- ^ /After/ `change` is called.
    }

-- | Configuration for zipper to run.
data Config ext dir a m = Config
    { initial  :: ext                     -- ^ Initial plugin state.
    , handlers :: Handlers ext dir a m    -- ^ Handlers.
    }

-- | Monad transformer. Can read handlers and read-write zipper state.
--
--   [@ext@] is an additional state
--   [@dir@] is a marker of `Direction`
--   [@a@] is a type of object you traverse
--   [@m@] is monad you enrich
--   [@r@] is a result of the `Action`
--
newtype Action ext dir a m r = Action
    { runAction :: ReaderT (Handlers ext dir a m) (StateT (ZipperState ext dir a m) m) r
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState  (ZipperState ext dir a m)
    , MonadReader (Handlers    ext dir a m)
    , MonadThrow
    , MonadCatch
    )

instance MonadTrans (Action ext dir a) where
    lift = Action . lift . lift

makeLenses ''ZipperState
makeLenses ''Layer
makeLenses ''HandlersEnv

-- | Allows accessing plugin state only.
plugState :: Monad m => StateT ext m r -> Action ext dir a m r
plugState action = do
    pluginState         <- use ext
    (res, pluginState') <- lift $ runStateT action pluginState
    ext .= pluginState'
    return res

plugHandler :: Monad m => Handler ext a m -> Action ext dir a m ()
plugHandler action = do
    pluginState          <- use ext
    loc                  <- use locus
    (r, HandlersEnv a s) <- lift $ action `runStateT` HandlersEnv loc pluginState
    ext   .= s
    locus .= a
    when r $ do
        dirty .= True

-- | Open a zipper with given config on given object, perform given action and then close.
with :: MonadThrow m => Config ext dir a m -> a -> Action ext dir a m r -> m (r, a, ext)
with (Config _ext handlers) _locus action = do
    let start = ZipperState { _ext, _locus, _loci = [], _dirty = False }
    (r, ZipperState ext' locus' [] _) <-
        action <* exit
            & runAction
            & (`runReaderT` handlers)
            & (`runStateT`  start)

    return (r, locus', ext')

-- | Invoke an action with `()` as additional state and with inert handlers.
with' :: MonadThrow m => a -> Action () dir a m r -> m (r, a)
with' object action = multOne <$> with Config
    { initial = ()
    , handlers = Handlers
        { onUp     = \_ -> return False
        , onDown   = \_ -> return False
        , onChange =       return False
        }
    }
    object
    action
  where
    multOne (a, b, ()) = (a, b)

exit :: MonadThrow m => Action ext dir a m ()
exit = do
    locs <- use loci
    if null locs
    then do
        isDirty <- use dirty
        plugHandler . ($ isDirty) =<< asks onUp

    else do
        _ <- up
        exit

-- | Thrown if you tried go `up` from root position.
data CantGoUp = CantGoUp
    deriving (Eq, Show, Typeable)

instance Exception CantGoUp

-- | Apply all changes in the current node and return to parent.
--
--   Returns a direction you came from.
--
--   Throws `CantGoUp` if you are in a root node.
--
up :: MonadThrow m => Action ext dir a m dir
up = do
    use loci >>= \case
        [] -> do
            throwM CantGoUp

        prev : rest -> do
            isDirty <- use dirty
            plugHandler . ($ isDirty) =<< asks onUp

            loci  .= rest
            if isDirty
            then do
                loc  <- use locus
                loc' <- lift $ (prev^.cameFrom.to jamIn) (prev^.place) loc

                locus .= loc'
                dirty .= True

            else do
                locus .= prev^.place
                dirty .= prev^.update

            return $ prev^.cameFrom.to designation

-- | Thrown if its impossible to move where you want.
data CantGoThere = CantGoThere
    deriving (Eq, Show, Typeable)

instance Exception CantGoThere

-- | Attempts to move in given direction.
--
--   Throws `CantGoThere` if you can't move there.
--
go :: MonadThrow m => Direction dir a m -> Action ext dir a m ()
go dir = do
    loc  <- use locus
    loc' <- lift $ tearOur dir loc

    plugHandler . ($ designation dir) =<< asks onDown

    upd  <- use dirty

    loci  %= (Layer dir loc upd :)
    locus .= loc'
    dirty .= False

-- | Apply the getter to the current locus and return the result.
peek :: Monad m => SimpleGetter a r -> Action ext dir a m r
peek getter = use $ locus.getter

-- | Perform given pure change on current locus of editation.
--
--   Marks node to update even if you pass `id`, calls `onChange` handler.
--
change :: Monad m => (a -> a) -> Action ext dir a m ()
change f = do
    locus %= f
    dirty .= True
    plugHandler =<< asks onChange

-- | Perform given action in base nonad on the current locus of editation.
--
--   Marks node to update even if you pass `pure` or `return`, calls `onChange` handler.
--
perform :: Monad m => (a -> m a) -> Action ext dir a m ()
perform f = do
    (locus .=) =<< lift . f =<< use locus
    dirty .= True
    plugHandler =<< asks onChange

-- | Perform a handler-like computation on object and additional state.
--
--   Marks node to update even if you pass `pure` or `return`, calls `onChange` handler.
--
handle :: Monad m => StateT (HandlersEnv ext a) m r -> Action ext dir a m r
handle action = do
    pluginState          <- use ext
    loc                  <- use locus
    (r, HandlersEnv a s) <- lift $ action `runStateT` HandlersEnv loc pluginState
    ext   .= s
    locus .= a
    dirty .= True
    return r

-- | Retrieve whole object in this very moment.
--
--   Warning: it will go `up` and call the `onUp` handlers!
--
--   This will not touch state you are in, though. Just make sure
--    you didn't put /mutable vars/ in there.
--
reconstruct :: MonadCatch m => Action ext dir a m a
reconstruct = do
    s <- get
    e <- ask
    lift $ do
        s' <- exit
            & runAction
            & (`runReaderT` e)
            & (`execStateT` s)
        return (s'^.locus)

-- | Construct a `Direction` out of `Traversal'`.
--
--   Its from `Traversal'`, because if you `makeLenses`, it will produce
--   a bunch of traversals (for non-total fiels) and lenses (which /are/ traversals).
--
fromTraversal :: MonadThrow m => dir -> Traversal' a a -> Direction dir a m
fromTraversal designation traversal = Direction
    { designation
    , tearOur = \s -> do
        case s^? traversal of
            Just it -> return it
            Nothing -> throwM CantGoThere

    , jamIn = \s a -> do
        return $ s & singular traversal .~ a
    }

-------------------------------------------------------------------------------
-- Prototype area -------------------------------------------------------------
-------------------------------------------------------------------------------
{-
data Tree = Bin { _l, _r :: Tree, _naem :: String } | Nip { _naem :: String }
    deriving Show

makeLenses ''Tree

data Dir = L | R deriving Show

type Ext = [String]

test :: IO ()
test = do
    let
      config = Config
        { initial = []
        , handlers = Handlers
            { onUp = \yeah -> do
                a <- use $ here.naem
                situation %= (++ if yeah then ["UP!", a] else ["up", a])
                return False

            , onDown = \(dir :: Dir) -> do
                a <- use $ here.naem
                situation %= (++ [show dir, a])
                return False

            , onChange = do
                situation %= (++ ["SPLAT"])
                here.naem %= (++ " SPLAT")
                return True
            }
        }
      tree = Bin (Bin (Nip "lol") (Nip "kek") "lal") (Bin (Nip "foo") (Nip "bar") "qux") "all"

      left  = fromTraversal L l
      right = fromTraversal R r

    res <- with config tree $ do
        go left
        go left
        change (naem .~ "HELLO")
        up
        up
        go right
        go right
        return ()

    print res
-}
