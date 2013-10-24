{-# LANGUAGE TypeFamilies, DeriveDataTypeable, FlexibleContexts, GADTs #-}
module Main (
    main
) where

import Data.Typeable
import qualified Data.Traversable as Traversable
import Control.Applicative
import Control.Monad
import Data.Maybe

import Data.IORef
import System.Mem.Weak

--transformers package:
import Control.Monad.IO.Class

main = example

data Person = Person {
    name :: String
} deriving (Show, Typeable)

data Company = Company {
    legalName :: String
} deriving (Show, Typeable)

-- the only thing we need MonadIO for in this exmple is printing output
example :: (MonadIO m, MonadComputed m) => m ()
example = do
    -- aliceRef :: Reference Person
    aliceRef <- new $ Person { name = "Alice" }
    -- alice :: Computed Person
    alice <- track aliceRef
    bobRef <- new $ Person { name = "Bob" }
    bob <- track bobRef
    -- companyRef :: Reference Company
    companyRef <- new $ Company { legalName = "Eve's Surveillance" }
    -- company :: Computed Company
    company <- track companyRef
    
    let dumpValues = do
        (liftIO . print) =<< runComputed alice
        (liftIO . print) =<< runComputed bob
        (liftIO . print) =<< runComputed company
        (liftIO . putStrLn) ""
    dumpValues
    
    people <- share $ Traversable.sequenceA [alice, bob]
    structure2 <- share $ do
        a <- alice
        c <- company
        return (a, c)
    structure3 <- share $ (pure (,)) <*> structure2 <*> bob
    
    let dumpStructures = do
        (liftIO . print) =<< runComputed people
        (liftIO . print) =<< runComputed structure2
        (liftIO . print) =<< runComputed structure3
        (liftIO . putStrLn) ""    
    dumpStructures

    set aliceRef Person { name = "Mike" }
    
    dumpValues
    dumpStructures
    
    set companyRef Company { legalName = "Mike's Meddling" }
        
    dumpValues
    dumpStructures
        
    -- Run it again to demonstrate that only one get is required to get the entire pure structure
    dumpStructures
    
    set aliceRef Person { name = "Alice Again" }
    
    dumpValues
    dumpStructures
        
    set bobRef Person { name = "Bob doesn't touch structure2" }
    
    dumpValues
    dumpStructures
    


-- Class for a typed dictionary in a monadic context
class (Monad m) => MonadReference m where
    type Reference :: * -> *
    new :: (Typeable a) => a -> m (Reference a)
    set :: (Typeable a) => Reference a -> a -> m ()
    get :: (Typeable a) => Reference a -> m a


-- Class for a monad with state dependent values
class (MonadReference m, Applicative Computed, Monad Computed) => MonadComputed m where
    type Computed :: * -> *
    track :: (Typeable a) => Reference a -> m (Computed a)
    share ::  (Typeable a) => (Computed a) -> m (Computed a)
    runComputed :: (Typeable a) => (Computed a) -> m a



-- A published value for IO, using Weak references to the subscribers
data Published a = Published {
    valueRef :: IORef a,
    subscribers :: IORef [Weak (IO ())]
}


-- A new implementation that keeps an update list
instance MonadReference IO where
    type Reference = Published
    new = newIORefPublished
    set = setIORefPublished
    get = readIORefPublished

-- Separate implemenations for these, since we'd like to drop the Typeable constraint
newIORefPublished value =
    do
        ref <- newIORef value
        subscribersRef <- newIORef []
        return Published { valueRef = ref, subscribers = subscribersRef }

setIORefPublished published value =
    do
        writeIORef (valueRef published) value
        notify $ subscribers published                 


--readIORefPublished = readIORef . valueRef

readIORefPublished x = do
    putStrLn "getting"
    readIORef $ valueRef x

notify :: IORef [Weak (IO ())] -> IO ()
notify = go
    where
        go subscribersRef = do
            subscribers <- readIORef subscribersRef
            needsCleanup <- (liftM (any id)) (mapM notifySubscriber subscribers)
            when needsCleanup $ cleanupWeakRefs subscribersRef
        notifySubscriber weakSubscriber = do
            maybeSubscriber <- deRefWeak weakSubscriber
            case maybeSubscriber of
                Nothing -> return True
                Just subscriber -> subscriber >> return False
    

cleanupWeakRefs :: IORef [Weak a] -> IO ()
cleanupWeakRefs ref = do
    weaks <- readIORef ref
    newWeaks <- (liftM catMaybes) $ mapM testWeak weaks
    writeIORef ref newWeaks
    where
        testWeak weakRef = liftM (>> Just weakRef) $ deRefWeak weakRef  


-- Data type for building computations
data IORefComputed a where 
    Pure :: a -> IORefComputed a
    Apply :: IORefComputed (b -> a) -> IORefComputed b -> IORefComputed a
    Bound :: IORefComputed b -> (b -> IORefComputed a) -> IORefComputed a
    Tracked :: Published a -> IORefComputed a
    Shared :: Published (Either (IORefComputed a) a) -> IORefComputed a

instance Monad IORefComputed where
    return = Pure
    (>>=) = Bound
    (>>) _ = id

instance Applicative IORefComputed where
    pure = return
    (<*>) = Apply
    
instance Functor IORefComputed where
    fmap = (<*>) . pure     

-- Evaluate computations built in IO
instance MonadComputed IO where
    type Computed = IORefComputed
    track = trackIORefComputed
    runComputed = evalIORefComputed
    share = shareIORefComputed

-- Separate implementations, again to drop the Typeable constraint
trackIORefComputed = return . Tracked

evalIORefComputed :: IORefComputed a -> IO a
evalIORefComputed c = 
    case c of
        Pure x -> return x
        Apply cf cx -> do
            f <- evalIORefComputed cf
            x <- evalIORefComputed cx
            return (f x) 
        Bound cx k -> do            
            value <- evalIORefComputed cx
            evalIORefComputed (k value)
        Tracked published -> readIORefPublished published
        Shared publishedThunk -> do
            thunk <- readIORefPublished publishedThunk
            case thunk of
                Left computation@(Bound cx k) -> do
                    x <- evalIORefComputed cx
                    -- Make a shared version of the computed computation
                    currentExpression <- shareIORefComputed (k x)
                    let gcKeyedCurrentExpression = Left currentExpression
                    writeIORef (valueRef publishedThunk) gcKeyedCurrentExpression
                    markDirty <- makeMarkDirty publishedThunk gcKeyedCurrentExpression computation
                    subscribeTo currentExpression markDirty
                    evalIORefComputed c
                Left computation -> do
                    value <- evalIORefComputed computation
                    writeIORef (valueRef publishedThunk) (Right value)
                    return value
                Right x ->
                    return x
            
            
shareIORefComputed :: IORefComputed a -> IO (IORefComputed a)
--shareIORefComputed c = return c
shareIORefComputed c =
    case c of          
        Apply cf cx -> do
            sharedf <- shareIORefComputed cf
            sharedx <- shareIORefComputed cx
            case (sharedf, sharedx) of
                -- Optimize away constants
                (Pure f, Pure x) -> return . Pure $ f x 
                _ -> do
                    let sharedc = sharedf <*> sharedx
                    published <- newIORefPublished $ Left sharedc
                    -- What we are going to do when either argument changes
                    markDirty <- makeMarkDirty published published sharedc
                    subscribeTo sharedf markDirty
                    subscribeTo sharedx markDirty
                    return $ Shared published     
        Bound cx k -> do
            sharedx <- shareIORefComputed cx
            case cx of
                -- Optimize away constants
                (Pure x) -> shareIORefComputed $ k x
                _ -> do
                    let dirtyc = sharedx >>= k
                    published <- newIORefPublished $ Left dirtyc
                    -- What we are going to do when the argument to k changes
                    markDirty <- makeMarkDirty published published dirtyc
                    subscribeTo sharedx markDirty            
                    return $ Shared published
        _ -> return c

makeMarkDirty :: Published (Either (IORefComputed a) a) -> k -> IORefComputed a -> IO (Weak (IO ())) 
makeMarkDirty published key definition =
    do
        let markDirty = do
            existing <- readIORef (valueRef published)
            case existing of
                Right _ -> setIORefPublished published $ Left definition
                _ -> return ()
        mkWeak key markDirty Nothing
        

subscribeTo :: IORefComputed a -> Weak (IO ()) -> IO ()
subscribeTo (Tracked published) trigger = modifyIORef' (subscribers published) (trigger :)
subscribeTo (Shared published) trigger = modifyIORef' (subscribers published) (trigger :)    
subscribeTo _ _ = return ()
   
