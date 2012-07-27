{-# OPTIONS_GHC -Wall #-}

module LittleDVDA.Expr where

-- We lift expressions onto mutable references, but IO is sad times --
-- we'll use ST.
import Control.Monad (liftM)
import Control.Monad.ST
import Data.STRef

-- Mutable hash tables for forming our expression graph.
import Data.Hashable ( Hashable, hash, combine )
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C

-- | Label indicating a symbolic variable or a point in the expression
-- graph.
newtype Label a = Label String deriving (Show, Eq)

instance Hashable (Label a) where
    hash (Label x) = hash "Label" `combine` hash x

-- | Unary operations (only one for now)
data Un = Negate deriving (Show, Eq, Bounded, Enum)

instance Hashable Un where
    hash = hash . show

-- | Binary operations (only two for now)
data Bin = Mul | Add deriving (Show, Eq, Bounded, Enum)

instance Hashable Bin where
    hash = hash . show 

-- | A mathematical expression.
data Expr a = EBin Bin (Expr a) (Expr a)
            | EUn Un (Expr a)
            | EConst a
            | ELabel (Label a)
              deriving (Show, Eq)

instance Hashable a => Hashable (Expr a) where
    hash (EBin f a b) = hash "Bin" `combine` hash f `combine` hash a `combine` hash b
    hash (EUn f a) = hash "Un" `combine` hash f `combine` hash a
    hash (EConst a) = hash "Const" `combine` hash a
    hash (ELabel a) = hash "Label" `combine` hash a

-- | For @Num@ operations, we perform operations immediately on
-- constants and simply create a new node for everything else.
instance (Num a, Eq a) => Num (Expr a) where
    (EConst a) * (EConst b) = EConst $ a * b
    a' * b' = EBin Mul a' b'

    (EConst a) + (EConst b) = EConst $ a + b
    a' + b' = EBin Add a' b'

    negate (EConst a) = EConst $ negate a
    negate a' = EUn Negate a'

-- | This function takes a pure @Expr@ of values and puts it on top of
-- @STRefs@.  This way the pointers are mutable when we want to
-- simplify the expression.
nodeifyExpr :: Expr a -> ST s (Expr (STRef s a))
nodeifyExpr (EConst a) = liftM EConst $ newSTRef a
nodeifyExpr (EUn f a) = liftM (EUn f) $ nodeifyExpr a
nodeifyExpr (EBin f a b) = do a' <- nodeifyExpr a
                              b' <- nodeifyExpr b
                              return $ EBin f a' b'
-- This case is a bit wacky.  Although all labels are @String@s,
-- @Label@ has a phantom type, so we need to unpack it and repack it
-- behind a new @Label@ constructor with the new @Expr@ type
nodeifyExpr (ELabel (Label l)) = return $ ELabel (Label l)

-- Some handy aliases.  
type GraphKey = Int

-- This is the recommended way to use the ST-mutable hash tables.
type HashTable s k v = C.HashTable s k v

-- | This function takes an @Expr@ tree of mutable (@STRef@) values
-- and recasts it as a hash table, in order to perform simplification.
-- It returns the updated expression and the hash of the current node.
-- Both of these are used by the caller for node construction (the
-- parent node).
nodesToGraph' :: Hashable a => 
                 HashTable s GraphKey (Expr (STRef s a)) -- ^ Hash table to mutate
              -> Expr (STRef s a) -- ^ Current expression to add
              -> ST s (HashTable s GraphKey (Expr (STRef s a)), Expr (STRef s a), GraphKey) 
-- It's a little annoying that we have to work around the STRef for
-- computing the hash.
nodesToGraph' hm e@(EConst a) = 
    do r <- readSTRef a  -- Get the constant value for hash computation
       let hr = hash r
       H.insert hm hr e  -- Put the expression into the hashtable
       return (hm, e, hr)  -- Give back the modified hashmap and the expression
nodesToGraph' hm e@(ELabel l) = 
    do H.insert hm hl e
       return (hm, e, hl)
    where hl = hash l
nodesToGraph' hm (EUn f a) = 
    do (hm', r, hr) <- nodesToGraph' hm a  -- Graphify the predecessor nodes
       let e' = (EUn f r)  -- Replace the node pointer with the one to the hashtable
           newhash = hash "Un" `combine` hash f `combine` hr
       H.insert hm' newhash e'  -- Insert the new node.  It turns out
                                -- that the @Hashable@ method @hash@
                                -- for @Int@s is just @id@, so we can
                                -- compute the hash ourselves (to get
                                -- around the @ST@ constraint) with no
                                -- performance penalty!
       return (hm', e', newhash) -- Return the new node.
nodesToGraph' hm (EBin f a b) = 
    do (hm', ra, hra) <- nodesToGraph' hm a  -- Graphify the predecessor nodes
       (hm'', rb, hrb) <- nodesToGraph' hm' b
       let e' = (EBin f ra rb)  -- Replace the node pointer with ones to the hashtable
           newhash = hash "Bin" `combine` hash f `combine` hra `combine` hrb
       H.insert hm'' newhash e'  -- Insert the new node.  
       return (hm'', e', newhash) -- Return the new node.

-- | This is the top-level function which takes in an @Expr@ (tree)
-- and returns a @HashTable@ with its values.
nodesToGraph :: Hashable a => 
                Expr (STRef s a) 
             -> ST s (HashTable s GraphKey (Expr (STRef s a)), Expr (STRef s a), GraphKey)
nodesToGraph e = H.new >>= \h -> nodesToGraph' h e

-- | This is a simple test @Expr@.
testExpr :: Expr Double
testExpr = EBin Mul (EBin Add (ELabel (Label "x")) (ELabel (Label "y"))) (EConst 2)

-- | This is simply a test function for printing out @Expr@s after
-- they've been put on top of @STRef@s.
getNode :: Show a => Expr (STRef s a) -> ST s String
getNode (EConst a) = readSTRef a >>= \x -> return $ show x
getNode (EUn f a) = do a' <- getNode a
                       return $ (show f) ++ "(" ++ a' ++ ")"
getNode (EBin f a b) = do a' <- getNode a
                          b' <- getNode b
                          return $ (show f) ++ "(" ++ a' ++ ", " ++ b' ++ ")"
getNode (ELabel (Label l)) = return l

-- example: 
-- runST $ nodeifyExpr testExpr >>= getNode
