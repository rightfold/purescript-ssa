module Data.SSA.CFG
  ( BID
  , IID

  , CFG
  , class I, targets, operands

  , empty

  , allBs
  , findI
  , allIs
  , usages
  , outgoing
  , incoming
  ) where

import Data.Foldable (find, findMap, fold, foldMap)
import Data.List (List(Nil))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Prelude

--------------------------------------------------------------------------------

-- | Basic block identifier.
data BID
  = EntryBID
  | BID Int
derive instance eqBID  :: Eq  BID
derive instance ordBID :: Ord BID

-- | Instruction identifier.
newtype IID = IID Int
derive newtype instance eqIID  :: Eq  IID
derive newtype instance ordIID :: Ord IID

--------------------------------------------------------------------------------

-- | Control flow graph.
data CFG i = CFG Int Int (Map BID (B i))

-- | Basic block.
newtype B i = B (List (IID /\ i))
derive instance newtypeB :: Newtype (B i) _

-- | Instruction.
class I i where
  targets  :: i -> Set BID
  operands :: i -> Set IID

--------------------------------------------------------------------------------

-- | The empty control flow graph.
empty :: ∀ i. CFG i
empty = CFG 0 0 $ Map.singleton EntryBID (B Nil)

--------------------------------------------------------------------------------

-- | All basic block identifiers in a control flow graph.
allBs :: ∀ i. CFG i -> Set BID
allBs (CFG _ _ bs) = Set.fromFoldable $ Map.keys bs

-- | The instruction with some identifier in a control flow graph.
findI :: ∀ i. IID -> CFG i -> Maybe i
findI iid (CFG _ _ bs) = snd <$> findMap (find (eq iid <<< fst) <<< unwrap) bs

-- | All instructions in the basic block with some identifier in a control flow
-- | graph.
allIs :: ∀ i. BID -> CFG i -> List (IID /\ i)
allIs bid (CFG _ _ bs) = fold $ unwrap <$> Map.lookup bid bs

-- | All instructions that use a particular instruction as an operand in a
-- | control flow graph, in no particular order.
usages :: ∀ i. I i => IID -> CFG i -> List (IID /\ i)
usages iid cfg = List.filter p $ foldMap (allIs `flip` cfg) $ allBs cfg
  where p = Set.member iid <<< operands <<< snd

-- | All outgoing edges of a basic block with some identifier in a control flow
-- | graph.
outgoing :: ∀ i. I i => BID -> CFG i -> Set BID
outgoing = (foldMap (targets <<< snd) <<< _) <<< allIs

-- | All incoming edges of a basic block with some identifier in a control flow
-- | graph.
incoming :: ∀ i. I i => BID -> CFG i -> Set BID
incoming tgt cfg = filter (Set.member tgt <<< outgoing `flip` cfg) $ allBs cfg
  where filter f = Set.fromFoldable <<< List.filter f <<< Set.toUnfoldable
