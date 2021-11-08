module Hierarchy.Validate where

import Prelude

import Data.Map (Map)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, unwrap)
import Data.Tree (Tree)
import Record as R
import Hierarchy.Tree (Node, TreeNode(..), foldWithLevel, mapWithLevel)
import Type.Proxy (Proxy(..))

type Validated = { isValid :: Boolean }

newtype ValidatedNode = ValidatedNode (Node ( isValid :: Boolean ))
instance Show ValidatedNode where
  show (ValidatedNode node) = node.code <> " (" <> node.name <> ")"

derive newtype instance Eq ValidatedNode
derive newtype instance Ord ValidatedNode

derive instance Newtype ValidatedNode _

type ParentByChildMap = Map { level :: Int, code :: String } (Additive Int)

isValid :: ValidatedNode -> Boolean
isValid (ValidatedNode n) = n.isValid

checkValid :: Tree TreeNode -> Tree ValidatedNode
checkValid tree = mapWithLevel (validate (parentByChild tree)) tree
  where
  parentByChild :: Tree TreeNode -> ParentByChildMap
  parentByChild = foldWithLevel accumParentByChild M.empty

  accumParentByChild :: Int -> ParentByChildMap -> TreeNode -> ParentByChildMap
  accumParentByChild i m (TreeNode node) = M.alter (pure <<< append (pure 1) <<< fromMaybe mempty) { level: i, code: node.code } m

  validate :: ParentByChildMap -> Int -> TreeNode -> ValidatedNode
  validate m i (TreeNode node) = ValidatedNode $ R.insert (Proxy :: _ "isValid") (fromMaybe false $ (>) 2 <<< unwrap <$> M.lookup { level: i, code: node.code } m) node
