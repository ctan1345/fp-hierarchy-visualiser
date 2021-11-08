module Hierarchy.Tree where

import Prelude

import Control.Comonad.Cofree (head, tail, (:<))
import Control.Fold (Fold, foldl, unfoldFold_)
import Data.Array (drop, take)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty (fromList, toList)
import Data.List.Types (NonEmptyList)
import Data.Map (SemigroupMap(..), empty)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (sequence)
import Data.Tree (Forest, Tree, appendChild, mkTree)
import Data.Tuple (Tuple(..))
import Text.CSV as CSV

type Node a = 
  { code :: String 
  , name :: String
  | a 
  }

newtype TreeNode = TreeNode (Node ())

instance Show TreeNode where
  show (TreeNode node) = node.name <> " (" <> node.code <> ")"

derive newtype instance Eq TreeNode
derive newtype instance Ord TreeNode
derive instance Newtype TreeNode _


root :: TreeNode
root = TreeNode { code: "All", name: "All" }

type HierarchyList = NonEmptyList TreeNode

newtype TreeBuilder = TreeBuilder (SemigroupMap TreeNode TreeBuilder)
derive instance Newtype TreeBuilder _

emptyTreeBuilder :: TreeBuilder
emptyTreeBuilder = TreeBuilder (SemigroupMap empty)

parseTree :: String -> Either String (Tree TreeNode)
parseTree text = do
  csv <- lmap _.error $ CSV.parse text
  hier <- sequence $ note "Row does not have enough columns to build tree" <<< fromList <<< toHierarchy <$> csv

  let result = foldl builderFold $ toList <$> hier
  let tree = mkTree root $ toTree result

  pure tree

  where
  toHierarchy :: Array String -> List TreeNode
  toHierarchy [] = Nil
  toHierarchy xs = case take 3 xs of 
    [code, name, _] -> TreeNode { code, name } : toHierarchy (drop 3 xs)
    _ -> Nil

  toTree :: TreeBuilder -> Forest TreeNode
  toTree (TreeBuilder (SemigroupMap m)) = (\(Tuple k v) -> mkTree k $ toTree v) <$> M.toUnfoldable m


builderFold :: Fold (List TreeNode) TreeBuilder
builderFold = unfoldFold_ emptyTreeBuilder $ flip addNodes

addNodes :: List TreeNode -> TreeBuilder -> TreeBuilder
addNodes Nil ms = ms
addNodes (x:y:Nil) (TreeBuilder m) = TreeBuilder <<< SemigroupMap $ M.alter (pure <<< insertBuilder y <<< fromMaybe emptyTreeBuilder) x (unwrap m)
addNodes xs@(x:_) (TreeBuilder m) = TreeBuilder <<< SemigroupMap $ M.alter (pure <<< addNodes (L.drop 1 xs) <<< fromMaybe emptyTreeBuilder) x (unwrap m)

insertBuilder :: TreeNode -> TreeBuilder -> TreeBuilder
insertBuilder node (TreeBuilder (SemigroupMap m)) = TreeBuilder <<< SemigroupMap $ M.insert node emptyTreeBuilder m


foldWithLevel :: ∀ a b. (Int -> b -> a -> b) -> b -> Tree a -> b
foldWithLevel = go 0
  where
  go :: Int -> (Int -> b -> a -> b) -> b -> Tree a -> b
  go n f b t = let b' = (f n b $ head t) in L.foldl (go (n + 1) f) b' (tail t)

mapWithLevel :: ∀ a b. (Int -> a -> b) -> Tree a -> Tree b
mapWithLevel = go 0
  where
  go :: Int -> (Int -> a -> b) -> Tree a -> Tree b
  go n f t = (f n $ head t) :< ((go (n + 1) f) <$> tail t)

-- Given a function that takes level and parent, returns a new child
appendAtLevel :: ∀ a. Int -> (Int -> Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
appendAtLevel level f  = go 0 
  where 
  go :: Int -> Tree a -> Tree a
  go i t | i < level = head t :< ((go (i + 1)) <$> (tail t))
  go i t = case f i t of
    Nothing -> t
    Just child -> appendChild child t

firstBranch :: Tree ~> List
firstBranch t = head t : fromMaybe Nil (firstBranch <$> (L.head $ tail t))

joinTree :: ∀ a. List (Tree a) -> Maybe (Tree a)
joinTree Nil = Nothing
joinTree (x:xs) = case joinTree xs of
  Nothing -> Just x
  Just c -> Just $ appendChild c x

match :: ∀ a. (a -> Boolean) -> Tree a -> List (Tree a)
match f t = 
  let h = head t
  in if f h then t : ((match f) =<< tail t) else match f =<< tail t