module Pages.Home (Props, mkHome, getServerSideProps) where

import Prelude
import Components.Page as Page
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.Promise (Promise, fromAff)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Tree (Forest, Tree, mkTree)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (readString)
import Hierarchy.Tree as Tree
import Hierarchy.Tree (appendAtLevel, firstBranch, joinTree, removeLevel)
import Hierarchy.Validate (ValidatedNode(..), checkValid, isValid)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetFiles, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import Record as Record
import Record.Extra (sequenceRecord)
import Sample.Sample (sampleData)
import Type.Proxy (Proxy(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader (fileReader, readAsText, toEventTarget, result)
import Web.HTML.Event.EventTypes as EventTypes

type Props
  = { header :: String
    }

mkHome :: Page.Component Props
mkHome = do
  Page.component "Home" \env props -> React.do
    settings <- React.useContext env.settings
    showConfig /\ setShowConfig <- React.useState false
    showLevels /\ setShowLevels <- React.useState Map.empty
    augmentConfig /\ setAugmentConfig <- React.useState { level: Nothing, name: Nothing, code: Nothing }
    rawData /\ setRawData <- React.useState Nothing
    tree <-
      React.useMemo rawData \_ -> do
        case rawData of
          Nothing -> Nothing
          Just d -> hush $ checkValid <$> Tree.parseTree d
    augmentedTree <-
      React.useMemo (tree /\ augmentConfig) \_ -> do
        maybe tree (\t -> pure $ maybe t (\{ level, name, code } -> augmentAtLevel level name code t) (sequenceRecord augmentConfig)) tree
    prunedTree <-
      React.useMemo (augmentedTree /\ showLevels) \_ ->
        maybe augmentedTree
          ( \t ->
              let
                fns = removeLevel <$> (List.sortBy (flip compare) $ List.fromFoldable (Map.keys $ Map.filter (eq false) showLevels))
              in
                pure $ foldl (\t' f -> f t') t fns
          )
          augmentedTree
    invalidPaths <-
      React.useMemo tree \_ -> do
        case tree of
          Nothing -> Nothing
          Just t ->
            let
              validatedTreeWithLevel = Tree.mapWithLevel (Record.insert (Proxy :: _ "level")) (unwrap <$> t)
            in
              pure $ List.nub $ Tree.match (not <<< _.isValid) validatedTreeWithLevel
    React.useEffect settings do
      Console.log $ fromMaybe "No settings" settings
      mempty
    pure $ render props { tree, prunedTree, invalidPaths, showConfig, showLevels, augmentConfig } { setRawData, setShowConfig, setShowLevels, setAugmentConfig }
  where
  render props state handlers =
    React.fragment
      [ R.div
          { className: "max-w-5xl flex mx-auto my-12 flex-col"
          , children:
              [ R.ol
                  { children:
                      [ R.li_ [ R.text "Upload a CSV file with format [code_1, name_1, blank, code_2, name_2, blank, ..., code_n, name_n, blank]" ]
                      , R.li_
                          [ R.div
                              { className: "flex"
                              , children:
                                  [ R.input
                                      { type: "file"
                                      , onChange:
                                          handler targetFiles \files -> do
                                            _ <-
                                              runMaybeT do
                                                fileList <- MaybeT $ pure files
                                                reader <- lift fileReader
                                                f <- MaybeT $ pure $ item 0 fileList
                                                lift $ readAsText (toBlob f) reader
                                                let
                                                  et = toEventTarget reader
                                                loadListener <-
                                                  lift
                                                    $ eventListener \_ -> do
                                                        res <- result reader
                                                        handlers.setRawData \_ -> hush $ (runExcept $ readString res)
                                                lift $ addEventListener EventTypes.load loadListener false et
                                                pure unit
                                            pure unit
                                      }
                                  , R.a
                                      { className: "text-teal-700 hover:text-teal-600"
                                      , href: "#"
                                      , onClick: handler_ (handlers.setRawData \_ -> Just sampleData)
                                      , children:
                                          [ R.text "OR Use the example Australian Geography file" ]
                                      }
                                  , R.a
                                      { className: "text-blue-700 hover:text-blue-600 pl-4"
                                      , href: "/sample"
                                      , children: [ R.text " (click to see raw file)" ]
                                      }
                                  ]
                              }
                          ]
                      ]
                  }
              , renderHierarchy state.prunedTree
              , renderInvalidPaths state.invalidPaths
              ]
          }
      , R.div
          { className: guard (not state.showConfig) "invisible"
          , children: [ renderConfigPanel state.tree state.showLevels state.augmentConfig handlers.setShowConfig handlers.setShowLevels handlers.setAugmentConfig ]
          }
      , R.div
          { className: guard state.showConfig "invisible"
          , children: [ renderConfigButton handlers.setShowConfig ]
          }
      ]

  renderHierarchy =
    maybe mempty \tree ->
      R.div
        { className: "mt-5 p-4 bg-gray-300 rounded"
        , children:
            [ R.pre
                { children:
                    [ renderValidatedTree tree ]
                }
            ]
        }

  renderInvalidPaths =
    maybe mempty \trees ->
      R.div
        { className: "mt-5 p-4 bg-gray-300 rounded"
        , children:
            [ R.h1
                { className: "text-red-600 text-lg py-6"
                , children: [ R.text $ "There are total of " <> (show <<< List.length $ trees) <> " invalid paths" ]
                }
            , R.pre
                { children: renderInvalidTrees trees
                }
            ]
        }

  renderConfigButton setShowConfig =
    R.div
      { className: "fixed inset-y-1/2 cursor-pointer"
      , children:
          [ R.div
              { className: "flex w-4 h-4 bg-blue-400 hover:bg-blue-500"
              , onClick: handler_ $ setShowConfig \_ -> true
              }
          ]
      }

  renderConfigPanel tree showLevels augmentConfig setShowConfig setShowLevels setAugmentConfig =
    R.div
      { className: "top-0 fixed h-screen w-1/4 bg-blue-200 transition duration-200 shadow"
      , children:
          [ R.div
              { className: "flex flex-col h-screen py-8 px-4"
              , children:
                  [ R.h2
                      { className: "text-2xl font-bold pt-4 pb-8"
                      , children: [ R.text "Hierarchy Configuration" ]
                      }
                  , R.div
                      { className: "flex-grow"
                      , children:
                          [ R.h3
                              { className: "font-semibold"
                              , children: [ R.text "Show/Hide level" ]
                              }
                          , R.div
                              { children:
                                  maybe
                                    [ R.text "No hierarchy file is loaded" ]
                                    ( \t ->
                                        Array.fromFoldable
                                          $ ( \(i /\ level) ->
                                                R.div
                                                  { className: "cursor-pointer"
                                                  , children:
                                                      [ R.input
                                                          { id: level
                                                          , type: "checkbox"
                                                          , name: level
                                                          , checked: fromMaybe true $ Map.lookup i showLevels
                                                          , onChange: handler_ $ setShowLevels $ Map.alter (pure <<< not <<< fromMaybe true) i
                                                          }
                                                      , R.label
                                                          { className: "px-2 cursor-pointer"
                                                          , htmlFor: level
                                                          , children: [ R.text level ]
                                                          }
                                                      ]
                                                  }
                                            )
                                          <$> (List.mapWithIndex (\i n -> i /\ ("Level " <> (show $ i + 1) <> " (" <> (unwrap n).name <> ")")) $ List.drop 1 $ firstBranch t)
                                    )
                                    tree
                              }
                          ]
                      }
                  , R.div
                      { className: "flex-grow"
                      , children:
                          [ R.h3
                              { className: "font-semibold"
                              , children: [ R.text "Modify tree" ]
                              }
                          , guard (isJust tree)
                              R.div
                              { className: ""
                              , children:
                                  [ R.div
                                      { className: "my-2"
                                      , children:
                                          [ R.label { className: "px-2", children: [ R.text "Augment from level" ] }
                                          , R.input
                                              { type: "number"
                                              , value: show $ fromMaybe 0 $ augmentConfig.level
                                              , onChange: handler targetValue (\level -> (setAugmentConfig \s -> s { level = fromString =<< level }))
                                              }
                                          ]
                                      }
                                  , R.div
                                      { className: "my-2"
                                      , children:
                                          [ R.label { className: "px-2", children: [ R.text "Node name" ] }
                                          , R.input
                                              { type: "text"
                                              , placeholder: "unassigned"
                                              , value: fromMaybe "" $ augmentConfig.name
                                              , onChange: handler targetValue (\name -> (setAugmentConfig \s -> s { name = name }))
                                              }
                                          ]
                                      }
                                  , R.div
                                      { className: "my-2"
                                      , children:
                                          [ R.label { className: "px-2", children: [ R.text "Node code" ] }
                                          , R.input
                                              { type: "text"
                                              , placeholder: "unassigned"
                                              , value: fromMaybe "" $ augmentConfig.code
                                              , onChange: handler targetValue (\code -> (setAugmentConfig \s -> s { code = code }))
                                              }
                                          ]
                                      }
                                  ]
                              }
                          ]
                      }
                  , R.div
                      { children:
                          [ R.button
                              { className: "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                              , children: [ R.text "Done" ]
                              , onClick: handler_ $ setShowConfig \_ -> false
                              }
                          ]
                      }
                  ]
              }
          ]
      }

renderValidatedTree :: Tree ValidatedNode -> JSX
renderValidatedTree = drawTree 0
  where
  drawTree :: Int -> Tree ValidatedNode -> JSX
  drawTree level t =
    let
      marginLeft = css { marginLeft: level * 40 }

      node = head t

      colourClass = if isValid node then "text-gray-800" else "text-red-700"

      treeRoot = R.p { style: marginLeft, children: [ R.span { className: colourClass, children: [ R.text $ "|----> " <> show node ] } ] }

      treeChildren = drawForest (level + 1) (tail t)
    in
      treeRoot <> treeChildren

  drawForest :: Int -> Forest ValidatedNode -> JSX
  drawForest level forest = tailRec goForest { level: level, drawn: React.fragment [], current: forest }

  goForest ::
    { current :: Forest ValidatedNode, drawn :: JSX, level :: Int } ->
    Step { current :: Forest ValidatedNode, drawn :: JSX, level :: Int } JSX
  goForest { level: _, drawn: s, current: Nil } = Done s

  goForest { level: l, drawn: s, current: c : cs } =
    let
      drawnTree = drawTree l c
    in
      Loop { level: l, drawn: s <> drawnTree, current: cs }

renderInvalidTrees :: forall a. List (Tree ({ code :: String, name :: String, level :: Int | a })) -> Array JSX
renderInvalidTrees trees = R.p_ <<< Array.singleton <$> R.text <<< (\n -> "level " <> show n.level <> ": " <> n.code <> " (" <> n.name <> ")") <<< head <$> Array.fromFoldable trees

augmentAtLevel :: Int -> String -> String -> Tree ValidatedNode -> Tree ValidatedNode
augmentAtLevel level code name tree = appendAtLevel level (\_ t -> joinTree $ (\_ -> mkTree (ValidatedNode { code: (unwrap (head t)).code <> "-" <> code, name, isValid: true }) Nil) <$> (fromMaybe Nil $ List.tail $ firstBranch t)) tree

fetchData :: forall ctx. ctx -> Aff Props
fetchData _ = do
  pure $ { header: "Home" }

getServerSideProps :: forall ctx. EffectFn1 ctx (Promise { props :: Props })
getServerSideProps =
  mkEffectFn1 $ fromAff
    <<< map { props: _ }
    <<< fetchData
