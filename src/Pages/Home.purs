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
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tree (Tree, Forest)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (readString)
import Hierarchy.Tree as Tree
import Hierarchy.Validate (ValidatedNode, checkValid, isValid)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetFiles)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import Record as Record
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
    rawData /\ setRawData <- React.useState Nothing
    tree <-
      React.useMemo rawData \_ -> do
        case rawData of
          Nothing -> Nothing
          Just d -> hush $ checkValid <$> Tree.parseTree d
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
    pure $ render props { tree, invalidPaths } { setRawData }
  where
  render props state handlers =
    React.fragment
      [ R.div
          { className: "max-w-5xl flex mx-auto my-12 flex-col"
          , children:
              [ R.div_
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
                  ]
              , renderHierarchy state.tree
              , renderInvalidPaths state.invalidPaths
              ]
          }
      ]

  renderHierarchy = case _ of
    Nothing -> mempty
    Just tree ->
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
    maybe mempty \trees -> do
      R.div
        { className: "mt-5 p-4 bg-gray-300 rounded"
        , children:
            [ R.pre
                { children: renderInvalidTrees trees
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

fetchData :: forall ctx. ctx -> Aff Props
fetchData _ = do
  pure $ { header: "Home" }

getServerSideProps :: forall ctx. EffectFn1 ctx (Promise { props :: Props })
getServerSideProps =
  mkEffectFn1 $ fromAff
    <<< map { props: _ }
    <<< fetchData
