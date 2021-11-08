module Pages.Sample (mkSample) where

import Prelude
import Components.Page as Page
import Data.Array (singleton)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import React.Basic.DOM as R
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks as React
import Sample.Sample (sampleData)

mkSample :: Page.Component Unit
mkSample = do
  Page.component "Profile" \_ _ -> pure render
  where
  render =
    React.fragment
      [ R.div
          { className: "text-gray-800 mx-8 my-24 shadow-2xl bg-gray-200 rounded p-12 text-left"
          , children: R.p_ <<< singleton <<< R.text <$> (split (Pattern "\\n") $ show sampleData)
          }
      ]
