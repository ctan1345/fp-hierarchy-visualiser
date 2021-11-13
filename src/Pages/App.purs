module Pages.App (Props, mkApp) where

import Prelude
import Components.Loading (mkLoading)
import Components.Navigation (navigation)
import Components.Footer (footer)
import Components.Page as Page
import Context.Settings (mkSettingsProvider)
import Control.Monad.Reader (runReaderT)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import React.Basic.Hooks as React
import React.Basic.DOM as R

type Props props
  = { "Component" :: Page.Component props
    , pageProps :: props
    }

mkApp :: forall props. Effect (EffectFn1 (Props props) React.JSX)
mkApp = do
  context /\ settingsProvider <- mkSettingsProvider
  loading <- mkLoading
  pure
    $ mkEffectFn1 \props -> do
        component <- runReaderT props."Component" { settings: context }
        pure
          $ settingsProvider
          $ React.fragment
              [ loading unit
              , R.div
                  { className: "flex flex-col h-screen"
                  , children:
                      [ navigation
                      , R.div
                          { className: "mb-auto"
                          , children:
                              [ component props.pageProps
                              ]
                          }
                      , footer
                      ]
                  }
              ]
