module Components.Navigation (navigation) where

import Next.Link (link) as N
import React.Basic.DOM as R
import React.Basic.Hooks as React

navigation :: React.JSX
navigation =
  R.nav
    { className: "text-gray-800 w-full flex items-center justify-between py-3 px-4"
    , children:
        [ R.div
            { className: "font-extrabold text-xl"
            , children:
                [ R.text "Hierarchy visualiser" ]
            }
        ]
    }
