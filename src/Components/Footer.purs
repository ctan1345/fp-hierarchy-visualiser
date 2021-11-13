module Components.Footer (footer) where

import React.Basic.DOM as R
import React.Basic.Hooks as React

footer :: React.JSX
footer =
  R.footer
    { className: "text-gray-800 w-full flex items-center justify-center py-3 px-4 text-gray-800"
    , children:
        [ R.p_
            [ R.text "The source code of this website can be on "
            , R.a
                { className: "text-blue-600 hover:text-blue-500"
                , target: "blank"
                , href: "https://github.com/ctan1345/fp-hierarchy-visualiser"
                , children: [ R.text "Github" ]
                }
            ]
        ]
    }
