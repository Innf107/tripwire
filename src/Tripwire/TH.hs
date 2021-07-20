{-#LANGUAGE TemplateHaskell#-}
module Tripwire.TH where

import Relude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

file :: QuasiQuoter
file = QuasiQuoter {
    quoteExp  = \f -> stringE =<< runIO (readFile f)
,   quotePat  = \f -> litP . stringL =<< runIO (readFile f)
,   quoteType = \_ -> fail "file quotations cannot be used for types"
,   quoteDec  = \_ -> fail "file quotations cannot be used for declarations"
}


