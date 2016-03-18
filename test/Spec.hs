module Main
    ( main
    ) where


import Data.IORef

import Types
import qualified Node as N
import qualified Var as V
import qualified State as S

import Test.Hspec

main :: IO ()
main = hspec $ describe "testing Incremental" $ do
            it "testing Map"   $ S.runTest S.testMap
            it "testing Map2"  $ S.runTest S.testMap2
            it "testing Bind1" $ S.runTest S.testBind1



