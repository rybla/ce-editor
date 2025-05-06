module Data.Diagnostic where

import Prelude

import Control.Monad.Writer (Writer, WriterT)
import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Ui.Halogen (classes)

--------------------------------------------------------------------------------
-- M
--------------------------------------------------------------------------------

type M = Writer (Array Diagnostic)
type MT m = WriterT (Array Diagnostic) m

--------------------------------------------------------------------------------
-- Diagnostic
--------------------------------------------------------------------------------

data Diagnostic = Diagnostic DiagnosticComponent

mkDiagnostic
  :: { title :: String
     , body :: DiagnosticHTML
     }
  -> Diagnostic
mkDiagnostic args = Diagnostic $ H.mkComponent { initialState, eval, render }
  where
  initialState _ = {}
  eval = H.mkEval H.defaultEval
  render _ =
    HH.div [ classes [ "Diagnostic" ] ]
      [ HH.div [ classes [ "title" ] ] [ HH.text args.title ]
      , HH.div [ classes [ "body" ] ] [ args.body ]
      ]

text :: String -> String -> Diagnostic
text title body = mkDiagnostic { title, body: HH.text body }

--------------------------------------------------------------------------------
-- DiagnosticComponent
--------------------------------------------------------------------------------

type DiagnosticComponent = H.Component DiagnosticQuery DiagnosticInput DiagnosticOutput Aff

type DiagnosticQuery :: Type -> Type
type DiagnosticQuery = Const Void

type DiagnosticInput = {}

type DiagnosticOutput = Void

type DiagnosticState = {}

data DiagnosticAction = Initialize_DiagnosticAction

type DiagnosticSlots :: Row Type
type DiagnosticSlots = ()

type DiagnosticM = H.HalogenM DiagnosticState DiagnosticAction DiagnosticSlots DiagnosticOutput Aff

type DiagnosticHTML = H.ComponentHTML DiagnosticAction DiagnosticSlots Aff

