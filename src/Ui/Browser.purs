module Ui.Browser where

import Prelude

import Effect (Effect)
import Ui.Editor.Config as Config

foreign import navigator_clibpoard_writeText :: String -> Effect Unit

play_audio :: String -> Effect Unit
play_audio =
  if Config.sound_effects then play_audio_
  else const $ pure unit

foreign import play_audio_ :: String -> Effect Unit

