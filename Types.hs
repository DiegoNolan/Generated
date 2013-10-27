{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens (makeLenses)
import           Control.Concurrent.STM (TQueue)
import           Control.Monad.RWS.Strict (RWST)

import qualified Graphics.UI.GLFW as GLFW

import           PrimGraphics

type GameState = RWST Env () State IO

data Env = Env
   {
     _envEventsChain    :: TQueue Event
   , _envWindow         :: !GLFW.Window
   }

data State = State
   {
     _objects  :: [Object]
   }

data Event =
     EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
   | EventCursorPos   !GLFW.Window !Double !Double
   | EventKey         !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys

makeLenses ''Env
makeLenses ''State


