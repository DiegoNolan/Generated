
module Callback
   (
     mouseButtonCallback
   , keyCallback
   , cursorPosCallback
   , simpleErrorCallback
   ) where

import qualified Graphics.UI.GLFW as GLFW

import           Control.Concurrent.STM (TQueue)
import           Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)

import           Types

mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton ->
   GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $
   EventMouseButton     win mb mba mk

keyCallback :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk

cursorPosCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback tc win x y = atomically $ writeTQueue tc $ EventCursorPos win x y

simpleErrorCallback e s = putStrLn $ unwords [ show e, show s]

