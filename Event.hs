
module Event
   (
     Event (..)
   , processEvents
   , processEvent
   ) where

import           Control.Concurrent.STM (TQueue, atomically, tryReadTQueue)
import           Control.Monad.RWS.Strict (RWST, ask, asks, liftIO)

import qualified Graphics.UI.GLFW as GLFW

import           Types

processEvents :: GameState ()
processEvents = do
   tc <- asks  _envEventsChain
   me <- liftIO $ atomically $ tryReadTQueue tc
   case me of
      Just e -> do
         processEvent e
         processEvents
      Nothing -> return ()

processEvent :: Event -> GameState ()
processEvent ev =
   case ev of
      (EventMouseButton _ mb mbs mk) -> return ()

      (EventCursorPos _ x y) ->  return ()

      (EventKey win k scancode ks mk) -> return ()


