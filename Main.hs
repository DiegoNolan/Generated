
--module Main (main) where


import           Control.Lens
import           Control.Concurrent.STM (TQueue, atomically, newTQueueIO,
   tryReadTQueue, writeTQueue)
import           Control.Monad (when, unless, void)
import           Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get,
   liftIO, modify, put)
import           Control.Monad.Random (runRand)
import           System.Random

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

import           Callback
import           Color
import           Config
import           Event
import           Mountain
import           PrimGraphics
import           Types

main :: IO ()
main = do
   let width = screenWidth
       height = screenHeight

   GLFW.setErrorCallback $ Just simpleErrorCallback

   r <- GLFW.init
   when r $ do
      m <- GLFW.createWindow width height "Test" Nothing Nothing
      case m of
         Nothing -> do
            putStrLn "Your shit is fucked"
         Just win -> do

            eventsChan <- newTQueueIO :: IO (TQueue Event)

            GLFW.setWindowCloseCallback win $ Just $ (\_ -> putStrLn "Closed")
            GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
            GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan
            GLFW.setKeyCallback win $ Just $ keyCallback eventsChan
            GLFW.makeContextCurrent m

            let env = Env eventsChan win

            gen <- getStdGen

            let (mount, ng) = runRand (mountain (-10,3) (10,3) (-4) (gray 0.3)) gen

            runGame env (State [
                                 circle (7,5) 1 white
                               , mount
                               , pulley (1,1) 1.5 (3,4) 1 cyan
                               ])

            GLFW.destroyWindow win
      GLFW.terminate

runGame :: Env -> State -> IO ()
runGame env state = void $ evalRWST run env state

run :: GameState ()
run = do

   win <- asks _envWindow

   draw

   liftIO $ do
      GLFW.swapBuffers win
      GL.flush -- not necesarry
      GLFW.pollEvents
   processEvents

   mt <- liftIO GLFW.getTime
   q <- liftIO $ GLFW.windowShouldClose win
   unless q run


draw :: GameState ()
draw = do
   env <- ask
   state <- get

   newObjects <- liftIO $ mapM renderObject (state^.objects)

   objects .= newObjects


