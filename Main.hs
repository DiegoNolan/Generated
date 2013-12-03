
--module Main (main) where


import           Control.Lens
import           Control.Concurrent.STM (TQueue, atomically, newTQueueIO,
   tryReadTQueue, writeTQueue)
import           Control.Monad -- (when, unless, void)
import           Control.Monad.Writer
import           Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get,
   liftIO, modify, put)
import           Control.Monad.Random (runRand, Rand)
import           Data.List (foldl')
import           System.Random

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))

import           Animation
import           AnimatedTree
import           Callback
import           Color
import           Config
import           Event
import           Mountain
import           PrimGraphics
import           Tree
import           Types
import           Vec2

runGroup :: RandomGen g =>
            [Rand g a] ->
            g ->
            ([a],g)
runGroup toRuns gen = over _1 reverse $ foldl' inner ([],gen) toRuns
      where inner (os,lg) toRun = let (o,ng) = runRand toRun lg
                                  in (o:os,ng)
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

            let toRuns =
                  [
                    (mountain (Vec2 (-10) 3) (Vec2 10 3) (-4) (gray 0.3))
                  , (grassPatch (Vec2 (-12) (-4)) (Vec2 12 (-4)) black)
                  ]
                background = (square (Vec2 (-10) 10) (Vec2 10 (-3.5)) blueSteel)
                moon = circle (Vec2 7 4) 1 (gray 0.8)
                (graphics,ng) = runGroup toRuns gen
            animTree1 <- animatedTree ng (Vec2 4 (-4)) black
            animTree2 <- animatedTree gen (Vec2 (-4) (-4)) black

            runGame env (State (background:moon:graphics) [animTree1, animTree2])

            GLFW.destroyWindow win
      GLFW.terminate

runGame :: Env -> State -> IO ()
runGame env state = do
   (_,log) <- evalRWST run env state

   case log of
      [] -> return ()
      _  -> do
         putStrLn $ "Average frame rate " ++
            (show $ (fromIntegral $ length log) / ((last log) - (head log)))
         let n = 20
             dropN = drop n log
         putStrLn $ "Framerate after dropping first " ++ show n ++ " : " ++
            (show $ (fromIntegral $ length dropN) / ((last dropN) - (head dropN)))

run :: GameState ()
run = do

   win <- asks _envWindow
   mt <- liftIO GLFW.getTime

   case mt of
      Nothing -> draw 0
      Just t  -> do
         tell [t]
         draw t

   liftIO $ do
      GLFW.swapBuffers win
      GL.flush
      GLFW.pollEvents
   processEvents

   q <- liftIO $ GLFW.windowShouldClose win
   unless q run

draw :: Double -> GameState ()
draw time = do
   env <- ask
   state <- get

   newGraphics <- liftIO $ mapM renderDelayed (state^.graphics)
   liftIO $ mapM_ (renderAnimation time) (state^.animations)

   graphics .= newGraphics


