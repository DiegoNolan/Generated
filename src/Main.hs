
module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Concurrent.STM (TQueue, atomically, newTQueueIO,
   tryReadTQueue, writeTQueue)
import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.RWS.Strict (RWST, ask, asks, evalRWST, get,
   liftIO, modify, put)
import           Control.Monad.Random (runRandT, RandT)
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
            [RandT g IO a] ->
            g ->
            IO ([a],g)
runGroup toRuns gen = over _1 reverse <$> foldM inner ([],gen) toRuns
      where inner (os,lg) toRun = do
                (o,ng) <- runRandT toRun lg
                return (o:os,ng)
main :: IO ()
main = do
   let width = screenWidth
       height = screenHeight

   GLFW.setErrorCallback $ Just simpleErrorCallback

   r <- GLFW.init
   when r $ do
      m <- GLFW.createWindow width height "Generated" Nothing Nothing
      case m of
         Nothing -> do
            putStrLn "Oh Dear"
         Just win -> do

            eventsChan <- newTQueueIO :: IO (TQueue Event)

            GLFW.setWindowCloseCallback win $ Just $ (\_ -> putStrLn "Closed")
            GLFW.setMouseButtonCallback win $ Just $
                mouseButtonCallback eventsChan
            GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan
            GLFW.setKeyCallback win $ Just $ keyCallback eventsChan
            GLFW.makeContextCurrent m

            let env = Env eventsChan win

            gen <- getStdGen

            let toRunGraphics =
                    [
                      (mountain (Vec2 (-10) 3) (Vec2 10 3) (-4) (gray 0.3))
                     , (grassPatch (Vec2 (-12) (-4)) (Vec2 12 (-4)) black)
                    ]
            let toRunAnims =
                    [
                      animatedTree (Vec2 6 (-4)) black
                    , animatedTree (Vec2 0 (-4)) black
                    , animatedTree (Vec2 (-6) (-4)) black
                    ]

            background <- (square (Vec2 (-10) 10) (Vec2 10 (-3.5)) blueSteel)
            moon <- circle (Vec2 7 4) 1 (gray 0.8)
            (graphics,ng) <- runGroup toRunGraphics gen
            (anims, nng)  <- runGroup toRunAnims ng

            runGame env (State (background:moon:graphics) anims)

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
            (show $ (fromIntegral $ length dropN) /
                ((last dropN) - (head dropN)))

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

   liftIO $ sequence_ (state^.graphics)
   liftIO $ mapM_ ($ time) (state^.animations)


