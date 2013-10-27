{-# LANGUAGE TemplateHaskell #-}

module State where

import           Control.Monad.RWS.Strict 

import qualified Graphics.UI.GLFW as GLFW

data Env = Env
   {
     _envEventsChain    :: TQueue Event
   , _envWindow         :: !GLFW.Window
   }



