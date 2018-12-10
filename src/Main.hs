{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}



module Main where

import Env(Env(..))
import App(State(..))
import Input(Event(..), Dir(..))
import SDL(($=))

import qualified SDL
--import qualified App
import qualified Env
import qualified Loop
import qualified Input
import Data.List(foldl')
import System.Environment(getProgName)
import SDL.Vect (Point(P), V2(..), V4(..))



type GlobalEnv = Env Input.Events App.State

initialize :: IO GlobalEnv
initialize = do

  printDebugInfo

  let flags = [SDL.InitVideo]
  SDL.initialize flags

  window <- SDL.createWindow
    ""
    SDL.defaultWindow {
      SDL.windowInitialSize = V2
          (fromIntegral wWidth)
          (fromIntegral wHeight)
    }

  renderer <- SDL.createRenderer
    window
    (-1)
    SDL.defaultRenderer

  SDL.showWindow window

  pure $ MkEnv {
    _envCont = Loop.body,
    _envRender = \e -> rndr renderer (_envAppState e),
    _envEventFetch = const $ get,
    _envUpdate = upd,
    _envFinalize = const $ fin window renderer,
    _envFpsCap = 60,
    _envAppState = App.MkState {xPos = wWidth `div` 2, yPos = wHeight `div` 2}
  }

  where

    wWidth, wHeight :: Int
    (wWidth, wHeight) = (800, 600)

    printDebugInfo :: IO ()
    printDebugInfo = do
      pn <- getProgName
      putStrLn $ "Executable: \"" ++ pn ++ "\""
      sdlV <- SDL.version
      putStrLn $ "SDL version: " ++ show (sdlV :: (Int, Int, Int))

    get :: IO Input.Events
    get =
      do
        sdlEvents <- SDL.pollEvents
        -- mapM_ (putStrLn . show) sdlEvents
        pure $ reverse $ foldl' (\a e ->
            case eventTrans e of
              Nothing -> a
              Just e' -> e' : a
          )
          []
          sdlEvents
      where
        eventTrans :: SDL.Event -> Maybe Input.Event
        eventTrans SDL.Event{SDL.eventPayload = SDL.QuitEvent} = Just QUIT
        eventTrans SDL.Event{SDL.eventPayload = SDL.TextInputEvent e} =
          case SDL.textInputEventText e of
            "w" -> Just (GO F)
            "a" -> Just (GO L)
            "s" -> Just (GO B)
            "d" -> Just (GO R)
            _   -> Nothing
        eventTrans _ = Nothing

    upd :: GlobalEnv -> Input.Events -> GlobalEnv
    upd env events = foldl' evalStep env events where
      evalStep :: GlobalEnv -> Input.Event -> GlobalEnv
      evalStep e@(MkEnv {_envAppState = s}) (GO F) = e {_envAppState = s {yPos = yPos s - 5}}
      evalStep e@(MkEnv {_envAppState = s}) (GO B) = e {_envAppState = s {yPos = yPos s + 5}}
      evalStep e@(MkEnv {_envAppState = s}) (GO L) = e {_envAppState = s {xPos = xPos s - 5}}
      evalStep e@(MkEnv {_envAppState = s}) (GO R) = e {_envAppState = s {xPos = xPos s + 5}}
      evalStep e                            (QUIT) = e {_envCont  = Loop.stop}

    rndr :: SDL.Renderer -> App.State -> IO ()
    rndr r s = do
      SDL.rendererDrawColor r $= V4 0 0 0 0
      SDL.clear r
      SDL.rendererDrawColor r $= V4 0 maxBound 0 0
      SDL.drawPoint r
        (P
          (V2
            (fromIntegral $ xPos s)
            (fromIntegral $ yPos s)
          )
        )
      SDL.present r

    fin :: SDL.Window -> SDL.Renderer -> IO ()
    fin w r = do
      SDL.destroyRenderer r
      putStrLn "SDL renderer destroyed"
      SDL.destroyWindow w
      putStrLn "SDL window destroyed"
      SDL.quit
      putStrLn "SDL finalized"



main :: IO ()
main = do
  env  <- initialize
  env' <- Env.run env
  Env.finalize env'

