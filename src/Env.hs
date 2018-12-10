module Env where



import Util
import System.IO(IO)



data Env e s = MkEnv {
  _envCont       :: Env e s -> IO (Env e s),
  _envRender     :: Env e s -> IO (),
  _envEventFetch :: Env e s -> IO e,
  _envUpdate     :: Env e s -> e -> Env e s,
  _envFinalize   :: Env e s -> IO (),
  _envFpsCap     :: FPS,
  _envAppState   :: s

}

_ec :: (a -> a -> r) -> (a -> r)
_ec m c = m c c

run :: Env e s -> IO (Env e s)

run env = _ec _envCont env

render :: Env e s -> IO ()
render env = _ec _envRender env

fetch :: Env e s -> IO e
fetch env = _ec _envEventFetch env

update :: Env e s -> e -> Env e s
update env events = _ec _envUpdate env events

finalize :: Env e s -> IO ()
finalize env = _ec _envFinalize env

fpsCap :: Env e s -> FPS
fpsCap = _envFpsCap
