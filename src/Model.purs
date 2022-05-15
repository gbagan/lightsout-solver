module Lightsout.Model
  where
  
import Prelude
import Data.Array (replicate, modifyAtIndices)
import Data.Maybe (Maybe(..))
import Lightsout.Solver (solve)

type Model =
  { nrows :: Int
  , ncols :: Int
  , grid :: Array Boolean
  , solution :: Maybe (Array Boolean)
  }

init :: Model
init =
  { nrows: 10
  , ncols: 10
  , grid: replicate 100 true
  , solution: Nothing
  }

data Msg = Solve | Flip Int

update :: Msg -> Model -> Model
update Solve model = model { solution = solve model.ncols model.nrows model.grid }
update (Flip i) model = model { grid = model.grid # modifyAtIndices [i] not }