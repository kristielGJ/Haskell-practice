
import Search
import Data.Set (Set)
import qualified Data.Set as Set

-- crossed the river? False = near bank, True = far bank
type Bank = Bool

-- crossed: farmer, wolf, goat, cabbage
data State = State Bank Bank Bank Bank
    deriving (Eq, Ord, Show)

-- initial state
start :: State
start = State False False False False

-- goal state
finish :: State
finish = State True True True True

-- nothing will be eaten in the state
safe :: State -> Bool
safe (State f w g c) = g == f || g /= w && g /= c

-- opposite bank
cross :: Bank -> Bank
cross  = not

-- legal and safe single crossings from a given state
crossings :: State -> Set State
crossings (State f w g c) =
    Set.fromList $ filter safe $
        [State (cross f) w g c] ++
        [State (cross f) (cross w) g c | w == f] ++
        [State (cross f) w (cross g) c | g == f] ++
        [State (cross f) w g (cross c) | c == f]