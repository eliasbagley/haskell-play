import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

findy :: (Eq k) => k -> [(k,v)] -> Maybe v
findy key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap = case Map.lookup lockerNumber lockerMap of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist"
  Just (state, code) -> case state of
      Taken -> Left $ "Sorry, locker" ++ show lockerNumber ++ " is taken"
      Free -> Right code
