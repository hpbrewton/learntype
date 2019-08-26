{-# LANGUAGE RankNTypes #-}

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:xs) 
	| f x = Just x
	| otherwise = find f xs

data LList a = BotList
	| AnyList a
	| NonEmpty a
	| Empty
	| SingList [a]
	deriving (Show)

data LRange a = BotNumber
	| AnyNumber
	| Range a a 
	| SingNumber a
	deriving (Show)

data LBool = BotBool
	| ATrue
	| AFalse
	| AnyBool
	deriving (Show)

data LUnit = BotUnit
	| AnyUnit
	deriving (Show)

data LObj a = BotObj
	| ObjSing [(String, a)]
	| ObjRest [(String, a)] [(String, a)] Bool -- required and types, optional and types, additional vars allowed?
	| AnyObj
	deriving (Show)

map_with_zipper_aux :: [a] -> ([a] -> a -> [a] -> b) -> [a] -> [b]
map_with_zipper_aux prev f [] = []
map_with_zipper_aux prev f (x:xs) = (f prev x xs) : (map_with_zipper_aux (prev ++ [x]) f xs)

map_with_zipper :: ([a] -> a -> [a] -> b) -> [a] -> [b]
map_with_zipper f xs =  map_with_zipper_aux [] f xs 

transpose_out :: (Monad m) => (a, m b)  -> m (a, b)
transpose_out (l, r) = do
	rs <- r
	return (l, rs)

transpose_list_monad :: (Monad m) => [m a] -> m [a]
transpose_list_monad [] = return []
transpose_list_monad (x:xs) = do
	xsafe <- x
	fmap (xsafe:) $ transpose_list_monad xs

learn_obj :: (Learnable a) => (LObj a) -> (LObj a -> IO Bool) -> IO (LObj a)
learn_obj (ObjSing fields) o = do
	requiredSafe <- required
	return $ ObjRest requiredSafe [] True
	where
		kvps = map_with_zipper (\lefts (k, v) right -> (k, learn v (\t -> o $ ObjSing (lefts ++ [(k, t)] ++ right)))) fields
		required = transpose_list_monad $ map transpose_out kvps

class Learnable a where
	learn :: a -> (a -> IO Bool) -> IO a

instance (Learnable a) => Learnable (LObj a) where
	learn = learn_obj

instance (Num a, Eq a, Fractional a) => Learnable (LRange a) where
	learn = range_learn

instance (Learnable a) => Learnable (LList a) where
	learn = list_learn

data LJSON = BotJson
	| ListOf (LList LJSON)
	| Number (LRange Float)
	| Null LUnit
	| Boolean LBool
	| Object (LObj LJSON)
	deriving (Show)
	
json_learn_aux :: LJSON -> (LJSON -> IO Bool) -> IO LJSON
json_learn_aux (Number e) o = fmap Number $ learn e (o . Number) 
json_learn_aux (ListOf e) o = fmap ListOf $ learn e (o . ListOf)
json_learn_aux (Boolean e) o = fmap Boolean $ learn e (o. Boolean)
json_learn_aux (Null e) o = fmap Null $ learn e (o. Null)
json_learn_aux (Object e) o = fmap Object $ learn e (o. Object)

json_learn = json_learn_aux

instance Learnable LJSON where
	learn = json_learn

boolean_learn :: LBool -> (LBool -> IO Bool) -> IO LBool
boolean_learn _ pred = do
	t <- pred ATrue
	f <- pred AFalse
	return $ case (t, f) of
		(True, True) -> AnyBool
		(True, False) -> ATrue
		(False, True) -> AFalse
		(False, False) -> BotBool

unit_learn :: LUnit -> (LUnit -> IO Bool) -> IO LUnit
unit_learn AnyUnit pred = return AnyUnit

instance Learnable LUnit where
	learn = unit_learn

instance Learnable LBool where
	learn = boolean_learn

range_learn :: (Num a, Eq a, Fractional a) => LRange a -> (LRange a -> IO Bool) -> IO (LRange a)
range_learn (SingNumber n) pred = do 
	r <- threshhold pred n 100
	l <- threshhold (fmap not . pred) (-100) n
	return $ Range l r
	where
		threshhold pred l r
			| (m == l) || (m == r) = return m
			| otherwise = do
				b <- pred (SingNumber m)
				if b
					then threshhold pred m r
					else threshhold pred l m
			where 
				m = (l+r)/2

list_learn :: (Learnable a) => LList a -> (LList a -> IO Bool) -> IO (LList a)
list_learn (SingList (x:xs)) pred = do 
	listType <- learn x (pred . (\v -> SingList (v:xs)))
	b <- pred (SingList [])
	if b
		then return $ AnyList listType
		else return $ NonEmpty listType 

numb_oracle :: LJSON -> Bool
numb_oracle (Number (SingNumber n)) = (n >= -90) && (n < 90)
numb_oracle _ = False

numb_oracle_2 :: LJSON -> Bool
numb_oracle_2 (Number (SingNumber n)) = (n <= 10) && (n > 0)

bool_oracle :: LJSON -> Bool
bool_oracle (Boolean (ATrue)) = True
bool_oracle (Boolean (AFalse)) = False


object_oracle :: LJSON -> Bool
object_oracle (Object (ObjSing xs)) = case (find ((== "x") . fst) xs, find ((=="y") . fst) xs) of
	(Just a, Just b) -> all (numb_oracle_2) $ map snd [a, b]
	_ -> False

main :: IO ()
main = do
	v <- learn (Object (ObjSing [("x", Number $ SingNumber 7), ("y", Number $ SingNumber 8)])) (return . object_oracle)
	putStrLn $ show v
	
