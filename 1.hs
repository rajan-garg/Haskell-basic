import Data.Set (Set)
import qualified Data.Set as Set

findUnion a b = Set.union (Set.fromList a) (Set.fromList b)						-- union function for sets

findIntersection a b = Set.intersection (Set.fromList a) (Set.fromList b)		-- intersection function for sets

checkNull a = Set.null (Set.fromList a)					-- check if  set is empty
		
findSubtraction a b = Set.fromList ([c | c <- a, c `notElem` b])	-- subtraction of b's elements from a set

findAddition a b = Set.fromList ([x + y | x <- a, y <- b])			-- addition of 2 sets