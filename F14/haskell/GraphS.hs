
-- GraphS.hs - Graph module using set-of-edges representation of Graphs. This
-- version uses Haskell newtype to encapsulate the internal data
-- representation.

module GraphS (Graph, makeGraph, makeDiGraph, showGraph, vertices, edges, 
       adjacent, isAdjacent) where
       
--import SetOL  -- change this name as needed to work with your set module
import Data.Set   -- change this name as needed to work with your set module

--Graph ADT signature

-- makeGraph   :: Ord a => [a] -> [(a,a)] -> Graph a
-- makeDiGraph :: Ord a => [a] -> [(a,a)] -> Graph a
-- showGraph   :: Ord a => Graph a -> ([a],[(a,a)])
-- vertices    :: Ord a => Graph a -> [a]	    -- return vertices of graph
-- edges       :: Ord a => Graph a -> [(a,a)]	    -- return edges of graph
-- adjacent    :: Ord a => Graph a -> a -> [a]	    -- return the adjacency list
-- isAdjacent  :: Ord a => Graph a -> a -> a -> Bool -- is (x,y) an edge?


-------------------------------------------------------------------------------
-- WARNING - this file is a skeleton and won't work as-is until
-- code is completed.
-------------------------------------------------------------------------------
       
-- Definition: (From Dossey p152, 3rd Ed) A directed graph is a pair (V,E)
-- where V is a finite non-empty set of "vertices", and E is a set
-- of "directed edges", which are ordered pairs of elements of V.

-- Note that the definition for undirected graphs that we are using here
-- does not allow loop edges from a node to itself and there is at most one
-- edge between any two nodes. (Directed graphs do allow reflexive edges).

-- A graph in this module is represented as set of nodes and a set of directed
-- edges. Thus, a digraph is directly represented, however, if the graph
-- is undirected, then makeGraph must generate all of the symetric pairs of
-- directed edges as needed. 

------------------------------------------------------------------------------


newtype Graph a = GraphC (Set a, Set (a,a)) deriving (Eq,Show)

-- Undirected graphs do not allow loop edges

checkGraph :: Ord a => [a] -> [(a,a)] -> Bool
checkGraph vs es 
  | not (subset endpoints vs)
                  = error "makeGraph - Some endpoints are not in vertex list"
  | isLoopEdge es = error "makeGraph - Loop edges not allowed in edge list"
  | otherwise    = True
  where
    isLoopEdge = or . fmap (\(x,y) -> x == y) 
    (xs,ys) = unzip es
    endpoints = xs ++ ys


-- DiGraphs allow loop edges

checkDiGraph :: Ord a => [a] -> [(a,a)] -> Bool
checkDiGraph vs es 
  | not (subset endpoints vs)
                 = error "makeGraph - Some endpoints are not in vertex list"
  | otherwise    = True
  where
    (xs,ys) = unzip es
    endpoints = xs ++ ys

makeGraph, makeDiGraph :: Ord a => [a] -> [(a,a)] -> Graph a

makeGraph vs es 
  | checkGraph vs es = GraphC (fromList vertices, fromList edges)
  | otherwise        = error "makeGraph - shouldn't get here"
  where
    swap (x,y) = (y,x)
    edges = es ++ fmap swap es -- add symetric pairs
    vertices = vs

makeDiGraph vs es
  | checkDiGraph vs es = GraphC (fromList vs, fromList es)
  | otherwise        = error "makeDiGraph - shouldn't get here"


showGraph :: Ord a => Graph a -> ([a],[(a,a)])
showGraph (GraphC (v,e)) = (toList v, toList e) 

vertices :: Ord a => Graph a -> [a]
vertices (GraphC (v,e)) = toList v

edges :: Ord a => Graph a -> [(a,a)]
edges (GraphC (v,e)) = toList e

adjacent :: Ord a => Graph a -> a -> [a]
adjacent (GraphC (v,(e1,e2):es)) x
	| x == e1 = e2 : (adjacent (GraphC (v,es)) x)
	| x == e2 = e1 : (adjacent (GraphC (v,es)) x)
	| otherwise = adjacent (GraphC (v,es)) x
adjacent (GraphC (s1, s2) x 
	| (s1 == isEmpty && s2 == isEmpty) = []
	| otherwise = []


isAdjacent :: Ord a => Graph a -> a -> a -> Bool
isAdjacent g x y = checkAdj g x y
	where 
		checkAdj 
			| y `elem` (adjacent g x) = True
			| otherwise = False

------------- Support functions -------------------------

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

------------------------------------------------------------------------------
-- Testing. Create your own set of test cases to supplement these.
------------------------------------------------------------------------------

g4 :: Graph Int			-- disconnected
g4 = makeGraph [1..6]
         [(1,2),(2,3),(1,3),(4,6)]

g5 :: Graph Int			-- has euler circuit
g5 = makeGraph [1..5]
         [(1,2),(1,3),(2,3),(3,4),(3,5),(4,5)]

g6 :: Graph Int			-- has euler path
g6 = makeGraph [1..6]
         [(1,2),(1,3),(2,6),(2,3),(3,4),(3,5),(4,5),(5,6)]

g7 :: Graph Int			-- has euler path
g7 = makeGraph [1..5]
         [(1,2),(2,3),(3,4),(4,5)]

gBad :: Graph Int    		-- not a graph
gBad = makeGraph [1..3] [(1,3),(3,4)]

gRef :: Graph Int    		-- has a reflexive edge
gRef = makeGraph [1..3] [(3,3),(3,2)]


-- Test cases are just a selection and not complete. You must at least
-- verify that these results are correct and then add more testing code.

t1 = showGraph g4
t2 = showGraph g5
t3 = showGraph g6
t4 = showGraph g7
t5 = showGraph gBad
t6 = showGraph gRef

t7 = adjacent g4 2
t8 = adjacent g4 5
t9 = edges g4
t10 = vertices g4
t11 = isAdjacent g4 3 5

t12 = adjacent g5 3

-- FILL IN TEST CODE ---
