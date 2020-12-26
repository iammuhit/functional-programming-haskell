module Graph(Graph,empty,addVertex,addEdge,vertices,neighbors) where  -- modify as needed

type Vertex a = a
type Edge a = (Vertex a, Vertex a)
type Graph a = ([Vertex a], [Edge a])

-- data Graph a = Graph ([a],[(a,a)]) deriving (Show)

{-  empty

    Get an empty graph

    RETURNS: empty graph
    EXAMPLES: empty == ([],[])
-}
empty :: Graph a
empty = ([],[])

{-  addVertex g v

    Takes a graph and a new vertex

    RETURNS: the graph after inserting that new vertex into it
    EXAMPLES: Graph.addVertex Graph.empty "a" == (["a"],[])
-}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (vs,es) v
    | elem v vs  = error "vertex already exists" 
    | otherwise = (vs ++ [v], es)

{-  addEdge g (u,v)

    Takes a graph and an edge (pairs of vertex)

    RETURNS: the graph after inserting that new edge into it
    EXAMPLES: Graph.addEdge (["a","b","c"],[]) ("a","b") == (["a","b","c"],[("a","b")])
-}
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (vs,es) (u,v) 
    | elem (u,v) es || elem (v,u) es = error "edge already exists" 
    | otherwise = (vs, es ++ [(u,v)])

{-  vertices g

    Takes a graph

    RETURNS: the list vertices
    EXAMPLES: vertices (["a","b","c"],[]) == ["a","b","c"]
-}
vertices :: Eq a => Graph a -> [a]
vertices (vs,_) = vs

{-  neighbors g n

    Takes a graph and a vertex

    RETURNS: the list of vertex which are neighbors of vertex n
    VARIANTS: es
    EXAMPLES: neighbors (["a","b","c"],[("a","b"),("a","c"),("b","c")]) == ["b","c"]
-}
neighbors :: Eq a => Graph a -> a -> [a]
neighbors (vs,[]) _ = []
neighbors (vs, (u,v) : es) n 
    | n == u            = v : neighbors (vs,es) n
    | n == v            = u : neighbors (vs,es) n
    | (u,v) : es == []  = []
    | otherwise         = neighbors (vs,es) n


-- For all operations, state their (worst-case) time complexity,
-- expressed as a function of the number of vertices or edges (or
-- both) that are contained in the graph.

{-
    empty O(1)
    addVertex O(|V|)
    addEdge O(|E|)
    vertices O(1)
    neighbors O(|E|)
-}