module GraphImplementation(dot, connectedComponent) where

import Graph(Graph, empty, addVertex, addEdge, vertices, neighbors)

{-
    dot g
    Takes a graph of string values
    RETURNS: string, the dot representation of that graph
    EXAMPLES: dot (["a","b","c"],[]) == graph { a; b; c; }
-}
dot :: Graph String -> String
dot (v,e) = "\ngraph {\n" ++ dot (v,e) ++ "}\n"
    where 
        dot ([],[]) = "\0"
        dot (v:vs, []) = "\t" ++ v ++ ";\n" ++ dot (vs,[])
        dot (v:vs, (x,y) : es) = "\t" ++ v ++ ";\n" ++ dot (vs,es) ++ "\t" ++ x ++ " -- " ++ y ++ ";\n"

{-
    connectedVertices g n
    Takes a graph and a vertex
    RETURNS: the list of vertices which are connected to vertex n
    EXAMPLES: connectedVertices (["a","b","c"],[("a","b"),("a","c"),("b","c")]) == ["a","b","c"]
-}
connectedVertices :: Eq a => Graph a -> a -> [a]
connectedVertices (v,e) n
    | [x | x<-v, x==n] == [] = []
    | otherwise = connectedVertices' (v,e) [n]

{-
    connectedVertices' g stack
    Takes a graph and a list of vertices
    RETURNS: the list of connected vertices
    VARIANTS: stack
    EXAMPLES: connectedVertices' (["a","b","c"],[("a","b"),("a","c"),("b","c")]) == ["a","b","c"]
-}
connectedVertices' :: Eq a => Graph a -> [a] -> [a]
connectedVertices' ([],_) _ = []
connectedVertices' (_,_) [] = []
connectedVertices' (v,e) (top:stack)
    | [x | x<-v, x==top] == [] = connectedVertices' (newv,e) stack
    | otherwise = top : connectedVertices' (newv,e) (adjacent ++ stack)
    where
        adjacent = [x | (x,y)<-e, y==top] ++ [x | (y,x)<-e, y==top]
        newv = [x | x<-v, x/=top]

{-
    connectedEdges g stack
    Takes a graph and a list of connected vertices
    RETURNS: the list of connected edges
    VARIANTS: stack
    EXAMPLES: connectedEdges (["a","b","c"],[("a","b"),("a","c"),("b","c")]) == [("a","b"),("a","c"),("b","c")]
-}
connectedEdges :: Eq a => Graph a -> [a] -> [(a,a)]
connectedEdges (_,[]) _ = []
connectedEdges (_,_) [] = []
connectedEdges (v,e) (top:stack)
    | [x | x<-v, x==top] == [] = connectedEdges (v,e) stack
    | otherwise = [(x,y) | (x,y)<-e, y==top] ++ connectedEdges (v,e) stack

{-
    connectedComponent g n
    Takes a graph and a vertex
    RETURNS: the connected component (sub-graph which is connected to given vertex)
    EXAMPLES: connectedComponent (["a","b","c"],[("a","b"),("a","c"),("b","c")]) == [("a","b"),("a","c"),("b","c")]
-}
connectedComponent :: Eq a => Graph a -> a -> Graph a
connectedComponent (v,e) n = 
    let
        cv = connectedVertices (v,e) n
        ce = connectedEdges (v,e) cv
    in (cv, ce)