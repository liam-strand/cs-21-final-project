from grandalf.graphs import Vertex, Edge, Graph, graph_core
V = [Vertex(data) for data in range(10)]
X = [(0,1), (0,2), (1,3), (2,3), (4,0), (1,4), (4,5), (5,6), (3,6), (3,7), 
     (6,8), (7,8), (8,9), (5,9)]
E = [Edge(V[v], V[w]) for (v, w) in X]
g = Graph(V, E)
g.C
print()
