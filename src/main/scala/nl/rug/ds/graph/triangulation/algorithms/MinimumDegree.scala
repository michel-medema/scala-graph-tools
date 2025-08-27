package nl.rug.ds.graph.triangulation.algorithms

import nl.rug.ds.graph.common.Graph

class MinimumDegree[V]() extends EliminationHeuristic[V] {
  override def nextVertex(g: Graph[V]): Option[V] = g.vertices.minByOption( v => g.neighbours(v).size )
}
