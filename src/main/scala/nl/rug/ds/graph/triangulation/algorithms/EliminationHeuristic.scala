package nl.rug.ds.graph.triangulation.algorithms

import nl.rug.ds.graph.common.Graph

trait EliminationHeuristic[V] {
  def nextVertex(g: Graph[V]): Option[V]
}
