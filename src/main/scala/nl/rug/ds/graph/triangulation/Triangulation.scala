package nl.rug.ds.graph.triangulation

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.triangulation.algorithms.{EliminationGame, MinimumDegree, MinimumFillIn}

object Triangulation {
  def minFill[V]( g: Graph[V] ): EliminationGraph[V] = new EliminationGame( new MinimumFillIn() ).fill( g )

  def minDegree[V]( g: Graph[V] ): EliminationGraph[V] = new EliminationGame( new MinimumDegree() ).fill( g )
}
