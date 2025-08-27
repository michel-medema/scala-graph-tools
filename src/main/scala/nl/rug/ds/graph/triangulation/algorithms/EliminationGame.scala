package nl.rug.ds.graph.triangulation.algorithms

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.triangulation.EliminationGraph

import scala.annotation.tailrec
import scala.collection.immutable.Queue


class EliminationGame[V](heuristic: EliminationHeuristic[V]) {
  // Returns the fill graph that results from the elimination of the vertices of G in the order specified by
  // the heuristic.
  def fill(g: Graph[V]): EliminationGraph[V] = {
    @tailrec
    def fill(eliminationGraph: Graph[V], ordering: Queue[V], fillIn: Queue[Set[(V, V)]]): EliminationGraph[V] = {
      heuristic.nextVertex( eliminationGraph ) match {
        case None =>
          new EliminationGraph( g, ordering.toList ) {
            override def fillEdges: Iterator[Set[(V, V)]] = fillIn.iterator
          }

        case Some(v :V) =>
          val (elimGraph: Graph[V], edges: Iterator[(V, V)]) = EliminationGraph.eliminate( eliminationGraph, v )

          fill( elimGraph, ordering.appended(v), fillIn.appended( edges.toSet ) )
      }
    }

    fill( g, Queue.empty, Queue.empty )
  }
}
