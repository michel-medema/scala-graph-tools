package nl.rug.ds.graph.triangulation

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.triangulation.EliminationGraph.eliminate
import nl.rug.ds.common.Helper.pairs

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object EliminationGraph {
  def eliminate[V](g: Graph[V], v: V): (Graph[V], Iterator[(V, V)]) = {
    // The fill-edges that result from the elimination of v are all the edges between the neighbours of v that are
    // not yet in the graph.
    val fillEdges: Iterator[(V, V)] = pairs( g.neighbours( v ) ).filterNot { case (u, v) => g.neighbours( u ).contains( v ) }
    val (it1, it2) = fillEdges.duplicate

    val eliminationGraph: Graph[V] = g.addEdges( it1 ) \ v

    (eliminationGraph, it2)
  }
}

class EliminationGraph[V](override val g: Graph[V], override val eliminationOrdering: List[V]) extends TriangulatedGraph[V] {
  def fillEdges: Iterator[Set[(V, V)]] = {
    @tailrec
    def fill(g: Graph[V], ordering: List[V], edges: Queue[Set[(V, V)]]): Iterator[Set[(V, V)]] = {
      ordering match {
        case Nil => edges.iterator
        case (v: V) :: tail =>
          val (eliminationGraph: Graph[V], fillEdges: Iterator[(V, V)]) = eliminate(g, v)

          fill( eliminationGraph, tail, edges.appended( fillEdges.toSet ) )
      }
    }

    fill(g, eliminationOrdering, Queue.empty)
  }

  override val h: Graph[V] = g.addEdges( fillEdges.flatten )

  // TODO: Require all variables to be included in the elimination ordering.
  def isPerfect: Boolean = fillEdges.flatten.isEmpty
}