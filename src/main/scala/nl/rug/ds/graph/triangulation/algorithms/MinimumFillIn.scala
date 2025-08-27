package nl.rug.ds.graph.triangulation.algorithms

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.common.Helper.pairs

import scala.annotation.tailrec

class MinimumFillIn[V]() extends EliminationHeuristic[V] {
  // Find the vertex with the smallest deficiency, which is equal to the number of edges that are missing between the neighbours of v in G.
  override def nextVertex(g: Graph[V]): Option[V] = {
    @tailrec
    def fillEdges( edges: Iterator[(V, V)], missing: Int, min: Int ): Int = {
      if ( edges.isEmpty ) {
        missing
      } else if ( missing >= min ) { // Whenever the deficiency is larger or equal to the deficiency of a previous vertex, this vertex will not have the smallest value.
        Int.MaxValue
      } else {
        val (u, v) = edges.next()

        if ( !g.neighbours( u ).contains( v ) ) {
          fillEdges( edges, missing + 1, min )
        } else {
          fillEdges( edges, missing, min )
        }
      }
    }

    @tailrec
    def minFill( vs: Set[V], smallestDeficiency: (Option[V], Int) ): Option[V] = {
      if ( vs.isEmpty ) {
        smallestDeficiency._1
      } else {
        val v: V = vs.head
        val numFillEdges: Int = fillEdges( pairs( g.neighbours(v) ), missing = 0, smallestDeficiency._2 )

        if ( numFillEdges == 0 ) { // The number of fill edges cannot be smaller than zero, so return without considering the remaining vertices.
          Some(v)
        } else {
          if ( numFillEdges < smallestDeficiency._2 ) {
            minFill( vs.tail, (Some(v), numFillEdges) )
          } else {
            minFill( vs.tail, smallestDeficiency )
          }
        }
      }
    }

    minFill( g.V, (None, Int.MaxValue) )
  }
}
