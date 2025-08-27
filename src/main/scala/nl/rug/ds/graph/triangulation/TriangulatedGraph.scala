package nl.rug.ds.graph.triangulation

import nl.rug.ds.graph.common.Graph
import scala.annotation.tailrec

trait TriangulatedGraph[V] {
  val g: Graph[V]
  val h: Graph[V]
  val eliminationOrdering: List[V]

  // Based on the paper "Algorithms for Minimum Coloring, Maximum Clique, Minimum Covering by Cliques, and
  // Maximum Independent Set of a Chordal Graph" by Gavril.
  def maximalCliques: Vector[Set[V]] = {
    @tailrec
    def findCliques(h: Graph[V], order: List[V], cliques: Vector[Set[V]]): Vector[Set[V]] = {
      order match {
        case Nil => cliques
        case v :: tail => findCliques(h \ v, tail, cliques.appended( h.neighbours(v) + v ))
      }
    }
    // In the chordal graph, iterate over the vertices in the order of elimination.
    // For each vertex, take the adjacent vertices that appear after it in the order.
    // This set will be a clique in the graph.
    val cliques: Vector[Set[V]] = findCliques(h, eliminationOrdering, Vector.empty)

    // Filter out any set that is contained in a larger set to get the maximal cliques.
    cliques.filterNot( clique => cliques.exists( c => clique.subsetOf(c) && clique != c ) )
  }

  lazy val cliqueNumber: Int = maximalCliques.map( _.size ).maxOption.getOrElse(0)
}