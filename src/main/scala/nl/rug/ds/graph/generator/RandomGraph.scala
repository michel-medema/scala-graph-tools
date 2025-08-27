package nl.rug.ds.graph.generator

import nl.rug.ds.graph.common.Graph
import scala.collection.immutable.Queue

object RandomGraph extends GraphGenerator {
  def generate[V]( vs: Set[V], avgDegree: Int ): Graph[V] = {
    // Create a random spanning tree between the vertices.
    val tree: List[List[V]] = combine( vs.toList.map( v => List( v ) ), 2, Queue.empty )

    val E1: Set[(V, V)] = tree.map { case List(u, v) => (u, v) }.toSet

    // Calculate the number of edges that still have to be generated (the total number of edges minus the number
    // of edges that have been introduced by the spanning tree).
    val m: Int = math.floor(vs.size * (avgDegree.toDouble / 2.0)).toInt

    // Add random edges between the vertices of the spanning tree.
    val E2: Set[(V, V)] = randomEdges( vs.toVector, m, E1 )

    Graph( vs, E2 )
  }
}
