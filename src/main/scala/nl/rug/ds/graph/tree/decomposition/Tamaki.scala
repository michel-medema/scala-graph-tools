package nl.rug.ds.graph.tree.decomposition

import io.github.twalgor.decomposer.SemiPID
import nl.rug.ds.graph.common.Graph

class Tamaki {
  private def twalgor[V]( g: Graph[V] ): io.github.twalgor.common.TreeDecomposition = {
    // Convert the graph to the format used by Tamaki, which requires the vertices to be labeled from 0 to n - 1.
    val vertexMap: Map[V, Int] = g.vertices.zipWithIndex.toMap
    val graph = new io.github.twalgor.common.Graph( g.n )
    g.E.foreach( e => graph.addEdge( vertexMap( e._1 ), vertexMap( e._2 ) ) )

    // Compute a tree decomposition.
    SemiPID.decompose( graph )
  }

  def treeWidth[V]( g: Graph[V] ): Int = {
    val td: io.github.twalgor.common.TreeDecomposition = twalgor(g)

    if (td == null ) {
      0
    } else {
      td.width
    }
  }
}
