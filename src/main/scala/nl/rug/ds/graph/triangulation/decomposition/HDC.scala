package nl.rug.ds.graph.triangulation.decomposition

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.communities.QualityOptimisation
import nl.rug.ds.graph.communities.QualityOptimisation.Modularity
import nl.rug.ds.graph.communities.Louvain

import scala.annotation.tailrec

// Heuristic Decomposition using Community Structure.
class HDC {
  /**
   * Return the subgraphs that are obtained when the vertices of the given separator
   * are removed. The vertices of the separator are turned into a clique to ensure
   * the treewidth can be computed correctly.
   */
  private def split[V]( g: Graph[V], separator: Set[V] ): Set[Graph[V]] = {
    if ( separator.subsetOf( g.V ) ) {
      val g1 = g ++ Graph.clique( separator )
      g.components( removed = separator ).map( c => g1.subgraph( c._1 ++ separator ) )
    } else {
      Set( g )
    }
  }

  /**
   * Find the community for which the neighbourhood is smallest. The removal of this neighbourhood
   * from the graph splits the community into a separate component.
   */
  private def smallestSeparator[V]( g: Graph[V], communities: List[Set[V]] ): Set[V] = {
    communities.flatMap { C => g.components( removed = C ).map( _._2 ) }.minBy( _.size )
  }

  @tailrec
  private def processGraphs[V]( remaining: List[(Graph[V], List[Set[V]])], processed: Set[Graph[V]] ): Set[Graph[V]] = {
    remaining match {
      case Nil => processed
      case (g: Graph[V], communities: List[Set[V]]) :: tail =>
        if ( communities.size < 2 ) {
          processGraphs( tail, processed + g )
        } else {
          val separator: Set[V] = smallestSeparator( g, communities )

          val subgraphs: List[(Graph[V], List[Set[V]])] = split( g, separator ).toList.map( g1 => {
            val vertices: Set[V] = g1.V
            val remainingCommunities: List[Set[V]] = communities.filter( C => C.subsetOf( vertices ) && C != separator )

            (g1, remainingCommunities)
          })

          processGraphs( tail ++ subgraphs, processed )
        }
    }
  }

  /**
   * Decompose the given graph G by using the boundaries of the communities as vertex separators.
   */
  def decompose[V]( g: Graph[V] ): (Set[Graph[V]], Modularity) = {
    val (modularity: Modularity, communities: Set[Set[V]]) = new Louvain[V]( new QualityOptimisation[V, V]( epsilon = 0.01 ) ).communityStructure( g )

    ( processGraphs( List( (g, communities.toList) ), Set.empty ), modularity )
  }
}
