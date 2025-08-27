package nl.rug.ds.graph.communities

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.communities
import nl.rug.ds.graph.communities.QualityOptimisation.Modularity
import nl.rug.ds.graph.communities.quality.UnipartiteModularity

import scala.annotation.tailrec

class Louvain[Vertex]( qualityOptimisation: QualityOptimisation[Vertex, Vertex] ) extends CommunityDetection[Vertex] {
  type Community = Vertex

  override def communityStructure(g: Graph[Vertex]): (Modularity, Set[Set[Vertex]]) = {
    // All the vertices start in their own community.
    val mapping: Map[Vertex, Community] = g.V.map( v => v -> v ).toMap

    step( quality.UnipartiteModularity(g, mapping), mapping )
  }

  @tailrec
  private def step(quality: UnipartiteModularity[Vertex, Community], mapping: Map[Vertex, Community]): (Modularity, Set[Set[Vertex]]) = {
    println(s"${quality.g.n} vertices, ${} edges, and ${quality.m} weight.")
    val (partitioning: Map[Vertex, Community], gain: Double) = qualityOptimisation.optimise( quality )

    val g: Graph[Vertex] = aggregateCommunities( quality.g, partitioning )
    val newMapping: Map[Vertex, Community] = mapping.view.mapValues( partitioning ).toMap
    val quality1: UnipartiteModularity[Vertex, Community] = communities.quality.UnipartiteModularity( g, g.V.map( v => v -> v).toMap )

    if ( gain > qualityOptimisation.epsilon ) {
      step( quality1, newMapping )
    } else {
      ( quality1.modularity(), groupCommunities(newMapping) )
    }
  }
}
