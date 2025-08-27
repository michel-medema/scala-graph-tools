package nl.rug.ds.graph.communities

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.communities.QualityOptimisation.Modularity

trait CommunityDetection[Vertex] {
	def communityStructure(g: Graph[Vertex]): (Modularity, Set[Set[Vertex]])

	protected def aggregateCommunities[T](g: Graph[Vertex], partitions: Map[Vertex, T]): Graph[T] = {
		val indices: Map[Vertex, Int] = g.V.zipWithIndex.toMap
		val edges = g.vertices.toList.flatMap( v =>
			g.A.getOrElse(v, Map.empty).toList
				.filter( e => partitions(v) == partitions(e._1) || indices(v) <= indices(e._1) )
				.map( e => (partitions(v), partitions(e._1), e._2) )
		)

		Graph.weighted( partitions.values, edges )
	}

	protected def groupCommunities(partitions: Map[Vertex, Vertex]): Set[Set[Vertex]] = {
		partitions.groupMapReduce( _._2 )( t => Set(t._1) )( _ union _ ).values.toSet
	}
}
