package nl.rug.ds.graph.communities.quality

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.graph.communities.QualityOptimisation.Modularity

object UnipartiteModularity {
	def apply[Vertex, Community](g: Graph[Vertex], mapping: Vertex => Community): UnipartiteModularity[Vertex, Community] = {
		val partitioning: Map[Vertex, Community] =  g.V.map( v => v -> mapping(v) ).toMap
		val total: Map[Community, Double] = g.V.groupMapReduce( partitioning )( g.degree )( _ + _ )

		new UnipartiteModularity(g, partitioning, total)
	}
}

final case class UnipartiteModularity[Vertex, Community] private(
	g: Graph[Vertex],
	override val partitioning: Map[Vertex, Community],
	private val total: Map[Community, Double]
) extends Quality[Vertex, Community] {
	// The sum of the degrees of all the edges plus a small constant
	// to prevent division by zero.
	lazy val m: Double = (g.V.toList.map( g.degree ).sum / 2.0)  + 0.000000000001


	override def moveToCommunity(v: Vertex, c: Community): UnipartiteModularity[Vertex, Community] = {
		val b: Community = partitioning(v)
		val d: Double = g.degree(v)

		val newTotal: Map[Community, Double] = total.updated(b, total(b) - d)

		new UnipartiteModularity(
			g,
			partitioning.updated(v, c),
			newTotal.updated(c, newTotal(c) + d)
		)
	}

	def modularity(): Modularity = {
		val q: Double = partitioning.values.toList.distinct.map { (c: Community) =>
			val in: Double = g.V.toList.filter(v => partitioning(v) == c).flatMap(v => g.A(v).filter(e => partitioning(e._1) == c).values).sum

			in - ( ( total(c) * total(c) ) / ( 2.0 * m ) )
		}.sum

		q / ( 2.0 * m )
	}

	override def gains(v: Vertex): List[(Community, Double)] = {
		val weights: Map[Community, Double] = g.A(v).-(v).groupMapReduce( e => partitioning(e._1) )( _._2 )( _ + _ )

		val b: Community = partitioning(v)
		val kb: Double = weights.getOrElse( b, 0.0 )
		val d: Double = g.degree(v)
		val tb: Double = total(b) - d

		weights.toList.map {
			case (c: Community, kc: Double) =>
				val tc: Double = if ( b == c ) tb else total(c)

				// Decrease in modularity when vertex v is removed from current community.
				val loss: Double = ( ( 2.0 * d * tb ) / ( 4.0 * m * m ) ) - ( kb / m )

				// Gain in modularity when vertex v moves to community c.
				val gain: Double = ( kc / m ) - ( ( 2.0 * d * tc ) / ( 4.0 * m * m ) )

				c -> (loss + gain)
		}
	}

	override def getVertices: List[Vertex] = g.V.toList
}
