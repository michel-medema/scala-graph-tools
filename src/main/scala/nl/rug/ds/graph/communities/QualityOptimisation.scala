package nl.rug.ds.graph.communities

import nl.rug.ds.graph.communities.quality.Quality
import scala.annotation.tailrec

object QualityOptimisation {
	type Modularity = Double
}

class QualityOptimisation[Vertex, Community](val epsilon: Double) {
	def optimise(m: Quality[Vertex, Community]): (Map[Vertex, Community], Double) = {
		@tailrec
		def optimise(m: Quality[Vertex, Community], vs: List[Vertex], gain: Double): (Map[Vertex, Community], Double) = {
			val (m1: Quality[Vertex, Community], newGain: Double) = step(m, vs, gain)

			// Computing the difference between the new gain and the old gain may result in small floating point inaccuracies,
			// which may trigger an additional iteration. Adding epsilon to the gain ensures only addition is used with Doubles,
			// keeping the inaccuracies consistent between the two values.
			if ( newGain > epsilon + gain ) {
				optimise(m1, vs, newGain)
			} else {
				(m1.partitioning, newGain)
			}
		}

		optimise(m, m.getVertices, gain = 0.0)
	}

	@tailrec
	private def step(m: Quality[Vertex, Community], vs: List[Vertex], gain: Double): (Quality[Vertex, Community], Double) = {
		vs match {
			case Nil => (m, gain)
			case v :: tail =>
				m.gains(v).maxByOption( _._2 ).filter( _._2 > 0.0 ) match {
					case None => step(m, tail, gain)
					case Some((c: Community, q: Double)) =>
						step(
							m.moveToCommunity(v, c),
							tail,
							gain + q
						)
				}
		}
	}
}