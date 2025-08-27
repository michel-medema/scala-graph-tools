package nl.rug.ds.graph.communities.quality

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.GraphDefinitions
import org.scalactic.Equality

class UnipartiteModularitySpec extends UnitSpec with GraphDefinitions {
	implicit val gainEq: Equality[(Community, Double)] = (a: (Community, Double), b: Any) => b match {
		case (c: Community, d: Double) => a._1 == c && a._2 === d +- 0.001
		case _ => false
	}

	val eps: Double = 0.001

	"A mapping that includes all vertices" should "not throw an exception" in {
		noException should be thrownBy UnipartiteModularity(graphWithIsolatedVertex, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4 ))
	}

	"A mapping that includes non-existing vertices" should "not throw an exception" in {
		noException should be thrownBy UnipartiteModularity(graphWithIsolatedVertex, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5-> 1 ))
	}

	"A mapping that does not include all vertices" should "throw an exception" in {
		an [NoSuchElementException] should be thrownBy UnipartiteModularity(graphWithIsolatedVertex, Map( 1 -> 1, 2 -> 2, 4 -> 4 ))
	}

	"Moving a vertex to a neighbouring community" should "update the partitioning" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))

		q.moveToCommunity(1, 2).partitioning shouldEqual Map( 1 -> 2, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 )
	}

	"Moving a vertex to a community with multiple members" should "update the partitioning" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 4, 5 -> 5, 6 -> 5 ))

		q.moveToCommunity(4, 5).partitioning shouldEqual Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 5, 5 -> 5, 6 -> 5 )
	}

	"Moving a vertex to a non-neighbouring community" should "update the partitioning" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))

		q.moveToCommunity(3, 5).partitioning shouldEqual Map( 1 -> 1, 2 -> 2, 3 -> 5, 4 -> 4, 5 -> 5, 6 -> 6 )
	}

	"Moving a vertex more than once" should "place it in the last community" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))

		q.moveToCommunity(1, 3).moveToCommunity(1, 2).partitioning shouldEqual Map( 1 -> 2, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 )
	}

	"Moving multiple vertices" should "move each vertex to the repsective communities" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))

		q.moveToCommunity(1, 2).moveToCommunity(3, 2).moveToCommunity(4, 5).partitioning shouldEqual Map( 1 -> 2, 2 -> 2, 3 -> 2, 4 -> 5, 5 -> 5, 6 -> 6 )
	}

	"Moving a vertex to its own community" should "not have an observable effect" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))
		val q1: UnipartiteModularity[Int, Int] = q.moveToCommunity(3, 3)

		q1.partitioning shouldEqual q.partitioning
		q1.modularity() shouldEqual q.modularity()
		q1.gains(3) shouldEqual q.gains(3)
	}

	"An isolated vertex" should "not have any neighbouring communities" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(graphWithIsolatedVertex, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4 ))

		q.gains(4) shouldEqual List.empty
	}

	"The modularity and gain" should "be computed correctly when all vertices belong to different community" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))
		val q1: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 2, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))
		val q2: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 3, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))

		val gain1: Double = q1.modularity() - q.modularity()
		val gain2: Double = q2.modularity() - q.modularity()

		q.modularity() shouldEqual -0.173 +- eps
		gain1 shouldEqual 0.102 +- eps
		gain2 shouldEqual 0.082 +- eps

		q.gains(1) should contain theSameElementsAs List( 2 -> gain1, 3 -> gain2 )
	}

	it should "be computed correctly when multiple neighbours belong to the same community" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 4, 5 -> 5, 6 -> 5 ))
		val q1: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 5, 6 -> 5 ))
		val q2: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 5, 5 -> 5, 6 -> 5 ))

		val gain1: Double = q1.modularity() - q.modularity()
		val gain2: Double = q2.modularity() - q.modularity()

		q.modularity() shouldEqual 0.194 +- eps
		gain1 shouldEqual -0.071 +- eps
		gain2 shouldEqual 0.163 +- eps

		q.gains(4) should contain theSameElementsAs List( 1 -> gain1, 5 -> gain2 )
	}

	it should "be computed correctly when a neighbour is in the same community as the vertex" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 4, 5 -> 4, 6 -> 4 ))
		val q1: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 4, 4 -> 4, 5 -> 4, 6 -> 4 ))

		val gain: Double = q1.modularity() - q.modularity()

		q.modularity() shouldEqual 0.357 +- eps
		gain shouldEqual -0.235 +- eps

		q.gains(3) should contain theSameElementsAs List( 1 -> 0.0, 4 -> gain )
	}

	it should "be computed correctly for a graph with self-loop and weighted edges" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(weightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 4 ))
		val q1: UnipartiteModularity[Int, Int] = UnipartiteModularity(weightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 3, 4 -> 4, 5 -> 4 ))
		val q2: UnipartiteModularity[Int, Int] = UnipartiteModularity(weightedGraph, Map( 1 -> 1, 2 -> 4, 3 -> 3, 4 -> 4, 5 -> 4 ))

		val gain1: Double = q1.modularity() - q.modularity()
		val gain2: Double = q2.modularity() - q.modularity()

		q.modularity() shouldEqual 0.218 +- eps
		gain1 shouldEqual -0.024	 +- eps
		gain2 shouldEqual -0.019 +- eps

		q.gains(2) should contain theSameElementsAs List( 1 -> gain1, 4 -> gain2 )
	}

	it should "be computed correctly after moving all vertices to the same community" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ))
		val q1: UnipartiteModularity[Int, Int] = UnipartiteModularity(unweightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 1 ))

		val q3: UnipartiteModularity[Int, Int] = q.moveToCommunity(2, 1)
			.moveToCommunity(5, 1)
			.moveToCommunity(4, 1)
			.moveToCommunity(6, 1)
			.moveToCommunity(3, 1)

		q3.modularity() shouldEqual q1.modularity() +- eps
		q3.gains(2) shouldEqual List( 1 -> 0.0 )
	}

	it should "be computed correctly for a weighted graph after moving vertices" in {
		val q: UnipartiteModularity[Int, Int] = UnipartiteModularity(weightedGraph, Map( 1 -> 1, 2 -> 1, 3 -> 3, 4 -> 4, 5 -> 5 ))
		val q1: UnipartiteModularity[Int, Int] = UnipartiteModularity(weightedGraph, Map( 1 -> 1, 2 -> 4, 3 -> 1, 4 -> 4, 5 -> 4 ))
		val q2: UnipartiteModularity[Int, Int] = UnipartiteModularity(weightedGraph, Map( 1 -> 1, 2 -> 4, 3 -> 4, 4 -> 4, 5 -> 4 ))
		val q3: UnipartiteModularity[Int, Int] = q.moveToCommunity(3, 1).moveToCommunity(2, 4).moveToCommunity(5, 4)

		val gain1: Double = q2.modularity() - q1.modularity()

		q3.modularity() shouldEqual q1.modularity() +- eps
		gain1 shouldEqual -0.231 +- eps
		q3.gains(3) should contain theSameElementsAs List( 1 -> 0.0, 4 -> gain1 )
	}
}
