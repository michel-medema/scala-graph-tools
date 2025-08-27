package nl.rug.ds.graph.communities

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.communities.quality.Quality
import org.scalamock.scalatest.MockFactory

class QualityOptimisationSpec extends UnitSpec with MockFactory {
	"Quality optimisation" should "move vertices to the community with largest gain" in {
		val m9: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m9.partitioning).expects().once()

		// Fourth and final iteration.
		val m8: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m8.gains _).expects(2).returns( List( 1 -> 0.03 ) ).once()
		(m8.moveToCommunity _).expects(2, 1).returns( m9 ).once()

		val m7: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m7.gains _).expects(1).returns( List( 1 -> 0.02 ) ).once()
		(m7.moveToCommunity _).expects(1, 1).returns( m8 ).once()

		// Third iteration.
		val m6: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m6.gains _).expects(2).returns( List( 2 -> 0.04 ) ).once()
		(m6.moveToCommunity _).expects(2, 2).returns( m7 ).once()

		val m5: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m5.gains _).expects(1).returns( List( 3 -> 0.02 ) ).once()
		(m5.moveToCommunity _).expects(1, 3).returns( m6 ).once()

		// Second iteration.
		val m4: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m4.gains _).expects(2).returns( List( 1 -> 0.15 ) ).once()
		(m4.moveToCommunity _).expects(2, 1).returns( m5 ).once()

		val m3: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m3.gains _).expects(1).returns( List( 1 -> 0.15 ) ).once()
		(m3.moveToCommunity _).expects(1, 1).returns( m4 ).once()

		// First iteration.
		val m2: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m2.gains _).expects(2).returns( List( 1 -> 0.1 ) ).once()
		(m2.moveToCommunity _).expects(2, 1).returns( m3 ).once()

		val m1: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m1.getVertices).expects().returning( List(1, 2) ).once()
		(m1.gains _).expects(1).returns( List( 1 -> 0.05, 2 -> 0.1 ) ).once()
		(m1.moveToCommunity _).expects(1, 2).returns( m2 ).once()

		val qualityOptimisation: QualityOptimisation[Int, Int] = new QualityOptimisation( epsilon = 0.05 )

		qualityOptimisation.optimise(m1)
	}

	it should "perform exactly one iteration if the gain does not exceed the threshold" in {
		val m3: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m3.partitioning).expects().once()

		val m2: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m2.gains _).expects(2).returns( List( 1 -> 0.01, 2 -> 0.02 ) ).once()
		(m2.moveToCommunity _).expects(2, 2).returns( m3 ).once()

		val m1: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m1.getVertices).expects().returning( List(1, 2) ).once()
		(m1.gains _).expects(1).returns( List( 1 -> 0.01, 2 -> 0.02, 3 -> 0.01 ) ).once()
		(m1.moveToCommunity _).expects(1, 2).returns( m2 ).once()

		val qualityOptimisation: QualityOptimisation[Int, Int] = new QualityOptimisation( epsilon = 0.05 )

		qualityOptimisation.optimise(m1)
	}

	it should "not move a vertex when there is no positive gain" in {
		val m1: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m1.getVertices).expects().returning( List(1, 2) ).once()
		(m1.gains _).expects(1).returns( List( 1 -> 0.0, 2 -> -0.1 ) ).once()
		(m1.gains _).expects(2).returns( List( 2 -> 0.0 ) ).once()
		(() => m1.partitioning).expects().once()

		val qualityOptimisation: QualityOptimisation[Int, Int] = new QualityOptimisation( epsilon = 0.0 )

		qualityOptimisation.optimise(m1)
	}

	it should "not move a vertex when there are no neighbouring communities" in {
		val m1: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m1.getVertices).expects().returning( List(1, 2) ).once()
		(m1.gains _).expects(1).returns( List.empty ).once()
		(m1.gains _).expects(2).returns( List.empty ).once()
		(() => m1.partitioning).expects().once()

		val qualityOptimisation: QualityOptimisation[Int, Int] = new QualityOptimisation( epsilon = 0.0 )

		qualityOptimisation.optimise(m1)
	}

	it should "select the first community in the list in case of ties" in {
		val m2: Quality[Int, Int] = mock[Quality[Int, Int]]
		(m2.gains _).expects(2).returns( List.empty ).once()
		(m2.gains _).expects(3).returns( List( 1 -> 0.0, 2 -> -0.25) ).once()
		(() => m2.partitioning).expects().once()

		val m1: Quality[Int, Int] = mock[Quality[Int, Int]]
		(() => m1.getVertices).expects().returning( List(1, 3, 2) ).once()
		(m1.gains _).expects(1).returns( List( 1 -> 0.2, 3 -> 0.05, 2 -> 0.2 ) ).once()
		(m1.moveToCommunity _).expects(1, 1).returns( m2 ).once()

		val qualityOptimisation: QualityOptimisation[Int, Int] = new QualityOptimisation( epsilon = 0.2 )

		qualityOptimisation.optimise(m1)
	}
}
