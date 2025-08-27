package nl.rug.ds.graph.communities

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.{Graph, GraphDefinitions}
import nl.rug.ds.graph.communities.quality.UnipartiteModularity
import org.scalamock.scalatest.MockFactory

class LouvainSpec extends UnitSpec with GraphDefinitions with MockFactory {
  "Louvain" should "iteratively optimise the modularity until the gain falls below the threshold" in {
    val m1: QualityOptimisation[Int, Int] = mock[QualityOptimisation[Int, Int]]

    val mapping: Map[Int, Int] = Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 4, 5 -> 4, 6 -> 4 )

    inSequence {
      (m1.optimise _).expects( UnipartiteModularity( unweightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6 ) ) ).returns(mapping, 0.20).once()
      (m1.optimise _).expects( UnipartiteModularity( Graph.weighted( Set( (1, 1, 6.0), (1, 4, 1.0), (4, 4, 6.0) ) ), Map( 1 -> 1, 4 -> 4 ) ) ).returns(Map(1 -> 1, 4 -> 4), 0.0).once()
    }

    val louvain: Louvain[Int] = new Louvain(m1)
    louvain.communityStructure( unweightedGraph )._2 shouldEqual Set(Set(1, 2, 3), Set(4, 5, 6))
  }

  it should "correctly aggregate weighted graphs" in {
    val m1: QualityOptimisation[Int, Int] = mock[QualityOptimisation[Int, Int]]

    val mapping: Map[Int, Int] = Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 2, 5 -> 2 )

    val modularity1: UnipartiteModularity[Int, Int] = UnipartiteModularity( weightedGraph, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5 ) )
    val modularity2: UnipartiteModularity[Int, Int] = UnipartiteModularity( Graph.weighted( Set( (1, 1, 5.0), (1, 2, 2.0), (1, 3, 4.0), (2, 2, 21.0), (3, 2, 1.0) ) ), Map( 1 -> 1, 2 -> 2, 3 -> 3 ) )
    val modularity3: UnipartiteModularity[Int, Int] = UnipartiteModularity( Graph.weighted( Set( (1, 1, 13.0), (1, 2, 3.0), (2, 2, 21.0) ) ), Map( 1 -> 1, 2 -> 2 ) )

    inSequence {
      (m1.optimise _).expects(modularity1).returns(mapping, 0.20).once()
      (m1.optimise _).expects(modularity2).returns(Map(1 -> 1, 2 -> 2, 3 -> 1), 0.05).once()
      (m1.optimise _).expects(modularity3).returns(Map(1 -> 1, 2 -> 2), 0.0).once()
    }

    val louvain: Louvain[Int] = new Louvain(m1)
    louvain.communityStructure( weightedGraph )._2 shouldEqual Set(Set(1, 3), Set(2, 4, 5))
  }

  it should "not merge separate components" in {
    val m1: QualityOptimisation[Int, Int] = mock[QualityOptimisation[Int, Int]]

    val mapping: Map[Int, Int] = Map( 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 4 )

    val modularity1: UnipartiteModularity[Int, Int] = UnipartiteModularity( graphWithIsolatedVertex, Map( 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4 ) )
    val modularity2: UnipartiteModularity[Int, Int] = UnipartiteModularity( Graph.weighted( Set(1, 4), Set( (1, 1, 6.0) ) ), Map( 1 -> 1, 4 -> 4 ) )

    inSequence {
      (m1.optimise _).expects(modularity1).returns(mapping, 0.10).once()
      (m1.optimise _).expects(modularity2).returns(Map(1 -> 1, 4 -> 4), 0.0).once()
    }

    val louvain: Louvain[Int] = new Louvain(m1)
    louvain.communityStructure( graphWithIsolatedVertex )._2 shouldEqual Set(Set(1, 2, 3), Set(4))
  }
}
