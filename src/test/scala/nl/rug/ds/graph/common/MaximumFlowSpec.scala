package nl.rug.ds.graph.common

import nl.rug.ds.common.UnitSpec

class MaximumFlowSpec extends UnitSpec {
  "Maximum flow" should "be equal to the weight of the edge between source and sink" in {
    val g: DiGraph[Int] = DiGraph.weighted( List( (1, 2, 12) ) )

    new MaximumFlow( g ).maxFlow( 1, 2 ) shouldEqual 12
  }

  it should "use all available paths" in {
    val g: DiGraph[String] = DiGraph.weighted( List( ("s", "u", 10), ("s", "v", 5), ("u", "v", 15), ("u", "t", 5), ("v", "t", 10) ) )

    new MaximumFlow( g ).maxFlow( "s", "t" ) shouldEqual 15
  }

  it should "use reverse flow" in {
    val g: DiGraph[String] = DiGraph.weighted( List( ("A", "B", 3), ("A", "D", 3), ("B", "C", 4), ("C", "A", 3), ("C", "D", 1), ("C", "E", 2), ("D", "E", 2), ("D", "F", 6), ("E", "B", 1), ("E", "G", 1), ("F", "G", 9) ) )

    new MaximumFlow( g ).maxFlow( "A", "G" ) shouldEqual 5
  }

  it should "not exceed the indegree of the sink" in {
    val g: DiGraph[Int] = DiGraph.weighted( List( (1, 2, 100), (1, 3, 100), (2, 3, 1), (2, 4, 100), (3, 4, 100) ) )

    new MaximumFlow( g ).maxFlow( 1, 4 ) shouldEqual 200
  }

  it should "send flow in only one direction" in {
    val g: DiGraph[Int] = DiGraph.weighted( List( (1, 2, 10), (1, 3, 20), (2, 4, 10), (2, 5, 50), (3, 4, 20), (4, 5, 10), (4, 2, 10) ) )

    new MaximumFlow( g ).maxFlow( 1, 4 ) shouldEqual 30
  }

  it should "equal zero when the sink cannot be reached" in {
    val g: DiGraph[Int] = DiGraph.weighted( List( (1, 2, 1.0), (1, 3, 1.0), (2, 3, 1.0), (4, 3, 1.0) ) )

    new MaximumFlow( g ).maxFlow( 2, 4 ) shouldEqual 0.0
  }
}
