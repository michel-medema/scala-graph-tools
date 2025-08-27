package nl.rug.ds.graph.triangulation

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.{Graph, GraphDefinitions}
import nl.rug.ds.graph.triangulation.algorithms.MinimumFillIn

class MinimumFillInSpec extends UnitSpec with GraphDefinitions {
  "All vertices in a complete graph" should "have a deficiency of zero" in {
    new MinimumFillIn().nextVertex( k4 ) should contain oneOf (1, 2, 3, 4)
  }

  "All vertices in a cycle" should "have a deficiency of one" in {
    new MinimumFillIn().nextVertex( cycleGraph ) should contain oneOf (1, 2, 3, 4, 5)
  }

  "The vertex with the smallest deficiency" should "be selected" in {
    val g: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 6, 3 -> 7, 4 -> 6, 4 -> 7, 5 -> 6, 5 -> 7 ) )

    new MinimumFillIn().nextVertex( g ) shouldEqual Some(1)
  }

  "The vertices with the smallest deficiency in a chordal graph" should "be simplicial vertices" in {
    new MinimumFillIn().nextVertex( chordalGraph1 ) should contain oneOf (2, 3)
  }

  "A graph with exactly one vertex" should "have only one candidate" in {
    new MinimumFillIn().nextVertex( Graph( List(4), List.empty ) ) shouldEqual Some(4)
  }

  "The vertex with the smallest deficiency in an empty graph" should "be non-existent" in {
    new MinimumFillIn().nextVertex( Graph.empty ) shouldEqual None
  }
}
