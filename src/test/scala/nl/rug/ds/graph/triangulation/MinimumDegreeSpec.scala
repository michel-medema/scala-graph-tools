package nl.rug.ds.graph.triangulation

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.{Graph, GraphDefinitions}
import nl.rug.ds.graph.triangulation.algorithms.MinimumDegree

class MinimumDegreeSpec extends UnitSpec with GraphDefinitions {
  "An empty graph" should "not have a vertex with minimum degree" in {
    new MinimumDegree().nextVertex( Graph.empty ) shouldEqual None
  }

  "The vertices of a complete graph" should "all have the same degree" in {
    new MinimumDegree().nextVertex( k4 ) should contain oneOf (1, 2, 3, 4)
  }

  "A graph with a single vertex" should "have a unique minimum-degree vertex" in {
    new MinimumDegree().nextVertex( Graph( Set(1), Set.empty ) ) shouldEqual Some(1)
  }

  "A single isolated vertex" should "be the minimum-degree vertex" in {
    new MinimumDegree().nextVertex( graphWithIsolatedVertex ) shouldEqual Some(4)
  }

  "The vertex with the smallest degree" should "be selected" in {
    new MinimumDegree().nextVertex( Graph( Set( 1 -> 2, 1 -> 4, 2 -> 3, 2 -> 4 ) ) ) shouldEqual Some(3)
  }
}
