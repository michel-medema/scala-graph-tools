package nl.rug.ds.graph.common

import nl.rug.ds.common.UnitSpec

class GraphConnectivitySpec extends UnitSpec with GraphDefinitions {
  "The vertex connectivity" should "be infinite for adjacent vertices" in {
    GraphConnectivity.vertexConnectivity( k4, 1, 2 ) shouldEqual Int.MaxValue
  }

  it should "be zero for separated vertices" in {
    GraphConnectivity.vertexConnectivity( graphWithIsolatedVertex, 3, 4 ) shouldEqual 0
  }

  it should "be one for vertices connected through a single bridge" in {
    GraphConnectivity.vertexConnectivity( unweightedGraph, 2, 5 ) shouldEqual 1
  }

  it should "be equal to the number of vertex-disjoint paths" in {
    val g: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 4, 2 -> 6, 3 -> 5, 4 -> 6, 5 -> 6 ) )

    GraphConnectivity.vertexConnectivity( g, 1, 6 ) shouldEqual 3
  }

  it should "be four for a four-connected graph" in {
    val g: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 1 -> 5, 1 -> 6, 2 -> 3, 2 -> 4, 2 -> 6, 3 -> 4, 3 -> 5, 4 -> 5, 4 -> 6, 5 -> 6 ) )

    GraphConnectivity.vertexConnectivity( g, 1, 4 ) shouldEqual 4
  }

  "The maximal cliques of an empty graph" should "be an empty set" in {
    val g: Graph[Int] = Graph.empty[Int]

    GraphConnectivity.maximalCliques( g ) shouldEqual Set.empty
  }

  "The maximal cliques of a complete graph" should "contain a single clique with all the vertices of the graph" in {
    val g: Graph[Int] = Graph.clique( Set(1, 3, 5, 7, 9) )

    GraphConnectivity.maximalCliques( g ) shouldEqual Set( Set( 1, 3, 5, 7, 9 ) )
  }

  "The maximal cliques of a disconnected graph" should "contain the cliques of each connected component" in {
    val g: Graph[Int] = Graph.clique( Set( 0, 1, 2 ) ) ++ Graph.clique( Set( 5, 7 ) )

    GraphConnectivity.maximalCliques( g ) shouldEqual Set( Set(0, 1, 2), Set(5, 7) )
  }

  "The maximal cliques" should "contain all the maximal cliques" in {
   val g: Graph[Int] = Graph( List(1, 2, 3, 4), Set( 1 -> 2, 1 -> 4, 2 -> 3, 2 -> 4 ) )

   GraphConnectivity.maximalCliques( g ) shouldEqual Set( Set( 1, 2, 4 ), Set(2, 3) )
  }

  "The maximal cliques" should "include maximal cliques that have one or more variables in common" in {
    val g: Graph[Int] = Graph( (1 to 8).toList, Set( 1 -> 2, 1 -> 3, 2 -> 3, 2 -> 4, 2 -> 5, 3 -> 4, 3 -> 5, 4 -> 5, 4 -> 6, 4 -> 7, 5 -> 6, 5 -> 8, 6 -> 7, 6 -> 8, 7 -> 8 ) )

    GraphConnectivity.maximalCliques( g ) shouldEqual Set( Set(1, 2, 3), Set(2, 3, 4, 5), Set(4, 5, 6), Set(4, 6, 7), Set(5, 6, 8), Set(6, 7, 8) )
  }
}
