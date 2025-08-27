package nl.rug.ds.graph.common

import nl.rug.ds.common.UnitSpec

class DiGraphSpec extends UnitSpec with GraphDefinitions {
  val digraphWithIsolatedVertex: DiGraph[Int] = DiGraph.weighted( Set(4), Set( (1, 2, 2.0), (1, 3, 3.0), (2, 3, 4.0) ) )
  val weightedDigraph: DiGraph[Int] = DiGraph.weighted( Set( (1, 2, 1.0), (1, 3, 2.50), (2, 3, 5.0), (3, 1, 4.0), (3, 4, 1.0), (4, 1, 7.50) ) )
  val digraphWithSelfLoops: DiGraph[Int] = DiGraph.weighted( Set( (1, 2, 2.0), (2, 3, 1.0), (3, 4, 8.0), (4, 1, 3.0), (4, 4, 15.0) ) )
  val dag: DiGraph[Int] = DiGraph.weighted( Set( (1, 2, 2.0), (1, 3, 1.0), (2, 4, 8.0), (3, 5, 3.0), (4, 6, 15.0), (5, 6, 1.0) ) )

  "An empty directed graph" should "not have any vertices or edges" in {
    DiGraph.empty[Int].n shouldEqual 0
    //DiGraph.empty[Int].m shouldEqual 0
    DiGraph.empty[Int].V shouldEqual Set.empty
  }

  "A weighted directed graph" should "include each edge with the correct weight" in {
    weightedDigraph.n shouldEqual 4
    //g.m shouldEqual 6
    weightedDigraph.V shouldEqual Set(1, 2, 3, 4)
    weightedDigraph.weight(1, 2) shouldEqual 1.0
    weightedDigraph.weight(1, 3) shouldEqual 2.50
    weightedDigraph.weight(2, 3) shouldEqual 5.0
    weightedDigraph.weight(3, 1) shouldEqual 4.0
    weightedDigraph.weight(3, 4) shouldEqual 1.0
    weightedDigraph.weight(4, 1) shouldEqual 7.50
  }

  it should "include vertices without incident edges" in {
    digraphWithIsolatedVertex.n shouldEqual 4
    //g.m shouldEqual 3
    digraphWithIsolatedVertex.V shouldEqual Set(1, 2, 3, 4)
    digraphWithIsolatedVertex.weight(1, 2) shouldEqual 2.0
    digraphWithIsolatedVertex.weight(1, 3) shouldEqual 3.0
    digraphWithIsolatedVertex.weight(2, 3) shouldEqual 4.0
  }

  it should "sum the weights of repeated edges" in {
    val g: DiGraph[Int] = DiGraph.weighted( Set( (1, 2, 2.0), (1, 4, 6.0), (2, 3, 3.0), (3, 4, 4.0), (4, 1, 1.0), (1, 4, 1.0)  ) )

    g.n shouldEqual 4
    //g.m shouldEqual 5
    g.V shouldEqual Set(1, 2, 3, 4)
    g.weight(1, 2) shouldEqual 2.0
    g.weight(1, 4) shouldEqual 7.0
    g.weight(2, 3) shouldEqual 3.0
    g.weight(3, 4) shouldEqual 4.0
    g.weight(4, 1) shouldEqual 1.0
  }

  "The neighbours of a vertex" should "include all the endpoints of outgoing edges" in {
    weightedDigraph.neighbours(1) shouldEqual Set(2, 3)
    weightedDigraph.neighbours(3) shouldEqual Set(1, 4)
  }

  it should "be empty for a vertex without outgoing edges" in {
    digraphWithIsolatedVertex.neighbours(4) shouldEqual Set.empty
    dag.neighbours(6) shouldEqual Set.empty
  }

  it should "empty for a non-existing vertex" in {
    weightedDigraph.neighbours(0) shouldEqual Set.empty
  }

  it should "include the vertex if it has a self-loop" in {
    digraphWithSelfLoops.neighbours(4) shouldEqual Set(1, 4)
  }

  "The neighbours of a set of vertices" should "include all endpoints of outgoing edges that are not in the set" in {
    weightedDigraph.neighbours( Set(1, 2) ) shouldEqual Set(3)
    digraphWithIsolatedVertex.neighbours( Set(1, 3, 4) ) shouldEqual Set(2)
  }

  it should "not include the vertex itself if it has a self-loop" in {
    weightedDigraph.neighbours( Set(4) ) shouldEqual Set(1)
  }

  "Adding vertices to a graph" should "include the vertex without any incident edges" in {
    weightedDigraph.addVertices( Set(0) ) shouldEqual DiGraph.weighted( Set(0, 1, 2, 3, 4), Set( (1, 2, 1.0), (1, 3, 2.50), (2, 3, 5.0), (3, 1, 4.0), (3, 4, 1.0), (4, 1, 7.50) ) )
  }

  "Adding existing vertices" should "not change the graph" in {
    weightedDigraph.addVertices( Set(1, 2, 3) ) shouldEqual weightedDigraph
  }

  "Adding an empty set of vertices" should "not change the graph" in {
    weightedDigraph.addVertices( Set.empty ) shouldEqual weightedDigraph
  }

  "Adding vertices more than once" should "add each vertex exactly once" in {
    digraphWithIsolatedVertex.addVertices( List(10, 11, 11, 12) ) shouldEqual DiGraph.weighted( Set(4, 10, 11, 12), Set( (1, 2, 2.0), (1, 3, 3.0), (2, 3, 4.0) ) )
  }

  "Removing vertices from the graph" should "delete the vertices and all incident edges" in {
    dag \ Set(1, 6) shouldEqual DiGraph.weighted( Set( (2, 4, 8.0), (3, 5, 3.0) ) )
  }

  "Removing non-existing vertices" should "not change the graph" in {
    weightedDigraph \ Set(10, 11, 12) shouldEqual weightedDigraph
  }

  "Removing a single vertex" should "remove the vertex and all its incident edges" in {
    digraphWithSelfLoops \ 4 shouldEqual DiGraph.weighted( Set( (1, 2, 2.0), (2, 3, 1.0) ) )
  }

  "Adding edges to a graph" should "include the new edges" in {
    dag.addEdges( List( (6, 5, 2.0), (6, 4, 8.50) ) ) shouldEqual DiGraph.weighted( Set( (1, 2, 2.0), (1, 3, 1.0), (2, 4, 8.0), (3, 5, 3.0), (4, 6, 15.0), (5, 6, 1.0), (6, 5, 2.0), (6, 4, 8.50) ) )
  }

  "Adding existing edges" should "add the weight to the existing edge" in {
    dag.addEdges( List( (1, 2, 3.0), (1, 3, 4.0) ) ) shouldEqual DiGraph.weighted( Set( (1, 2, 5.0), (1, 3, 5.0), (2, 4, 8.0), (3, 5, 3.0), (4, 6, 15.0), (5, 6, 1.0) ) )
  }

  "Adding an edge between a vertex and itself" should "create a self-loop" in {
    digraphWithSelfLoops.addEdges( List( (3, 3, 0.50) ) ) shouldEqual DiGraph.weighted( Set( (1, 2, 2.0), (2, 3, 1.0), (3, 3, 0.50), (3, 4, 8.0), (4, 1, 3.0), (4, 4, 15.0) ) )
  }

  "Updating an existing edge" should "set the weight to the new value" in {
    weightedDigraph.updateEdge(1, 3, 6.0) shouldEqual DiGraph.weighted( Set( (1, 2, 1.0), (1, 3, 6.0), (2, 3, 5.0), (3, 1, 4.0), (3, 4, 1.0), (4, 1, 7.50) ) )
  }

  "Updating a non-existing edge" should "add the edge to the graph" in {
    digraphWithIsolatedVertex.updateEdge(2, 4, 1.0) shouldEqual DiGraph.weighted( Set( (1, 2, 2.0), (1, 3, 3.0), (2, 3, 4.0), (2, 4, 1.0) ) )
  }

  "An induced subgraph" should "only include edges between members of the set" in {
    weightedDigraph.subgraph( Set(1, 2, 3) ) shouldEqual DiGraph.weighted( Set( (1, 2, 1.0), (1, 3, 2.50), (2, 3, 5.0), (3, 1, 4.0) ) )
  }

  it should "preserve self-loops" in {
    digraphWithSelfLoops.subgraph( Set(1, 3, 4) ) shouldEqual DiGraph.weighted( Set( (3, 4, 8.0), (4, 1, 3.0), (4, 4, 15.0) ) )
  }

  it should "include vertices without incident edges" in {
    digraphWithIsolatedVertex.subgraph( Set(2, 4) ) shouldEqual DiGraph.weighted( Set(2, 4), Set.empty )
  }

  "A graph" should "be equal to itself" in {
    dag shouldEqual dag
    DiGraph.weighted( Set( (1, 2, 2.0), (1, 3, 1.0), (2, 4, 8.0), (3, 5, 3.0), (4, 6, 15.0), (5, 6, 1.0) ) ) shouldEqual
      DiGraph.weighted( Set( (1, 2, 2.0), (1, 3, 1.0), (2, 4, 8.0), (3, 5, 3.0), (4, 6, 15.0), (5, 6, 1.0) ) )
  }

  "Graphs with identical edges but different weights" should "not be equal" in {
    DiGraph.weighted( Set( (1, 2, 1.0), (1, 3, 1.0), (2, 3, 1.0) ) ) should !== (DiGraph.weighted( Set( (1, 2, 1.0), (1, 3, 1.0), (2, 3, 2.0) ) ))
  }

  "Graphs with different vertex sets" should "not be equal" in {
    DiGraph.weighted( Set( 1, 2, 3, 4 ), Set.empty ) should !==( DiGraph.weighted( Set( 1, 2, 4 ), Set.empty ) )
  }
}
