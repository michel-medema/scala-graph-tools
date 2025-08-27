package nl.rug.ds.graph.common

import nl.rug.ds.common.UnitSpec
import org.scalactic.Equality

class GraphSpec extends UnitSpec with GraphDefinitions {
  implicit val edgeEq: Equality[(Int, Int)] = (a: (Int, Int), b: Any) => b match {
    case (u: Int, v: Int) => (a._1 == u && a._2 == v) || (a._1 == v && a._2 == u)
    case _ => false
  }

  "An empty graph" should "not have any vertices or edges" in {
    Graph.empty[Int].n shouldEqual 0
    Graph.empty[Int].m shouldEqual 0
    Graph.empty[Int].V shouldEqual Set.empty[Int]
    Graph.empty[Int].E shouldBe empty
  }

  "A clique on an empty set of vertices" should "be empty" in {
    Graph.clique( Set.empty[Int] ) shouldEqual Graph.empty[Int]
  }

  "A clique" should "include all vertices and all possible edges" in {
    val g: Graph[Int] = Graph.clique( Set( 1, 2, 3, 4 ) )

    g.n shouldEqual 4
    g.m shouldEqual 6
    g.V shouldEqual Set(1, 2, 3, 4)
    g.E.to(LazyList) should contain theSameElementsAs Set( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 3, 2 -> 4, 3 -> 4 )
  }

  "A graph" should "include all vertices and edge" in {
    unweightedGraph.n shouldEqual 6
    unweightedGraph.m shouldEqual 7
    unweightedGraph.V shouldEqual Set(1, 2, 3, 4, 5, 6)
    unweightedGraph.E.to(LazyList) should contain theSameElementsAs Set(1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 5, 4 -> 6, 5 -> 6)
  }

  "A vertex without edges" should "be included in the graph" in {
    graphWithIsolatedVertex.n shouldEqual 4
    graphWithIsolatedVertex.m shouldEqual 3
    graphWithIsolatedVertex.V shouldEqual Set(1, 2, 3, 4)
    graphWithIsolatedVertex.E.to(LazyList) should contain theSameElementsAs Set(1 -> 2, 1 -> 3, 2 -> 3)
  }

  "Repeated vertices and edges" should "be ignored" in {
    val g: Graph[Int] = Graph( List(1, 2, 3, 4, 1, 1), Set( 1 -> 2, 1 -> 3, 1 -> 4, 4 -> 1, 2 -> 3, 3 -> 2, 3 -> 4 ) )

    g.n shouldEqual 4
    g.m shouldEqual 5
    g.V shouldEqual Set(1, 2, 3, 4)
    g.E.to(LazyList) should contain theSameElementsAs Set( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 3, 3 -> 4 )
  }

  "The edges" should "have the correct weight" in {
    val g: Graph[Int] = Graph.weighted( Set( (1, 2, 1.0), (1, 3, 2.50), (1, 4, 5.0), (2, 3, 1.0), (3, 4, 10.0) ) )

    g.n shouldEqual 4
    g.m shouldEqual 5
    g.V shouldEqual Set(1, 2, 3, 4)
    g.E.to(LazyList) should contain theSameElementsAs Set( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 3, 3 -> 4 )
    g.weight(1, 2) shouldEqual 1.0
    g.weight(1, 3) shouldEqual 2.50
    g.weight(1, 4) shouldEqual 5.0
    g.weight(2, 3) shouldEqual 1.0
    g.weight(3, 4) shouldEqual 10.0
  }

  "Repeating edges with weights" should "sum the weights" in {
    val g: Graph[Int] = Graph.weighted( List(1, 2, 3, 4), List( (1, 2, 1.0), (2, 1, 1.0), (1, 3, 2.0), (2, 3, 3.0), (2, 3, 2.0) ) )

    g.n shouldEqual 4
    g.m shouldEqual 3
    g.V shouldEqual Set(1, 2, 3, 4)
    g.E.to(LazyList) should contain theSameElementsAs Set( 1 -> 2, 1 -> 3, 2 -> 3 )
    g.weight(1, 2) shouldEqual 2.0
    g.weight(1, 3) shouldEqual 2.0
    g.weight(2, 3) shouldEqual 5.0
  }

  "An edge with negative weight" should "be allowed" in {
    val g: Graph[Int] = Graph.weighted( List(1, 2), List( (1, 2, -7.0) ) )

    g.n shouldEqual 2
    g.m shouldEqual 1
    g.V shouldEqual Set(1, 2)
    g.E.to(LazyList) should contain theSameElementsAs Set( 1 -> 2 )
    g.weight(1, 2) shouldEqual -7.0
  }

  "The weight of a non-existing edge" should "be equal to zero" in {
    graphWithIsolatedVertex.weight(1, 4) shouldEqual 0.0
  }

  "The vertices of a graph" should "be returned in the order in which they were inserted" in {
    Graph( List(2, 5, 7, 1), List( 2 -> 5, 5 -> 7, 7 -> 1 ) ).vertices.to(LazyList) should contain theSameElementsInOrderAs List(2, 5, 7, 1)
  }

  "The order of vertices that are not in the vertex list" should "be determined by the order of the edges" in {
    Graph( List( 7 ), List( 1 -> 7, 3 -> 4, 3 -> 1, 3 -> 7, 5 -> 2, 4 -> 2 ) ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 7, 1, 3, 4, 5, 2 )
  }

  "The order of the vertices" should "not be affected by repeating vertices" in {
    Graph( List( 3, 6, 1, 3, 5, 1, 7 ), List.empty ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 3, 6, 1, 5, 7 )
  }

  "Additional vertices" should "be added to the end of the ordering" in {
    Graph( List(3, 9, 7, 11, 1), Set.empty ).addVertices( List(3, 2, 4, 5) ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 3, 9, 7, 11, 1, 2, 4, 5 )
  }

  "Adding edges to a graph" should "put new vertices at the end of the ordering" in {
    Graph( List( 3, 9, 7, 11, 1 ), Set.empty ).addEdges( List( 1 -> 3, 7 -> 8, 2 -> 5, 4 -> 3 ) ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 3, 9, 7, 11, 1, 8, 2, 5, 4 )
  }

  "Removing edges" should "not alter the vertex order" in {
    Graph( List( 7, 3, 2 ), Set( 2 -> 3, 3 -> 7, 7 -> 2) ).--( Set( 2 -> 3, 3 -> 7 ) ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 7, 3, 2 )
  }

  "Removing vertices" should "maintain the relative order" in {
    Graph( List( 3, 5, 1, 6, 7, 2 ), Set.empty).\( 1 ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 3, 5, 6, 7, 2 )
    Graph( List( 3, 5, 1, 6, 7, 2 ), Set.empty).\( Set( 5, 6, 7 ) ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 3, 1, 2 )
    Graph( List( 3, 5, 1, 6, 7, 2 ), Set.empty).subgraph( Set( 5, 6, 7 ) ).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 5, 6, 7 )
  }

  "Combining two graphs" should "append the vertices of the second graph to the vertices of the first graph" in {
    (Graph( List(10, 100, 1000), Set.empty ) ++ Graph( List(2, 5, 9), Set.empty )).vertices.to( LazyList ) should contain theSameElementsInOrderAs List( 10, 100, 1000, 2, 5, 9 )
  }

  "The neighbours of a vertex" should "include all adjacent vertices" in {
    unweightedGraph.neighbours( 1 ) shouldEqual Set(2, 3)
  }

  "A vertex with a self-loop" should "be its own neighbour" in {
    weightedGraph.neighbours( 2 ) shouldEqual Set(1, 2, 4, 5)
  }

  "A vertex without any incident edges" should "not have any neighbours" in {
    graphWithIsolatedVertex.neighbours( 4 ) shouldEqual Set.empty
  }

  "The neighbours of a set of vertices" should "include vertices adjacent to a member of the set that are not in the set" in {
    unweightedGraph.neighbours( Set(1, 2) ) shouldEqual Set(3)
  }

  "The neighbours of a set of vertices" should "not include a member of the set with a self-loop" in {
    weightedGraph.neighbours( Set(4, 5) ) shouldEqual Set(2, 3)
  }

  "The neighbours of a set with a single vertex" should "be equal to the neighbours of that vertex" in {
    unweightedGraph.neighbours( Set(1) ) shouldEqual unweightedGraph.neighbours( 1 )
    graphWithIsolatedVertex.neighbours( Set(4) ) shouldEqual graphWithIsolatedVertex.neighbours( 4 )
  }

  "The neighbours of a set with a single vertex" should "include all the neighbours of that vertex except the vertex itself" in {
    weightedGraph.neighbours( Set(2) ) shouldEqual Set(1, 4, 5)
  }

  "The degree of a vertex in an unweighted graph" should "be equal to the number of neighbours" in {
    unweightedGraph.degree( 4 ) shouldEqual 3
  }

  it should "include the vertex itself if it has a self-loop" in {
    unweightedGraphWithSelfLoop.degree( 1 ) shouldEqual 4
  }

  it should "be zero for a vertex without neighbours" in {
    graphWithIsolatedVertex.degree( 4 ) shouldEqual 0
  }

  "The degree of a vertex in a weighted graph" should "be equal to the sum of the weights" in {
    weightedGraph.degree( 5 ) shouldEqual 5.0
  }

  it should "include the weight of self-loops" in {
    weightedGraph.degree( 2 ) shouldEqual 9.0
  }

  "An induced subgraph" should "only include edges between members of the set" in {
    unweightedGraph.subgraph( Set(4, 5, 6) ) shouldEqual Graph( Set( 4 -> 5, 4 -> 6, 5 -> 6 ) )
  }

  it should "preserve self-loops" in {
    unweightedGraphWithSelfLoop.subgraph( Set(1, 2, 3) ) shouldEqual Graph( Set( 1 -> 1, 1 -> 2, 1 -> 3, 2 -> 3 ) )
  }

  it should "include vertices without incident edges" in {
    graphWithIsolatedVertex.subgraph( Set(3, 4) ) shouldEqual Graph( Set(3, 4), Set.empty )
  }

  it should "preserve the weights of the edges" in {
    weightedGraph.subgraph( Set(2, 4, 5) ) shouldEqual Graph.weighted( Set( (2, 2, 4.0), (2, 4, 2.0), (2, 5, 1.0), (4, 5, 3.0), (4, 4, 5.0) ) )
  }

  "Adding vertices to a graph" should "include the vertex without any incident edges" in {
    unweightedGraph.addVertices( Set(10) ) shouldEqual Graph( Set(1, 2, 3, 4, 5, 6, 10), Set( 1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 5, 4 -> 6, 5 -> 6 ) )
  }

  "Adding existing vertices" should "not change the graph" in {
    unweightedGraph.addVertices( Set(1, 2, 3) ) shouldEqual unweightedGraph
  }

  "Adding an empty set of vertices" should "not change the graph" in {
    weightedGraph.addVertices( Set.empty ) shouldEqual weightedGraph
  }

  "Adding vertices more than once" should "add each vertex exactly once" in {
    graphWithIsolatedVertex.addVertices( List(4, 4, 5, 5) ) shouldEqual Graph( List(1, 2, 3, 4, 5), List(1 -> 2, 1 -> 3, 2 -> 3) )
  }

  "Removing vertices from the graph" should "delete the vertices and all incident edges" in {
    unweightedGraph \ Set(3, 4) shouldEqual Graph( List( 1 -> 2, 5 -> 6 ) )
  }

  "Removing non-existing vertices" should "not change the graph" in {
    graphWithIsolatedVertex \ Set(5, 6, 7) shouldEqual graphWithIsolatedVertex
  }

  "Removing a single vertex" should "remove the vertex and all its incident edges" in {
    unweightedGraphWithSelfLoop \ 1 shouldEqual Graph( List( 2 -> 3, 3 -> 4 ) )
  }

  "Adding edges to a graph" should "include the new edges" in {
    unweightedGraph.addEdges( List( 1 -> 5, 2 -> 6 ) ) shouldEqual Graph( List( 1 -> 2, 1 -> 3, 1 -> 5, 2 -> 3, 2 -> 6, 3 -> 4, 4 -> 5, 4 -> 6, 5 -> 6 ) )
  }

  "Adding existing edges" should "not change the graph" in {
    graphWithIsolatedVertex.addEdges( List( 1 -> 2, 2 -> 3 ) ) shouldEqual graphWithIsolatedVertex
  }

  "Adding an edge between a vertex and itself" should "create a self-loop" in {
    weightedGraph.addEdges( List( 3 -> 3 ) ) shouldEqual Graph.weighted( List( (1, 1, 5.0), (1, 2, 2.0), (1, 3, 4.0), (2, 2, 4.0), (2, 4, 2.0), (2, 5, 1.0), (3, 3, 1.0), (3, 5, 1.0), (4, 5, 3.0), (4, 4, 5.0) ) )
  }

  "Adding an existing edge in a weighted graph" should "not change the graph" in {
    weightedGraph.addEdges( List( 1 -> 3 ) ) shouldEqual weightedGraph
  }

  "Removing edges" should "remove the edges from the graph" in {
    unweightedGraphWithSelfLoop -- Set( 1 -> 1, 2 -> 1, 3 -> 4 ) shouldEqual Graph( List( 1 -> 3, 1 -> 4, 2 -> 3 ) )
  }

  "Removing non-existing edges" should "not change the graph" in {
    graphWithIsolatedVertex -- Set( 1 -> 4, 2 -> 4 ) shouldEqual graphWithIsolatedVertex
  }

  "Removing all the incident edges of a vertex" should "isolate the vertex" in {
    weightedGraph -- Set( 1 -> 1, 2 -> 5, 3 -> 5, 4 -> 5) shouldEqual Graph.weighted( Set(1, 2, 3, 4, 5), List( (1, 2, 2.0), (1, 3, 4.0), (2, 2, 4.0), (2, 4, 2.0), (4, 4, 5.0) ) )
  }

  "Combining disjoint graphs" should "result in a graph with two components" in {
    unweightedGraph ++ Graph( Set( 10 -> 11, 10 -> 12, 11 -> 12 ) ) shouldEqual Graph( Set( 1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 5, 4 -> 6, 5 -> 6, 10 -> 11, 10 -> 12, 11 -> 12 ) )
  }

  "Combining a graph with itself" should "result in the same graph" in {
    graphWithIsolatedVertex ++ graphWithIsolatedVertex shouldEqual graphWithIsolatedVertex
  }

  "Combining partially overlapping graphs" should "join the two graphs" in {
    graphWithIsolatedVertex ++ Graph( Set( 3 -> 4, 4 -> 5, 3 -> 6, 4 -> 6 ) ) shouldEqual Graph( List( 1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 3 -> 6, 4 -> 5, 4 -> 6 ) )
  }

  // TODO: Test connectedness.

  "An empty graph" should "not be a complete graph" in {
    Graph.empty[Int] should not be Symbol( "complete" )
  }

  "A graph with a single vertex" should "be a complete graph" in {
    Graph( Set("a"), Set.empty ) shouldBe Symbol( "complete" )
  }

  "A clique" should "be a complete graph" in {
    Graph.clique( Set(1, 3, 5, 7) ) shouldBe Symbol( "complete" )
  }

  "A graph with an odd pair of vertices that are all connected" should "be a complete graph" in {
    Graph( Set(1, 2, 3), Set( 1 -> 2, 1 -> 3, 2 -> 3) ) shouldBe Symbol( "complete" )
  }

  "A graph with an even pair of vertices that are all connected" should "be a complete graph" in {
    Graph( Set( 1, 2 ), Set( 1 -> 2 ) ) shouldBe Symbol( "complete" )
  }

  "A graph in which a single pair of vertices is not connected by an edge" should "not be a complete graph" in {
    Graph.clique( Set(1, 2, 3) ).--( Set( 1 -> 3 ) ) should not be Symbol( "complete" )
  }

  // TODO: Combining weighted graphs is not supported and will remove the weights.
  /*"Combining weighted graphs" should "join the graphs with the correct weights" in {
    Graph.weighted( Set( (1, 1, 5.0), (1, 2, 2.0), (1, 3, 4.0), (2, 3, 2.0) ) ) ++
      Graph.weighted( Set( (3, 4, 1.0), (3, 5, 1.50), (4, 5, 3.75) ) ) shouldEqual
      Graph.weighted( Set( (1, 1, 5.0), (1, 2, 2.0), (1, 3, 4.0), (2, 3, 2.0), (3, 4, 1.0), (3, 5, 1.50), (4, 5, 3.75) ) )
  }*/

  "A graph" should "be equal to itself" in {
    graphWithIsolatedVertex shouldEqual graphWithIsolatedVertex
    Graph(List(1, 2, 3, 4), List(1 -> 2, 1 -> 3, 2 -> 3)) shouldEqual Graph(List(1, 2, 3, 4), List(1 -> 2, 1 -> 3, 2 -> 3))
    Graph.weighted( List((1, 2, 2.0), (2, 3, 4.0), (3, 1, 3.0) ) ) shouldEqual Graph.weighted( List((1, 2, 2.0), (2, 3, 4.0), (3, 1, 3.0) ) )
  }

  "Graphs with different vertex sets" should "not be equal" in {
    Graph( List(1 -> 2, 1 -> 3, 2 -> 3) ) should !== ( Graph( List(1, 2, 3, 4), List(1 -> 2, 1 -> 3, 2 -> 3) ) )
  }

  "Graphs with different edges" should "not be equal" in {
    Graph( List(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1) ) should !== ( Graph( List(1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 1) ) )
  }

  "Graphs whose edges have different weights" should "not be equal" in {
    Graph.weighted( List((1, 2, 2.0), (2, 3, 4.0), (3, 1, 3.0) ) ) should !== (Graph.weighted( List((1, 2, 1.0), (2, 3, 1.0), (3, 1, 1.0) ) ))
  }

  "A connected graph" should "have a single component" in {
    unweightedGraph.components() shouldEqual Set( Set(1, 2, 3, 4, 5, 6) )
  }

  "An isolated vertex" should "be in a separate component" in {
    graphWithIsolatedVertex.components() shouldEqual Set( Set(1, 2, 3), Set(4) )
  }

  "All components" should "be identified" in {
    graphWithMultipleComponents.components() shouldEqual Set( Set(1, 2, 3, 4), Set(5, 6, 7), Set(8, 9, 10, 11) )
  }

  "Excluding an empty set of vertices" should "return the original components" in {
    unweightedGraph.components( Set.empty ) shouldEqual Set( (Set(1, 2, 3, 4, 5, 6), Set.empty) )
  }

  "Excluding an articulation point from a connected graph" should "create multiple components with that vertex as the neighbourhood" in {
    unweightedGraph.components( Set(4) ) shouldEqual Set( (Set(1, 2, 3), Set(4)), (Set(5, 6), Set(4)) )
  }

  "The components with multiple vertices excluded" should "be correctly identified" in {
    graphWithMultipleComponents.components( Set(9, 10) ) shouldEqual Set( (Set(1, 2, 3, 4), Set.empty), (Set(5, 6, 7), Set.empty), (Set(8), Set(9, 10)), (Set(11), Set(9, 10)) )
  }

  "The component of a vertex and its neighbourhood" should "be correct when no vertices are excluded" in {
    unweightedGraph.component( 1, Set.empty ) shouldEqual ( Set(1, 2, 3, 4, 5, 6), Set.empty )
  }

  it should "be correct when a single vertex is excluded" in {
    unweightedGraph.component( 1, Set(4) ) shouldEqual ( Set(1, 2, 3), Set(4) )
  }

  it should "be correct when multiple vertices are excluded" in {
    weightedGraph.component( 2, Set(1, 5) ) shouldEqual ( Set(2, 4), Set(1, 5) )
  }
}
