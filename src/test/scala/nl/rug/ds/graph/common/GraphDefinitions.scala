package nl.rug.ds.graph.common

import nl.rug.ds.graph.triangulation.EliminationGraph

trait GraphDefinitions {
  type Community = Int

  val chordalGraph1: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 4, 3 -> 4 ) )
  val chordalGraph2: Graph[Int] = Graph( List( 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 1, 2 -> 5, 2 -> 4 ) )

  val k4: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 3, 2 -> 4, 3 -> 4 ) )
  val tree: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 5, 2 -> 6 ) )

  val cycleGraph: Graph[Int] = Graph( List( 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 1 ) )
  val doubleSquare: Graph[Int] = Graph( List( 1 -> 3, 1 -> 4, 2 -> 3, 2 -> 5, 3 -> 6, 4 -> 6, 5 -> 6 ) )
  val sameNeighbours: Graph[Int] = Graph( Set( 1 -> 2, 1 -> 3, 1 -> 4, 1 -> 5, 2 -> 3, 2 -> 4, 2 -> 5, 3 -> 6, 4 -> 6, 5 -> 7 ) )

  val emptyEliminationGraph: EliminationGraph[Int] = new EliminationGraph( Graph.empty, List.empty )
  val k4ElimGraph: EliminationGraph[Int] = new EliminationGraph( k4, List(2, 1, 4, 3) )
  val treeElimGraph: EliminationGraph[Int] = new EliminationGraph( tree, List(2, 5, 3, 1, 6, 4) )
  val chordalElimGraph1: EliminationGraph[Int] = new EliminationGraph( chordalGraph1, List( 2, 3, 4, 1 ) )
  val chordalElimGraph2: EliminationGraph[Int] = new EliminationGraph( chordalGraph2, List( 3, 4, 1, 5, 2 ) )
  val sameNeighboursElimGraph: EliminationGraph[Int] = new EliminationGraph( sameNeighbours, List(1, 2, 3, 4, 5, 6, 7) )

  val treeTriangulation: Graph[Int] = Graph( List( 1 -> 2, 1 -> 3, 1 -> 4, 1 -> 5, 1 -> 6, 2 -> 5, 2 -> 6, 4 -> 6, 5 -> 6 ) )

  val unweightedGraph: Graph[Int] = Graph(List(1 -> 2, 1 -> 3, 2 -> 3, 3 -> 4, 4 -> 5, 4 -> 6, 5 -> 6))
  val unweightedGraphWithSelfLoop: Graph[Int] = Graph( List( 1 -> 1, 1 -> 2, 1 -> 3, 1 -> 4, 2 -> 3, 3 -> 4 ) )
  val graphWithIsolatedVertex: Graph[Int] = Graph(List(1, 2, 3, 4), List(1 -> 2, 1 -> 3, 2 -> 3))
  val weightedGraph: Graph[Int] = Graph.weighted(List((1, 1, 5.0), (1, 2, 2.0), (1, 3, 4.0), (2, 2, 4.0), (2, 4, 2.0), (2, 5, 1.0), (3, 5, 1.0), (4, 5, 3.0), (4, 4, 5.0)))
  val graphWithMultipleComponents: Graph[Int] = Graph( Set( 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1, 5 -> 6, 5 -> 7, 6 -> 7, 8 -> 9, 8 -> 10, 9 -> 10, 9 -> 11, 10 -> 11 ) )
}
