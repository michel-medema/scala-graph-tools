package nl.rug.ds.graph.triangulation

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.{Graph, GraphDefinitions}
import nl.rug.ds.graph.triangulation.algorithms.{EliminationGame, EliminationHeuristic}
import org.scalamock.scalatest.MockFactory

class EliminationGameSpec extends UnitSpec with GraphDefinitions with MockFactory {
  "Eliminating the vertices of a complete graph" should "not produce any fill-edges" in {
    val heuristic: EliminationHeuristic[Int] = mock[EliminationHeuristic[Int]]
    inSequence {
      (heuristic.nextVertex _).expects( k4 ).returns(Some(2))
      (heuristic.nextVertex _).expects( Graph( List(1 -> 3, 1 -> 4, 3 -> 4) ) ).returns(Some(1))
      (heuristic.nextVertex _).expects( Graph( List(3 -> 4) ) ).returns(Some(4))
      (heuristic.nextVertex _).expects( Graph( List(3), List.empty ) ).returns(Some(3))
      (heuristic.nextVertex _).expects( Graph.empty[Int] ).returns(None)
    }

    val eliminationGraph: EliminationGraph[Int] = new EliminationGame( heuristic ).fill( k4 )

    eliminationGraph.h shouldEqual k4ElimGraph.h
    eliminationGraph.fillEdges.to(LazyList) should contain theSameElementsInOrderAs k4ElimGraph.fillEdges.to(LazyList)
    eliminationGraph.eliminationOrdering shouldEqual k4ElimGraph.eliminationOrdering
  }

  "The elimination game" should "follow the given elimination order" in {
    val heuristic: EliminationHeuristic[Int] = mock[EliminationHeuristic[Int]]
    inSequence {
      (heuristic.nextVertex _).expects( tree ).returns( Some(2) )
      (heuristic.nextVertex _).expects( Graph( List( 1 -> 3, 1 -> 4, 1 -> 5, 1 -> 6, 5 -> 6 ) ) ).returns( Some(5) )
      (heuristic.nextVertex _).expects( Graph( List( 1 -> 3, 1 -> 4, 1 -> 6 ) ) ).returns( Some(3) )
      (heuristic.nextVertex _).expects( Graph( List( 1 -> 4, 1 -> 6 ) ) ).returns( Some(1) )
      (heuristic.nextVertex _).expects( Graph( List( 4 -> 6 ) ) ).returns( Some(6) )
      (heuristic.nextVertex _).expects( Graph( List(4), List.empty ) ).returns( Some(4) )
      (heuristic.nextVertex _).expects( Graph.empty[Int] ).returns(None)
    }

    val eliminationGraph: EliminationGraph[Int] = new EliminationGame( heuristic ).fill( tree )

    eliminationGraph.h shouldEqual treeElimGraph.h
    eliminationGraph.fillEdges.to(LazyList) should contain theSameElementsInOrderAs treeElimGraph.fillEdges.to(LazyList)
    eliminationGraph.eliminationOrdering shouldEqual treeElimGraph.eliminationOrdering
  }
}
