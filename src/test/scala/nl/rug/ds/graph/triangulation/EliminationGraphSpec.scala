package nl.rug.ds.graph.triangulation

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.{Graph, GraphDefinitions}
import org.scalactic.Equality

class EliminationGraphSpec extends UnitSpec with GraphDefinitions {
  implicit val edgeEq: Equality[(Int, Int)] = ( a: (Int, Int), b: Any ) => b match {
    case (u: Int, v: Int) => (a._1 == u && a._2 == v) || (a._1 == v && a._2 == u)
    case _ => false
  }

  implicit val edgesEq: Equality[Set[(Int, Int)]] = ( a: Set[(Int, Int)], b: Any ) => b match {
    case edges: Set[(Int, Int)] => a.size == edges.size && a.forall( e => edges.contains( e ) || edges.contains( e.swap ) )
    case _ => false
  }

  // TODO: Ordering when not all vertices are specified.
  // TODO: Ordering when non-existing vertices are specified.

  "Eliminating a vertex in a complete graph" should "not generate fill edges" in {
    val (eliminationGraph: Graph[Int], fillEdges: Iterator[(Int, Int)]) = EliminationGraph.eliminate( k4, 2 )

    eliminationGraph shouldEqual Graph( List( 1 -> 3, 1 -> 4, 3 -> 4 ) )
    fillEdges.to(LazyList) shouldBe empty
  }

  "Eliminating a vertex in a tree" should "create fill edges between all neighbours" in {
    val (eliminationGraph: Graph[Int], fillEdges: Iterator[(Int, Int)]) = EliminationGraph.eliminate( tree, 1 )

    eliminationGraph shouldEqual Graph( List( 2 -> 3, 2 -> 4, 3 -> 4, 2 -> 5, 2 -> 6 ) )
    fillEdges.to( LazyList ) should contain theSameElementsAs Set( 2 -> 3, 2 -> 4, 3 -> 4 )
  }

  "Eliminating a vertex with edges between some neighbours" should "only fill in the missing edges" in {
    val (eliminationGraph: Graph[Int], fillEdges: Iterator[(Int, Int)]) = EliminationGraph.eliminate( cycleGraph, 4 )

    eliminationGraph shouldEqual Graph( List( 1 -> 2, 2 -> 3, 3 -> 5, 5 -> 1 ) )
    fillEdges.to( LazyList ) should contain theSameElementsAs Set( 3 -> 5 )
  }

  "The sequence of fill edges" should "be a sequence of empty sets for a complete graph" in {
    k4ElimGraph.fillEdges.to(LazyList) should contain theSameElementsInOrderAs List( Set.empty, Set.empty, Set.empty, Set.empty )
  }

  it should "contain the fill edges in the correct order" in {
    val eliminationGraph: EliminationGraph[Int] = new EliminationGraph( cycleGraph, List( 3, 4, 5, 1, 2 ) )

    eliminationGraph.fillEdges.to( LazyList ) should contain theSameElementsInOrderAs List( Set( 2 -> 4 ), Set( 2 -> 5 ), Set.empty, Set.empty, Set.empty )
  }

  it should "be empty for an empty graph" in {
    emptyEliminationGraph.fillEdges.to( LazyList ) shouldBe empty
  }

  "Eliminating the vertices using a perfect elimination order" should "not introduce any fill edges" in {
    chordalElimGraph2.fillEdges.to( LazyList ) should contain theSameElementsInOrderAs List.fill(5)( Set.empty )
  }

  "The triangulation of a complete graph" should "be equal to the original graph" in {
    k4ElimGraph.h shouldEqual k4
  }

  "The triangulation of a chordal graph" should "be equal to the original graph" in {
    chordalElimGraph1.h shouldEqual chordalGraph1
  }

  "The triangulation of a chordal graph with non-perfect elimination order" should "include the fill edges" in {
    new EliminationGraph( chordalGraph1, List( 4, 2, 3, 1 ) ).h shouldEqual chordalGraph1.addEdges( Set(2 -> 3) )
  }

  "The triangulation" should "include all the fill edges" in {
    val eliminationGraph: EliminationGraph[Int] = new EliminationGraph( cycleGraph, List( 3, 4, 5, 1, 2 ) )
    eliminationGraph.h shouldEqual cycleGraph.addEdges( Set( 2 -> 4, 2 -> 5 ) )
  }

  "An elimination ordering of a complete graph" should "be perfect" in {
    k4ElimGraph.isPerfect shouldBe true
  }

  "An elimination ordering of a non-chordal graph" should "not be perfect" in {
    new EliminationGraph( cycleGraph, List( 1, 2, 3, 4, 5 ) ).isPerfect shouldBe false
  }

  "An elimination ordering of a chordal graph" should "not be perfect if it results in fill edges" in {
    new EliminationGraph( chordalGraph1, List( 4, 2, 3, 1 ) ).isPerfect shouldBe false
  }

  "An elimination ordering of a chordal graph" should "be perfect if there are no fill edges" in {
    chordalElimGraph1.isPerfect shouldBe true
  }

  // TODO: Extract for easy usage when multiple classes derive this trait.

  "The maximal cliques of an empty graph" should "be an empty collection" in {
    emptyEliminationGraph.maximalCliques shouldEqual List.empty
  }

  "The maximal cliques of a complete graph" should "contain a single clique with all vertices" in {
    k4ElimGraph.maximalCliques shouldEqual Vector( k4.V )
  }

  "The maximal cliques of a graph" should "contain all maximal cliques" in {
    treeElimGraph.maximalCliques shouldEqual Vector( Set( 1, 2, 5, 6 ), Set( 1, 3 ), Set( 1, 4, 6 ) )
  }

  "The maximal cliques of a graph" should "only contain maximal cliques" in {
    sameNeighboursElimGraph.maximalCliques shouldEqual Vector( Set( 1, 2, 3, 4, 5 ), Set( 3, 4, 5, 6 ), Set( 5, 6, 7 ) )
  }

  "The clique number" should "be zero for an empty graph" in {
    emptyEliminationGraph.cliqueNumber shouldEqual 0
  }

  it should "be equal to the number of vertices for a complete graph" in {
    k4ElimGraph.cliqueNumber shouldEqual k4.n
  }

  it should "be equal to the size of the largest clique" in {
    treeElimGraph.cliqueNumber shouldEqual 4
  }
}
