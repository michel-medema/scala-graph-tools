package nl.rug.ds.graph.tree.decomposition

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.{Graph, GraphDefinitions}
import org.scalatest.PrivateMethodTester

class TamakiSpec extends UnitSpec with PrivateMethodTester with GraphDefinitions {
  "Tamaki" should "convert a complete graph to a tree decomposition" in {
    val c1: Set[String] = Set( "1", "3", "5" )
    val g: Graph[String] = Graph.clique( c1 )
    new Tamaki().treeWidth( g ) shouldEqual 2
  }

  "Decomposing a chordal graph" should "create a tree decomposition where each maximal clique is a cluster" in {
    new Tamaki().treeWidth( chordalGraph2 ) shouldEqual 2
  }

  "Decomposing an empty graph" should "create an empty tree decomposition" in {
    new Tamaki().treeWidth( Graph.empty[String]  ) shouldEqual 0
  }
}
