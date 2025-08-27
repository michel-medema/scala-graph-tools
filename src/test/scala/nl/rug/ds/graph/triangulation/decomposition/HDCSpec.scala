package nl.rug.ds.graph.triangulation.decomposition

import nl.rug.ds.common.UnitSpec
import nl.rug.ds.graph.common.Graph

class HDCSpec extends UnitSpec {
  "Heuristic Decomposition with Community Structure" should "not decompose an empty graph" in {
    val hdc: HDC = new HDC()

    hdc.decompose( Graph.empty[Int] )._1 shouldEqual Set( Graph.empty[Int] )
  }

  it should "not decompose a graph with a single community" in {
    val g: Graph[Int] = Graph.clique( Set( 1, 2, 3, 4, 5 ) )

    val hdc: HDC = new HDC()

    hdc.decompose( g )._1 shouldEqual Set( g )
  }

  it should "use the smallest community in a graph with two communities that are fully connected" in {
    val g: Graph[Int] = (Graph.clique(Set(1, 2, 3, 4, 5)) ++ Graph.clique(Set(6, 7, 8, 9, 10, 11))) addEdges Set( 1 -> 6, 2 -> 7, 3 -> 8, 4 -> 9, 5 -> 10, 1 -> 11, 2 -> 11 )

    val hdc: HDC = new HDC()

    hdc.decompose( g )._1 shouldEqual Set( g )
  }

  it should "select the smallest separator until all communities are disconnected" in {
    /*
    1 - 2 - 5 - 7 - 09 - 11 - 13
    |   |   | / |   |  \    X
    4 - 3 - 6 - 8 - 10   12 - 14
     */
    val g: Graph[Int] = Graph(
      Set( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ),
      Set(
        1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1, 2 -> 5, 1 -> 5, 3 -> 6,
        5 -> 6, 5 -> 7, 5 -> 9, 5 -> 10, 6 -> 7, 7 -> 8, 6 -> 8, 7 -> 9, 8 -> 9, 8 -> 10, 9 -> 10, 9 -> 11, 9 -> 12,
        11 -> 13, 11 -> 14, 12 -> 13, 12 -> 14 )
    )

    val g1: Graph[Int] = Graph(
      Set( 1, 2, 3, 4, 5, 6 ),
      Set( 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1, 2 -> 5, 1 -> 5, 3 -> 6, 5 -> 6 )
    )

    val g2: Graph[Int] = Graph(
      Set( 5, 6, 7, 8, 9, 10 ),
      Set( 5 -> 6, 5 -> 7, 5 -> 9, 5 -> 10, 6 -> 7, 7 -> 8, 6 -> 8, 7 -> 9, 8 -> 9, 8 -> 10, 9 -> 10 )
    )

    val g3: Graph[Int] = Graph(
      Set( 9, 11, 12, 13, 14 ),
      Set( 9 -> 11, 9 -> 12, 11 -> 13, 11 -> 14, 12 -> 13, 12 -> 14 )
    )

    val hdc: HDC = new HDC()

    hdc.decompose( g )._1 shouldEqual Set( g1, g2, g3 )
  }

  it should "disconnect each community separately when they form a cycle" in {
    val g: Graph[Int] = (Graph.clique( Set( 1, 2, 3 ) ) ++ Graph.clique( Set( 4, 5, 6, 7 ) ) ++ Graph.clique( Set( 8, 9, 10, 11 ) ))
      .addEdges( Set( 2 -> 4, 3 -> 8, 4 -> 8, 4 -> 9, 6 -> 10, 7 -> 11 ) )

    val g1: Graph[Int] = Graph.clique( Set( 1, 2, 3 ) )

    val g2: Graph[Int] = Graph.clique( Set( 4, 5, 6, 7 ) )

    val g3: Graph[Int] = Graph(
      Set( 2, 3, 4, 6, 7, 8, 9, 10, 11 ),
      Set( 2 -> 3, 2 -> 4, 3 -> 8, 4 -> 6, 4 -> 7, 6 -> 7, 4 -> 8, 4 -> 9, 6 -> 10, 7 -> 11,  8 -> 9, 8 -> 10, 8 -> 11, 9 -> 10, 9 -> 11, 10 -> 11 )
    )

    val hdc: HDC = new HDC()

    hdc.decompose( g )._1 shouldEqual Set( g1, g2, g3 )
  }

  it should "select the smallest separator regardless of the community size" in {
    val g: Graph[Int] = Graph(
      Set( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ),
      Set(
        1 -> 2, 1 -> 3, 2 -> 3, 1 -> 4, 2 -> 4,
        4 -> 5, 4 -> 6, 4 -> 7, 4 -> 8, 5 -> 8, 6 -> 7, 6 -> 8, 7 -> 9, 7 -> 10, 8 -> 10, 8 -> 11,
        9 -> 10, 9 -> 11, 10 -> 11
      )
    )

    val g1: Graph[Int] = Graph(
      Set( 1, 2, 3, 4 ),
      Set( 1 -> 2, 1 -> 3, 2 -> 3, 1 -> 4, 2 -> 4 )
    )

    val g2: Graph[Int] = Graph(
      Set( 4, 5, 6, 7, 8 ),
      Set( 4 -> 5, 4 -> 6, 4 -> 7, 4 -> 8, 5 -> 8, 6 -> 7, 6 -> 8, 7 -> 8 )
    )

    val g3: Graph[Int] = Graph(
      Set( 7, 8, 9, 10, 11 ),
      Set( 7 -> 8, 7 -> 9, 7 -> 10, 8 -> 10, 8 -> 11, 9 -> 10, 9 -> 11, 10 -> 11 )
    )

    val hdc: HDC = new HDC()

    hdc.decompose( g )._1 shouldEqual Set( g1, g2, g3 )
  }

  it should "not continue decomposing a graph when only one community remains" in {
    val g: Graph[Int] = Graph.clique( Set(1, 2, 3, 4, 5) ) ++ Graph(
      Set( 6, 7, 8, 9 ),
      Set( 1 -> 6, 1 -> 9, 3 -> 7, 4 -> 8, 5 -> 8 )
    )

    val g1: Graph[Int] = g.subgraph( Set(1, 6) )

    val g2: Graph[Int] = g.subgraph( Set(1, 9) )

    val g3: Graph[Int] = g.subgraph( Set(3, 7) )

    val g4: Graph[Int] = g.subgraph( Set(1, 2, 3, 4, 5, 8) )

    val hdc: HDC = new HDC()

    hdc.decompose( g )._1 shouldEqual Set( g1, g2, g3, g4 )
  }
}
