package nl.rug.ds.graph.common

import nl.rug.ds.common.Helper

import GenericGraph.Weight

import scala.annotation.tailrec

object GraphConnectivity {
  // TODO: While finding the maximal clique requires exponential time in the worst case for general graphs, this method is terribly inefficient and only designed for extremely small graphs.
  def maximalCliques[V]( g: Graph[V] ): Set[Set[V]] = {
    @tailrec
    def findCliques( cliques: Set[Set[V]], maximalCliques: Set[Set[V]] ): Set[Set[V]] = {
      if ( cliques.isEmpty ) {
        maximalCliques
      } else {
        val cliqueNeighbours: Set[(Set[V], Set[V])] = cliques.map( clique => clique -> clique.map( v => g.neighbours( v ) ).reduce( _ intersect _ ) )

        val (maxCliques, newCliques) = cliqueNeighbours.partition( _._2.isEmpty )

        val newCliques1: Set[Set[V]] = newCliques.flatMap {
          case (clique: Set[V], commonNeighbours: Set[V]) => commonNeighbours.map( n => clique + n )
        }

        findCliques( newCliques1, maximalCliques ++ maxCliques.map( _._1 ) )
      }
    }

    findCliques( g.V.map( v => Set( v ) ), Set.empty )
  }


  def largestClique[V]( g: Graph[V] ): Set[V] = maximalCliques( g ).maxBy( _.size )

  def vertexConnectivity[V]( g: Graph[V], vs: IterableOnce[(V, V)] ): Iterator[Int] = {
    val mapping: Map[V, Int] = g.V.zipWithIndex.toMap

    val edges: List[(Int, Int, Weight)] = mapping.toList.flatMap {
      case (v: V, i: Int) =>
        val in: Int = 2 * i
        val out: Int = in + 1

        (in, out, 1.0) :: g.neighbours( v ).toList.flatMap( w => {
          List( (out, 2 * mapping( w ), 1.0), ((2 * mapping( w )) + 1, in, 1.0) )
        } )
    }

    val N: DiGraph[Int] = DiGraph.weighted( mapping.flatMap( v => List( v._2 * 2, (v._2 * 2) + 1 ) ), edges )

    vs.iterator.map {
      case (s: V, t: V) =>
        if ( g.neighbours( s ).contains( t ) ) {
          Int.MaxValue
        } else {
          new MaximumFlow( N ).maxFlow( mapping( s ) * 2 + 1, mapping( t ) * 2 ).toInt
        }
    }
  }

  // TODO: Duplication.
  def vertexConnectivity[V](g: Graph[V], s: V, t: V): Int = {
    if ( g.neighbours(s).contains(t) ) {
      Int.MaxValue
    } else {
      val mapping: Map[V, Int] = g.V.zipWithIndex.toMap

      val edges: List[(Int, Int, Weight)] = mapping.toList.flatMap {
        case (v: V, i: Int) =>
          val in: Int = 2 * i
          val out: Int = in + 1

          (in, out, 1.0) :: g.neighbours(v).toList.flatMap( w => {
            List( (out, 2 * mapping(w), 1.0), ((2 * mapping(w)) + 1, in, 1.0) )
          } )
      }

      val N: DiGraph[Int] = DiGraph.weighted( mapping.flatMap( v => List( v._2 * 2, (v._2 * 2) + 1 ) ), edges )

      new MaximumFlow(N).maxFlow( mapping(s) * 2 + 1, mapping(t) * 2 ).toInt
    }
  }

  def connectivity[V](g: Graph[V], vs: Set[V]): Int = {
    Helper.pairs( vs ).map { case (s, t) => vertexConnectivity(g, s, t) }.min
  }

  // TODO: Vertex connectivity for the entire graph (i.e. minimum over all pairs of non-adjacent vertices).
}
