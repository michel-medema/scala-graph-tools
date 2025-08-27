package nl.rug.ds.graph.generator

import nl.rug.ds.common.Helper.selectRandom

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait GraphGenerator {
  @tailrec
  protected final def combine[V]( l: List[List[V]], n: Int, combinations: Queue[List[V]] ): List[List[V]] = {
    if ( l.size > 1 ) {
      val a: Int = math.round( (l.size - 1).toDouble / (n - 1).toDouble ).toInt

      assert( l.size == (a * (n - 1)) + 1 )

      val shuffled: List[List[V]] = util.Random.shuffle( l )

      // Pick n items from l.
      val l1: List[List[V]] = shuffled.take( n )

      // Pick random item from each.
      val comb: List[V] = l1.map( list => selectRandom( list ).get )

      combine( l1.flatten +: shuffled.drop( n ), n, combinations.appended( comb ) )
    } else {
      combinations.toList
    }
  }

  // TODO: Can this be done more efficiently?
  @tailrec
  protected final def randomEdges[V]( vs: IndexedSeq[V], m: Int, E: Set[(V, V)] ): Set[(V, V)] = {
    if ( E.size < m ) {
      // Pick two random vertices from the given set of vertices.
      val u: V = vs( util.Random.nextInt( vs.size ) )
      val v: V = vs( util.Random.nextInt( vs.size ) )

      if ( u != v && !E.contains( (u, v) ) && !E.contains( (v, u) ) ) {
        // Keep the edge if it does not connect a vertex to itself and if it is not already in the set of edges.
        randomEdges( vs, m, E.+( (u, v) ) )
      } else {
        // Otherwise, generate a new edge.
        randomEdges( vs, m, E )
      }
    } else {
      E
    }
  }
}
