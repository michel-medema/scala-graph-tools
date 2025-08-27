package nl.rug.ds.graph.common

import scala.annotation.tailrec

// TODO: This does not have to be a class.
class MaximumFlow[V](g: DiGraph[V]) {
  private type Flow = Double

  private def augmentingPath(g: DiGraph[V], s: V, t: V): Option[List[(V, V)]] = {
    @tailrec
    def constructPath(v: V, predecessors: Map[V, V], path: List[(V, V)]): Option[List[(V, V)]] = {
      predecessors.get(v) match {
        case None =>
          if ( v == s ) {
            Some( path )
          } else {
            None
          }

        case Some(w: V) => constructPath(w, predecessors, path.prepended( w -> v ) )
      }
    }

    @tailrec
    def findPath(predecessors: Map[V, V], l: List[V]): Option[List[(V, V)]] = {
      l match {
        // No more vertices to process.
        case Nil => constructPath(t, predecessors, List.empty)

        case v :: tail =>
          // TODO: Terminate early if t reached.
          val neighbours: Set[V] = (g.neighbours(v) - s).filterNot( predecessors.contains ).filter( w => g.weight(v, w) > 0.0 )

          findPath( predecessors ++ neighbours.map( w => w -> v ), tail ++ neighbours.toList )
      }
    }

    findPath( Map.empty, List(s) )
  }

  def maxFlow(s: V, t: V): Flow = {
    @tailrec
    def maxFlow( g: DiGraph[V], flow: Double ): Flow = {
      // Find an augmenting path from source to sink.
      augmentingPath(g, s, t) match {
        // No more augmenting paths. Return the maximum flow.
        case None => flow

        case Some(p: List[(V, V)]) =>
          // Find the amount of flow that can be send along the path, which is the minimum over the remaining capacities of the edges.
          val capacity: Flow = p.map( e => g.weight( e._1, e._2 ) ).min

          // Update the flow of each edge and reverse edge.
          val g1: DiGraph[V] = p.foldLeft( g ) {
            case ( graph: DiGraph[V], (u: V, v: V) ) =>
              graph
                .updateEdge(u, v,  graph.weight(u, v) - capacity )
                .updateEdge(v, u,  graph.weight(v, u) + capacity )
          }

          maxFlow( g1, flow + capacity )
      }
    }

    // The initial flow through the network is zero everywhere.
    maxFlow( g, flow = 0.0 )
  }
}
