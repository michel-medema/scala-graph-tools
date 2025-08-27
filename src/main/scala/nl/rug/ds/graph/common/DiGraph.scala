package nl.rug.ds.graph.common

import nl.rug.ds.graph.common.GenericGraph._

import scala.collection.immutable.VectorMap


object DiGraph {
  def empty[V]: DiGraph[V] = new DiGraph( VectorMap.empty )

  def weighted[V]( E: IterableOnce[(V, V, Weight)] ): DiGraph[V] = DiGraph.weighted( List.empty, E )

  def weighted[V]( V: IterableOnce[V], E: IterableOnce[(V, V, Weight)] ): DiGraph[V] = DiGraph.empty.addVertices( V ).addEdges( E )
}

// TODO: Make constructor private.
class DiGraph[V]( protected val A: AdjacencyList[V] ) extends GenericGraph[V, DiGraph[V]] {
  // TODO: Implement this (not used for now).
  lazy val m: Int = ???

  override protected def create(adjacencyList: AdjacencyList[V]): DiGraph[V] = new DiGraph(adjacencyList)

  def updateEdge(u: V, v: V, w: Weight): DiGraph[V] = this.setWeight( u, v, w )

  def addEdges( es: IterableOnce[(V, V, Weight)] ): DiGraph[V] = {
    es.iterator.foldLeft( this ){
      case (g: DiGraph[V], (u: V, v: V, w: Weight)) => g.addWeight( u, v, w ).addVertices( Set( v) )
    }
  }
}
