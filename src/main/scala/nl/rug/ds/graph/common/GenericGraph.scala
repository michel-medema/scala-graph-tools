package nl.rug.ds.graph.common

import nl.rug.ds.graph.common.GenericGraph._

import scala.collection.immutable.VectorMap

object GenericGraph {
  type Weight = Double
  type AdjacencyList[V] = VectorMap[V, Map[V, Weight]]
}

trait GenericGraph[V, G <: GenericGraph[V, G]] { this: G =>
  protected val A: AdjacencyList[V]

  // The number of vertices in the graph.
  val n: Int = A.size

  // The number of edges in the graph.
  val m: Int

  protected lazy val degrees: Map[V, Weight] = A.map( v => v._1 -> v._2.values.sum )

  protected def create( adjacencyList: AdjacencyList[V] ): G

  private def addEdge( u: V, v: V )( f: Weight => Weight ): G = {
    this.create(
      A.updatedWith(u)( es =>
        Some( es.getOrElse( Map.empty ).updatedWith(v)( weight => Some( f( weight.getOrElse(0.0) ) ) ) )
      )
    )
  }

  protected def addWeight( u: V, v: V, w: Weight ): G = addEdge( u, v )( _ + w )

  protected def setWeight( u: V, v: V, w: Weight ): G = addEdge( u, v )( _ => w )

  protected def removeEdge( u: V, v: V ): G = {
    this.create(
      A.updatedWith(u)( _.map( _ - v ) )
    )
  }

  def V: Set[V] = A.keySet

  // An iterator that traverses the vertices in the order in which they were inserted into the graph.
  def vertices: Iterator[V] = A.keysIterator

  def neighbours(v: V): Set[V] = A.getOrElse(v, Map.empty).keySet

  def neighbours( vs: Set[V] ): Set[V] = (vs map neighbours).fold( Set.empty )( _ union _ ) -- vs

  def weight(u: V, v: V): Weight = A.getOrElse(u, Map.empty).getOrElse(v, 0.0)

  def subgraph( vs: Set[V] ): G = {
    this.create(
      A.collect {
        case (v: V, neighbours: Map[V, Weight]) if vs.contains(v) => v -> neighbours.filter( e => vs.contains(e._1) )
      }
    )
  }

  def addVertices( vs: IterableOnce[V] ): G = {
    this.create(
      A ++ vs.iterator.filterNot( A.contains ).map( v => v -> Map.empty )
    )
  }

  def \( v: V ): G = \( Set(v) )

  def \( W: Set[V] ): G = subgraph( V -- W )

  private def canEqual(other: Any): Boolean = other.isInstanceOf[G]

  override def equals(other: Any): Boolean = other match {
    case that: G => (that canEqual this) && A == that.A
    case _ => false
  }

  override def hashCode(): Int = A.hashCode()
}
