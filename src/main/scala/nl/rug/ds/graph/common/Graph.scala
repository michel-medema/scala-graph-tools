package nl.rug.ds.graph.common

import nl.rug.ds.common.Helper.pairs
import nl.rug.ds.graph.common.GenericGraph._

import scala.annotation.tailrec
import scala.collection.immutable.VectorMap

object Graph {
	def empty[V]: Graph[V] = new Graph( VectorMap.empty )

	def clique[V](vs: Set[V]): Graph[V] = Graph( pairs(vs) )

	def apply[V]( E: IterableOnce[(V, V)] ): Graph[V] = Graph( List.empty, E )

	def apply[V]( V: IterableOnce[V], E: IterableOnce[(V, V)] ): Graph[V] = Graph.empty.addVertices( V ).addEdges( E )

	def weighted[V]( E: IterableOnce[(V, V, Weight)] ): Graph[V] = Graph.weighted( List.empty, E )

	def weighted[V]( V: IterableOnce[V], E: IterableOnce[(V, V, Weight)] ): Graph[V] = {
		E.iterator.foldLeft( Graph.empty.addVertices( V ) ) {
			case (g: Graph[V], (u: V, v: V, w: Weight)) =>
				// TODO: Sum weights or replace them when the same edge appears more than once?
				if ( u == v ) {
					g.addWeight( u, v, w )
				} else {
					g.addWeight( u, v, w ).addWeight( v, u, w )
				}
		}
	}
}

// TODO: Separate weighted and unweighted graph.
// TODO: Make constructor private.
class Graph[V]  ( val A: AdjacencyList[V] ) extends GenericGraph[V, Graph[V]] {
	// TODO: Could generalise this method if both types of graphs have the 'edges' method.
	// Count the number of edges for each node and divide by two since all edges are counted twice.
	lazy val m: Int = A.iterator.map( _._2.size ).sum / 2

	override protected def create(adjacencyList: AdjacencyList[V]): Graph[V] = new Graph(adjacencyList)

	// TODO: Edges are undirected, but tuples do not capture this.
	def E: Iterator[(V, V)] = {
		val indices: Map[V, Int] = vertices.zipWithIndex.toMap

		A.iterator.flatMap {
			case (v: V, neighbours: Map[V, Weight]) => neighbours.keys.filter( u => indices(v) <= indices(u) ).map( (v, _) )
		}
	}

	def degree(v: V): Double = degrees(v)

	def addEdges( es: IterableOnce[(V, V)] ): Graph[V] = {
		es.iterator.filterNot( e => A.contains(e._1) && A(e._1).contains(e._2) ).foldLeft( this ) {
			case (g: Graph[V], (u: V, v: V)) => g.setWeight( u, v, 1.0 ).setWeight( v, u, 1.0 )
		}
	}

	// TODO: Call this union?
	def ++(g: Graph[V]): Graph[V] = {
		this.addVertices( g.V ).addEdges( g.E )
	}

	def --( edges: Set[(V, V)] ): Graph[V] = {
		edges.iterator.foldLeft( this ){
			case (g: Graph[V], (u: V, v: V)) => g.removeEdge( u, v ).removeEdge( v, u )
		}
	}

  /**
   * Returns whether the graph is connected or not, which is the case if
   * it has exactly one component (an empty graph is not connected).
   */
	def isConnected: Boolean = components().size == 1

	def isComplete: Boolean = {
		// Determining whether n is even or odd allows safe integer division.
		if ( n % 2 == 0 ) {
			n > 0 && m == ( (n / 2) * (n - 1) )
		} else {
			n > 0 && m == ( n * ( (n - 1) / 2 ) )
		}
	}

	def components(): Set[Set[V]] = components( Set.empty ).map( _._1 )

	def components( removed: Set[V] ): Set[(Set[V], Set[V])] = {
		@tailrec
		def components( vs: Set[V], C: Set[(Set[V], Set[V])] ): Set[(Set[V], Set[V])] = {
			if ( vs.isEmpty ) {
				C
			} else {
				val v: V = vs.head
				val comp: (Set[V], Set[V]) = component(v, removed)

				components( vs -- comp._1, C + comp )
			}
		}

		components( V -- removed, Set.empty )
	}

	//def component( v: V ): Set[V] = component(v, Set.empty)._1

	// TODO: Introduce type for components to avoid confusion.
	def component( v: V, removed: Set[V] ): (Set[V], Set[V]) = {
		val (g: Graph[V], neighbourhood: Set[V]) = graphComponent(v, removed)
		(g.V, neighbourhood)
	}

	private def graphComponent( v: V, removed: Set[V] ): (Graph[V], Set[V]) = {
		@tailrec
		def reachable( vs: Set[V], adjacencyList: AdjacencyList[V], N: Set[V]): (Graph[V], Set[V]) = {
			if ( vs.isEmpty ) {
				( new Graph( adjacencyList ), N )
			} else {
				val v: V = vs.head

				if ( A.contains(v) && !adjacencyList.contains(v) ) {
					val (neighbourhood, neighbours) = A(v).partition( r => removed.contains(r._1) )

					reachable( (vs ++ neighbours.keySet) - v, adjacencyList.updated(v, neighbours), N ++ neighbourhood.keySet )
				} else {
					reachable( vs - v, adjacencyList, N )
				}
			}
		}

		reachable( Set(v).diff(removed), VectorMap.empty[V, Map[V, Weight]], Set.empty )
	}

	override def toString: String = {
		s"Graph([${vertices.mkString(", ")}], [${E.mkString(", ")}])"
	}
}
