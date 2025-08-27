package nl.rug.ds.graph.common

import nl.rug.ds.graph.common.AdjacencyList.{Vertex, Weight}

import scala.collection.immutable.ArraySeq


object AdjacencyList {
	type Vertex = Int
	type Weight = Double
}

final case class AdjacencyList(
	A: ArraySeq[Map[Vertex, Weight]]
) {
	val n: Int = A.size

	private lazy val N: Map[Vertex, Set[Vertex]] = A.zipWithIndex.map( v => v._2 -> v._1.keySet ).toMap

	lazy val d: Map[Vertex, Double] = A.zipWithIndex.map(v => v._2 -> v._1.values.sum).toMap

	def neighbours(v: Vertex): Set[Vertex] = N(v)

	def degree(v: Vertex): Double = d(v)
}
