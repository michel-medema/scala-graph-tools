package nl.rug.ds.graph.generator

import nl.rug.ds.common.Helper
import nl.rug.ds.common.Helper.selectRandom
import nl.rug.ds.graph.common.{Graph, GraphConnectivity}

import scala.annotation.tailrec
import scala.collection.immutable.Queue


class SafeSeparatorGraph(
  numSeparators: Int,
  separatorSize: Int,
  communitiesPerSeparator: Int,
  verticesPerCommunity: Int,
  avgDegree: Int,
  separatorConnectivity: Double
) extends GraphGenerator {
  private val numCommunities: Int = (numSeparators * (communitiesPerSeparator - 1)) + 1
  private val m: Int = math.ceil( separatorConnectivity * ((separatorSize * (separatorSize - 1)).toDouble / 2.0) ).toInt
  private val extendPathProb: Double = 0.30

  private def vertexSets(numSets: Int, size: Int, offset: Int = 0): Iterator[Set[Int]] = {
    Range.apply(offset, offset + numSets * size).grouped( size ).map( _.toSet )
  }

  def generate(): (Graph[Int], List[Set[Int]]) = {
    if ( avgDegree >= verticesPerCommunity ) {
      throw new IllegalArgumentException("The average degree must be strictly smaller than the number of vertices in a community.")
    }

    if ( separatorSize > verticesPerCommunity ) {
      throw new IllegalArgumentException( "The size of the separator cannot exceed the size of the community." )
    }

    // Generate one connected graph per community.
    val communities: List[Graph[Int]] = vertexSets( numCommunities, verticesPerCommunity ).map( vs => RandomGraph.generate( vs, avgDegree ) ).toList

    // Create a spanning tree of the communities to determine how they are to be connected.
    val connectivity: List[List[Set[Int]]] = combine( communities.map( c => List(c.V) ), communitiesPerSeparator, Queue.empty )

    // Join communities into single graph.
    val g: Graph[Int] = communities.foldLeft( Graph.empty[Int] )( _ ++ _ )

    // Create one separator per set of communities.
    val separators: List[Graph[Int]] = vertexSets( numSeparators, separatorSize, g.n )
      .map( vs => Graph( vs, randomEdges( vs.toVector, m, Set.empty[(Int, Int)] ) ) ).toList

    // Add the separators to the graph.
    val g1: Graph[Int] = separators.foldLeft( g )( _ ++ _ )

    // Connect the separators and communities.
    val graph: Graph[Int] = (connectivity zip separators)
      .flatMap { case (communities: List[Set[Int]], separator: Graph[Int]) => communities.map( _ -> separator ) }
      .foldLeft( g1 ){ case (g: Graph[Int], (c: Set[Int], sep: Graph[Int])) => connect(g, c, sep) }

    (graph, separators.map( _.V ))
  }

  private def connect( g: Graph[Int], community: Set[Int], separator: Graph[Int] ): Graph[Int] = {
    val S: Set[Int] = separator.V

    // Create pairs of separator vertices that are not adjacent.
    //val pairs: Iterable[(Int, Int)] = Helper.combinations( S ).filterNot { case (s, t) => g.neighbours( s ).contains( t ) }

    // Connect each separator vertex to a random vertex in the community.
    val E: Set[(Int, Int)] = S.map( s => s -> selectRandom( community ).get )

    val clique: Set[Int] = GraphConnectivity.largestClique( separator )

    Helper.pairs( S ).filterNot{ case (u, v) => separator.neighbours(u).contains(v) }.foldLeft( g.addEdges( E ) ) {
      case (graph, (s: Int, t: Int)) =>
        // Determine the connectivity between s and t in the separator.
        //val k: Int = math.ceil( ( separatorSize - GraphConnectivity.vertexConnectivity(separator, s, t) ).toDouble / (communitiesPerSeparator - 1).toDouble ).toInt
        val k: Int = math.ceil( ( separatorSize - clique.size - GraphConnectivity.vertexConnectivity(separator.\(clique.--( Set(s, t) )), s, t) ).toDouble / (communitiesPerSeparator - 1).toDouble ).toInt
        makeKConnected( graph, s, t, k, community )
    }
  }

  @tailrec
  private def makeKConnected( g: Graph[Int], s: Int, t: Int, k: Int, vs: Set[Int] ): Graph[Int] = {
    if ( k < 1 ) {
      g
    } else {
      val Ns: Set[Int] = g.neighbours( s ).intersect( vs )
      val Nt: Set[Int] = g.neighbours( t ).intersect( vs )

      Ns.intersect( Nt ).headOption match {
        case Some( v: Int ) => makeKConnected( g, s, t, k - 1, vs - v )
        case None =>
          Nt.headOption match {
            case None =>
              // Create new neighbour.
              makeKConnected( g.addEdges( Set( t -> selectRandom( vs ).get ) ), s, t, k, vs )

            case Some( v: Int ) =>
              // The neighbours of s and t should not be used for constructing a path in the community
              // as that may result in additional neighbours being introduced.
              val allowedVertices: Set[Int] = vs -- Nt
              val pathLength: Int = math.ceil( allowedVertices.size.toDouble / k.toDouble ).toInt
              val p: List[Int] = randomPath( g, s, v, pathLength, allowedVertices )

              makeKConnected( g.addEdges( p zip p.tail ), s, t, k - 1, vs -- p.toSet )
          }
      }
    }
  }

  private def randomPath( g: Graph[Int], s: Int, t: Int, maxLength: Int, vs: Set[Int] ): List[Int] = {
    @tailrec
    def generatePath( p: List[Int], vs: Set[Int] ): List[Int] = {
      // Retrieve the last vertex that was added to the path.
      val v: Int = p.head
      val d: Double = util.Random.nextDouble()
      val u: Option[Int] = selectRandom( g.neighbours( v ).intersect( vs ) ).orElse( selectRandom( vs ) )

      if ( u.isEmpty || g.neighbours( v ).contains( t ) || p.size >= maxLength || (p.size > 1 && d > extendPathProb) ) {
        // Terminate the path if there are no more vertices to add to it, v is a neighbour of t, the maximum length
        // has been reached, or the probability that the path should be extended is below the threshold.
        p.prepended( t ).reverse
      } else {
        generatePath( p.prepended( u.get ), vs - u.get -- g.neighbours( v ) )
      }
    }

    generatePath( List(s), vs - s )
  }
}