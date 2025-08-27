package nl.rug.ds.graph.communities.quality

trait Quality[Vertex, Community] {
	def getVertices: List[Vertex]

	def partitioning: Map[Vertex, Community]

	def moveToCommunity(v: Vertex, c: Community): Quality[Vertex, Community]

	def gains(v: Vertex): List[(Community, Double)]
}