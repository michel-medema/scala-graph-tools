package nl.rug.ds.common

object Timer {
	def time[R](block: => R): (R, Long) = {
		val timer: Timer = Timer.start()

		(block, timer.elapsedNanos())
	}

	def start(): Timer = new Timer( System.nanoTime() )
}


final case class Timer(t0: Long) {
	def elapsedNanos(): Long = System.nanoTime() - t0

	def elapsedMillis(): Long = (elapsedNanos().toDouble / 1000000.0).toLong
}