package nl.rug.ds.graph.tree.decomposition.format

import java.io.File
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

object PaceTD {
  private val Comment: Regex = """c( (.*))?""".r
  private val SolutionLine: Regex = """s td (\d+) (\d+) (\d+)""".r
  private val Bag: Regex = """b (\d+)((?: \d+)+)""".r

  def apply(f: String): Try[PaceTD] = PaceTD( new File(f) )
  def apply(file: File): Try[PaceTD] = {
    Using( Source.fromFile(file) ) { source =>
      val lines: Iterator[String] = source.getLines().filterNot( Comment.matches )

      lines.nextOption() match {
        case Some( s: String ) =>
          s match {
            case SolutionLine( _, maxBagSize: String, _ ) => PaceTD( maxBagSize.toInt - 1 )
            case _ => throw new RuntimeException( s"[${file.getName}] Expected valid solution line at the start of the file." )
          }
        case None => throw new RuntimeException( s"[${file.getName}] Expected valid solution line at the start of the file." )
      }
    }
  }
}

final case class PaceTD(tw: Int/*, bags: List[List[Int]]*/)