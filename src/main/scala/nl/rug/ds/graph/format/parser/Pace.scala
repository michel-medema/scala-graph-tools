package nl.rug.ds.graph.format.parser

import nl.rug.ds.graph.common.Graph
import nl.rug.ds.common.FileHelper

import java.io.File
import java.nio.file.Path
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}

object Pace extends FileHelper {
  private val Comment: Regex = """c( (.*))?""".r
  private val Descriptor: Regex = """p tw (\d+) (\d+)""".r
  private val Edge: Regex = """(\d+) (\d+)""".r

  def load(path: Path): Try[Graph[Int]] = read( path.toFile )

  def save(path: Path, g: Graph[Int]): Try[Unit] = write( path.toFile, g )

  def format(g: Graph[Int]): Iterator[String] = {
    // TODO: Check format (i.e. that vertices are numbered from 1 to n)?
    Iterator( s"p tw ${g.n} ${g.m}" ) ++ g.vertices.flatMap { v =>
      g.neighbours( v ).filter( v < _ ).map( w => s"$v $w" )
    }
  }

  def read(file: String): Try[Graph[Int]] = read( new File(file) )

  def read(file: File): Try[Graph[Int]] = {
    Using( Source.fromFile(file) ) { source =>
      val lines: Iterator[String] = source.getLines().filterNot( Comment.matches )
      val s: Option[String] = lines.nextOption()

      if ( s.isEmpty || !Descriptor.matches( s.get ) ) {
        throw new RuntimeException(s"[${file.getName}] Expected valid graph descriptor at the start of the file.")
      }

      Graph[Int](
        lines.map {
          case Edge(u: String, v: String) => (u.toInt, v.toInt)
          case x => throw new RuntimeException(s"[${file.getName}] Unexpected line in input: $x.")
        }
      )
    }
  }

  def write(file: File, g: Graph[Int]): Try[Unit] = writeToFile( file, format(g) )
}
