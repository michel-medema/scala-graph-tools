package nl.rug.ds.graph.format.parser

import nl.rug.ds.common.FileHelper

import java.io.File
import scala.io.Source
import scala.util.{Try, Using}

object SeparatorFile extends FileHelper {
  def read(file: String): Try[List[Set[Int]]] = {
    Using( Source.fromFile(file) ) { source =>
      source.getLines().toList.map { s =>
        s.split(',').toSet.map { (str: String) =>
          str.toIntOption match {
            case Some(i: Int) => i
            case None => throw new RuntimeException(s"Unexpected line $s in file $file.")
          }
        }
      }
    }
  }

  def write(file: File, separators: List[Set[Int]]): Try[Unit] = {
    writeToFile( file, separators.iterator.map( s => s.mkString(",") ) )
  }
}
