package nl.rug.ds.common

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Path
import scala.util.{Try, Using}

trait FileHelper {
	/**
		* Returns the name of the file to which this path points without its extension.
		*/
	def getFileName( path: Path ): String = {
		stripExtension(path.getFileName.toString)
	}

  /**
   * Returns the filename without the extension. Note that this simple implementation
   * does not always correctly identify the extension: it, for instance, returns the
   * empty string for dot files.
   */
	def stripExtension(fileName: String): String = {
		fileName.lastIndexOf('.') match {
			case -1 => fileName
			case idx => fileName.substring(0, idx)
		}
	}

	def writeToFile(file: File, content: String): Try[Unit] = writeToFile(file, Iterator(content))

	def writeToFile(file: File, lines: Iterator[String]): Try[Unit] = {
		Using( new BufferedWriter( new FileWriter( file ) ) ) { writer =>
			lines.foreach( line => writer.write(line ++ "\n") )
		}
	}
}
