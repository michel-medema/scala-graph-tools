package nl.rug.ds.common

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.{Failure, Success, Try, Using}

trait FileHelper {
	/**
		*
		* Returns the name of the file to which this path points without its extension.
		*/
	def getFileName( path: Path ): String = {
		stripExtension(path.getFileName.toString)
	}

	def listFiles( path: Path ): Try[LazyList[Path]] = Try( Files.list(path).iterator().asScala.to(LazyList) )

	def stripTrailingSeparator(s: String): String = {
		val end: Int = if (s.endsWith(File.separator)) s.length - File.separator.length else s.length
		s.substring( 0, end )
	}

	def stripLeadingSeparator(s: String): String = {
		val start: Int = if (s.startsWith(File.separator)) File.separator.length else 0
		s.substring( start )
	}

	def stripExtension(fileName: String): String = {
		fileName.lastIndexOf('.') match {
			case -1 => fileName
			case idx => fileName.substring(0, idx)
		}
	}

	def buildPath( s: String, paths: String* ): String = {
		stripTrailingSeparator(s) ++ paths.map( s => stripTrailingSeparator(stripLeadingSeparator(s)) ).mkString(File.separator, File.separator, File.separator)
	}

	def buildFilePath( s: String, paths: String* ): String = {
		stripTrailingSeparator(s) ++
			File.separator ++
			stripLeadingSeparator(paths.init.map( s => stripTrailingSeparator(stripLeadingSeparator(s)) ).mkString("", File.separator, File.separator)) ++
			stripTrailingSeparator(stripLeadingSeparator(paths.last))
	}

	def writeToFile(file: File, content: String): Try[Unit] = writeToFile(file, Iterator(content))

	def writeToFile(file: File, lines: Iterator[String]): Try[Unit] = {
		Using( new BufferedWriter( new FileWriter( file ) ) ) { writer =>
			lines.foreach( line => writer.write(line ++ "\n") )
		}
	}

	def createDirectories(path: String): Boolean = {
		createDirectories( new File(path) )
	}

	def createDirectories(file: File): Boolean = {
		file.mkdirs()
	}

	def listFiles(dir: File, recursive: Boolean = false, filter: String => Boolean = _ => true ): Try[List[File]] = {
		@tailrec
		def processDirs(dirs: List[File], files: Queue[File]): Try[List[File]] = {
			dirs match {
				case Nil => Success(files.toList)
				case dir :: tail =>
					Try {
						dir.listFiles().toList.filter( file => filter(file.getName) ).partition( _.isFile )
					} match {
						case Success( t: (List[File], List[File]) ) =>
							if ( recursive ) {
								processDirs( tail prependedAll t._2, files ++ t._1 )
							} else {
								processDirs( tail, files ++ t._1 )
							}

						case Failure( e: Throwable ) => Failure( e )
					}
			}
		}

		processDirs(List(dir), Queue.empty)
	}
}
