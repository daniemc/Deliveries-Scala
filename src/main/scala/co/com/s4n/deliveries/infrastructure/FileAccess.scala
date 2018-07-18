package co.com.s4n.deliveries.infrastructure

import java.io.{BufferedWriter, File, FileWriter}

import co.com.s4n.deliveries.domain.VO.{A, L, Move, R}
import co.com.s4n.deliveries.domain.entities.{Address, Delivery}

import scala.util.{Failure, Success, Try}

object FileAccess {
  val rootPath = System.getProperty("user.dir")
  val basePath = "src/main/resources"
  def fullPath = {
    s"${ rootPath }/${ basePath }"
  }


  def read(fileName: String): Try[List[String]] = {
    Try(scala.io.Source.fromFile(s"$fullPath/$fileName").getLines().toList)
  }

  def write(fileName: String, message: String) = {
      Try {
        val path = s"${fullPath}/$fileName"
        val writer = new BufferedWriter(new FileWriter(path, true))
        writer.write(message)
        writer.newLine()
        writer.close()
      }
  }

  def list(path: String): Try[List[String]] = {
    Try( new File(path).listFiles
      .filter(file => file.isFile)
      .map(file => file.getName)
      .filter(file => file.startsWith("in") && file.endsWith("txt"))
      .filter(file => file.length == 8)
      .toList )
  }

  def getDelivery(fileName: String): Try[Delivery] = {
    FileAccess.read(fileName)
      .map(fileList => getAddressList(fileList))
      .flatMap(tAddressList => tAddressList
        .map(new Delivery(_)))
  }

  def getAddressList(fileLines: List[String]): Try[List[Address]] = {
    Try(fileLines
      .map(addressString => getMoveList(addressString))
      .map(moveList => moveList match {
        case Success(ml) => new Address(ml)
        case Failure(err) => throw new Exception(err.getMessage)
      }))
  }

  def getMoveList(moves: String): Try[List[Move]] = {
      Try(moves.map(buildMoves(_)).map(_.get).toList)
  }

  def buildMoves(move: Char): Try[Move] = Try {
    move match {
      case 'A' => A()
      case 'L' => L()
      case 'R' => R()
      case _ => throw new Exception(s"Move($move) not valid")
    }
  }
 
}
