package co.com.s4n.deliveries.infrastructure

import java.io.{BufferedWriter, File, FileWriter}

import co.com.s4n.deliveries.Config
import co.com.s4n.deliveries.domain.VO.{A, L, Move, R}
import co.com.s4n.deliveries.domain.entities.{Address, Delivery, Drone}

import scala.util.{Failure, Success, Try}

object FileAccess {

  def read(fileName: String): Try[List[String]] = {
    Try(scala.io.Source.fromFile(s"${Config.fullPath}/$fileName").getLines().toList)
  }

  def write(fileName: String, message: String) = {
      Try {
        val path = s"${Config.fullPath}/$fileName"
        val writer = new BufferedWriter(new FileWriter(path, true))
        writer.write(message)
        writer.newLine()
        writer.close()
      }
  }

  def list(path: String): Try[List[String]] = {
    Try {
      new File(path).listFiles
        .filter(file => file.isFile)
        .map(file => file.getName)
        .filter(file => file.startsWith("in") && file.endsWith("txt"))
        .filter(file => file.length == 8)
        .toList
    }
  }

  def getDelivery(fileName: String, ordersNumber: Int): Try[Delivery] = {
    FileAccess.read(fileName)
      .map(fileList => getAddressList(fileList, ordersNumber))
      .flatMap(tAddressList => tAddressList
        .map(new Delivery(_)))
  }

  def getAddressList(fileLines: List[String], ordersNumber: Int): Try[List[Address]] = {
    Try {
      fileLines
        .map(addressString => getMoveList(addressString))
        .map(moveList => moveList match {
          case Success(ml) => new Address(ml)
          case Failure(err) => throw new Exception(err.getMessage)
        }).take(ordersNumber)
    }
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

  def droneOutput(name: String): String =s"out${name}.txt"

  def report(drone: Drone): Drone = {
    val message = s"Delivery: (${drone.position.x}, ${drone.position.y} - ${drone.position.o})"
    FileAccess.write(droneOutput(drone.name), message)
    drone
  }

  def reportError(droneName: String, message: String) = {
    FileAccess.write("ErrorsReport.txt", s"[Error] Drone ${droneName} says: ${message}")
  }
 
}
