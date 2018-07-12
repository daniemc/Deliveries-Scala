import org.scalatest._

import scala.util.{Failure, Success, Try}

class test extends FunSuite {

  trait Moves extends Product with Serializable
  case class A() extends Moves
  case class L() extends Moves
  case class R() extends Moves
  case class D() extends Moves

  trait Orientation
  case class N() extends Orientation
  case class E() extends Orientation
  case class S() extends Orientation
  case class O() extends Orientation

  trait PositionBuilder {
    val x: Int
    val y: Int
    val o: Orientation
  }

  case class Position(x: Int, y: Int, o: Orientation) extends PositionBuilder



  trait MapBuilder {
    val N: Int
    val E: Int
    val S: Int
    val O: Int
  }
  case class MapLimits(N: Int, E: Int, S: Int, O: Int) extends MapBuilder

  case class Delivery(route : List[Try[Moves]])

  trait DroneBuilder {
    val name: String
    val input: String
    val output: String
    val map: MapLimits
    val position: Position
    val delivery: Delivery
  }

  case class Drone(name: String, input: String, output: String,
                           map: MapLimits, position: Position, delivery: Delivery) extends DroneBuilder

  object FileService {
    val rootPath = System.getProperty("user.dir")
    val basePath = "src/main/resources"
    def fullPath = {
      s"${ rootPath }/${ basePath }"
    }
    def read(fileName: String) = {
      var lines : List[String] = List()
      val bufferedSource = scala.io.Source.fromFile(s"$fullPath/$fileName")
      bufferedSource.getLines().foreach(fileLine => {
        lines = lines :+ fileLine
      })
      bufferedSource.close()
      lines
    }
  }

  object DeliveryService {
    def prepareDelivery(delivery: List[String]) : Delivery = {

      val newDelivery = delivery.flatMap(address => {
        address.map(move => {
          move match {
            case 'A' => Success(A())
            case 'L' => Success(L())
            case 'R' => Success(R())
            case _ => Failure(new Exception("Move not valid"))
          }
        }).:+(Success(D()))

      })

      new Delivery(newDelivery)
    }
  }

  object DroneService {
    def input(name: String): String = {
      s"{$name}in.txt"
    }
    def output(name: String): String ={
      s"{$name}out.txt"
    }
    def prepareDrone(name: String, delivery: Delivery) : Drone = {
      val map = new MapLimits(10, 10, 10, 10)
      val position = new Position(0, 0, N())
      new Drone(name, input(name), output(name), map, position, delivery)
    }
  }



  test("can read a file") {
    val file = FileService.read("in.txt")
    assert(0 < file.length)
  }

  test("can prepare delivery") {
    val delivery = List("ALR", "LRf")
    val newDelivery = DeliveryService.prepareDelivery(delivery)
    assert(0 < newDelivery.route.length)
  }

  // leer el archivo
  // linea por linea
  // recorrer letra por letra
  // hacer un match
  // retornar una nueva instancia de moves dependiendo de la letra o un error (usar Try)
  //

}
