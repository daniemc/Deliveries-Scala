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

  trait Position {
    val x = 0
    val y = 0
    val o: Orientation = new N()
  }

  trait Limit {
    val limit: Int
  }

  trait CityMap
  case class N_Limit(limit: Limit) extends CityMap
  case class E_Limit(limit: Limit) extends CityMap
  case class S_Limit(limit: Limit) extends CityMap
  case class O_Limit(limit: Limit) extends CityMap

  trait Address {
    val movesToReach: List[Moves]
  }

  trait Delivery {
    val deliveries: List[Address]
  }

  trait Drone {
    val name: String
    val input: String
    val output: String
    val map: CityMap
    val position: Position
    val delivery: List[Delivery]
  }

  case class DeliveryDrone(name: String, input: String, output: String, map: CityMap, position: Position, delivery: List[Delivery]) extends Drone

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
    def prepareDelivery(delivery: List[String]) : List[Try[Moves]] = {

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

      newDelivery
    }
  }



  test("can read a file") {
    val file = FileService.read("in.txt")
    assert(0 < file.length)
  }

  test("can prepare delivery") {
    val delivery = List("ALR", "LRf")
    val newDelivery = DeliveryService.prepareDelivery(delivery)
    println(newDelivery)
  }

  // leer el archivo
  // linea por linea
  // recorrer letra por letra
  // hacer un match
  // retornar una nueva instancia de moves dependiendo de la letra o un error (usar Try)
  //

}
