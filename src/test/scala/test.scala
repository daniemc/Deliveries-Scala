import java.io.{BufferedWriter, File, FileWriter}

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
    val position: Position
  }

  case class Drone(name: String, input: String, output: String,
                           position: Position) extends DroneBuilder

  object FileService {
    val rootPath = System.getProperty("user.dir")
    val basePath = "src/main/resources"
    def fullPath = {
      s"${ rootPath }/${ basePath }"
    }

    @throws(classOf[Exception])
    def read(fileName: String): List[String] = {
      var lines : List[String] = List()
      val bufferedSource = scala.io.Source.fromFile(s"$fullPath/$fileName")
      bufferedSource.getLines().foreach(fileLine => {
        lines = lines :+ fileLine
      })
      bufferedSource.close()
      lines
    }

    @throws(classOf[Exception])
    def write(fileName: String, message: String) = {
      if (message != "") {
        val path = s"${fullPath}/$fileName"
        val writer = new BufferedWriter(new FileWriter(path, true))
        writer.write(message)
        writer.newLine()
        writer.close()
      } else {
        throw new Exception("Write failed: A message must be provided")
      }
    }

    @throws(classOf[Exception])
    def list: List[String] = {
      new File(fullPath).listFiles
        .filter(file => file.isFile)
        .map(file => file.getName)
        .filter(file => file.startsWith("in") && file.endsWith("txt"))
        .toList
    }
  }

  object DeliveryService {
    def prepareDelivery(delivery: Try[List[String]], deliveriesNumber: Int) : Delivery = {
      delivery match {
        case Success(deliver) => new Delivery(deliver.take(deliveriesNumber)
          .flatMap(address => buildDeliveryRoute(address)))
        case Failure(err) => new Delivery(List())
      }
    }

    def buildDeliveryRoute(address: String) = {
      address.map(move => buildMoves(move)) :+(addDeliveryPoint)
    }

    def buildMoves(move: Char): Try[Moves] = {
      move match {
        case 'A' => Success(A())
        case 'L' => Success(L())
        case 'R' => Success(R())
        case _ => Failure(new Exception(s"Move($move) not valid"))
      }
    }

    def addDeliveryPoint: Try[Moves] = {
      Success(D())
    }
  }

  object OrientationService {
    def lFrom(position: Position): Position = {
      position.o match {
        case N() => new Position(position.x, position.y, O())
        case E() => new Position(position.x, position.y, N())
        case S() => new Position(position.x, position.y, E())
        case O() => new Position(position.x, position.y, S())
      }
    }

    def rFrom(position: Position): Position = {
      position.o match {
        case N() => new Position(position.x, position.y, E())
        case O() => new Position(position.x, position.y, N())
        case S() => new Position(position.x, position.y, O())
        case E() => new Position(position.x, position.y, S())
      }
    }
  }

  object PositionService {
    def move(move: Moves, position: Try[Position], cityMap: MapLimits): Try[Position] = {

          move match {
            case A() => advance(position.get, cityMap)
            case L() => Try(OrientationService.lFrom(position.get))
            case R() => Try(OrientationService.rFrom(position.get))
            case D() => Try(DroneService.delivery(position.get))
          }
    }

    def advance(position: Position, cityMap: MapLimits): Try[Position] = {
      position.o match {
        case N() => MapLimitsService.validateNS(new Position(position.x, position.y + 1, position.o), cityMap)
        case S() => MapLimitsService.validateNS(new Position(position.x, position.y - 1, position.o), cityMap)
        case O() => MapLimitsService.validateEO(new Position(position.x - 1, position.y, position.o), cityMap)
        case E() => MapLimitsService.validateEO(new Position(position.x + 1, position.y, position.o), cityMap)
      }
    }

  }

  object MapLimitsService {
    def validateNS(position: Position, cityMap: MapLimits): Try[Position] = {
      if (position.y >= cityMap.S && position.y <= cityMap.N) Success(position) else mapException(position)
    }

    def validateEO(position: Position, cityMap: MapLimits): Try[Position] = {
      if (position.x >= cityMap.O && position.x <= cityMap.E) Success(position) else mapException(position)
    }

    def mapException(position: Position): Try[Position] = {
      Failure(new Exception(s"Can't go beyond city map limits, Pos:($position)"))
    }

  }

  object DroneService {
    def defaultDrone: Drone = {
      val position = new Position(0, 0, N())
      new Drone("", "", "", position)
    }

    def input(name: String): String = {
      s"in${name}.txt"
    }

    def output(name: String): String ={
      s"out${name}.txt"
    }

    def prepareDrone(name: String) : Try[Drone] = {
      val position = new Position(0, 0, N())
      Try(new Drone(name, input(name), output(name), position))
    }

    def move(movement: Try[Moves], drone: Try[Drone], cityMap: MapLimits): Try[Drone] = {

      movement match {
        case Success(step) => {
          val newPosition = PositionService.move(step, drone.map(_.position), cityMap)

          newPosition match {
            case Success(newPoss) => drone.map(dr => new Drone(dr.name, dr.input, dr.input, newPoss))
            case Failure(err) => {
              reportError(drone.get.name, err.getMessage)
              Failure(new Exception(err.getMessage))
            }
          }

        }
        case Failure(err) => {
          reportError(drone.get.name, err.getMessage)
          Failure(new Exception(err.getMessage))
        }
      }
    }

    def makeDeliveries(drone: Try[Drone], delivery: Delivery, cityMap: MapLimits) = {
      var deliveryDrone = drone
      delivery.route.map(action => deliveryDrone = DroneService.move(action, deliveryDrone, cityMap))
    }

    def delivery(position: Position): Position = {
      val message = s"Delivery: (${position.x}, ${position.y} - ${position.o})"
      FileService.write("out.txt", message)
      position
    }

    def reportError(droneName: String, message: String) = {
      FileService.write("ErrorsReport.txt", s"[Error] Drone ${droneName} says: ${message}")
    }
  }



  test("can read a file") {
    val file = Try(FileService.read("in.txt"))
    assert(file.isSuccess)
  }

  test("if a bad file name is given should fail") {
    val file = Try(FileService.read("badFile.txt"))
    assert(file.isFailure)
  }

  test("can write a file") {
    val write = Try(FileService.write("test.txt", "test message"))
    assert(write.isSuccess)
  }

  test("writing a file should fail when i don't give a name or message") {
    val write = Try(FileService.write("", "test message"))
    val write2 = Try(FileService.write("text.txt", ""))
    assert(write.isFailure)
    assert(write2.isFailure)
  }

  test("can list files in a directory") {
    val files = Try(FileService.list)
    assert(files.isSuccess)
    files.map(fileList => assert(0 < fileList.length))
  }

  test("can prepare delivery") {
    val delivery = Try(List("ALR", "LRA"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    assert(0 < deliveries.route.length)
  }

  test("dron can make delivers") {
    val delivery = Try(List("ALR", "LRA"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isSuccess)
  }

  test("pass bad moves to drone will fail") {
    val delivery = Try(List("AALR", "LRAB"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isFailure)
  }

  test("go beyond map limits will fail") {
    val delivery = Try(List("ALR", "LAAAAAAAAAAAARA"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isFailure)
  }

  test("a dron can make delivers from file") {
    val file = Try(FileService.read("in.txt"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(file, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isSuccess)

  }

}
