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
        case _ => Failure(new Exception("Move not valid"))
      }
    }

    def addDeliveryPoint: Try[Moves] = {
      Success(D())
    }
  }

  object OrientationService {
    def lFrom(position: Position): Position = {
      position.o match {
        case N() => new Position(position.x, position.y, E())
        case E() => new Position(position.x, position.y, S())
        case S() => new Position(position.x, position.y, O())
        case O() => new Position(position.x, position.y, N())
      }
    }

    def rFrom(position: Position): Position = {
      position.o match {
        case N() => new Position(position.x, position.y, O())
        case O() => new Position(position.x, position.y, S())
        case S() => new Position(position.x, position.y, E())
        case E() => new Position(position.x, position.y, N())
      }
    }
  }

  object PositionService {
    def move(move: Moves, position: Try[Position]): Try[Position] = {
      position match {
        case Success(pos) => {
          move match {
            case A() => Try(advance(pos))
            case L() => Try(OrientationService.lFrom(pos))
            case R() => Try(OrientationService.rFrom(pos))
            case D() => Try(DroneService.delivery(pos))
          }
        }
        case Failure(err) => Failure(new Exception("Position not allowed"))
      }

    }

    def advance(position: Position) = {
      position.o match {
        case N() => new Position(position.x, position.y + 1, position.o)
        case S() => new Position(position.x, position.y - 1, position.o)
        case O() => new Position(position.x + 1, position.y, position.o)
        case E() => new Position(position.x - 1, position.y, position.o)
      }
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
      val map = new MapLimits(10, 10, -10, -10)
      val position = new Position(0, 0, N())
      Try(new Drone(name, input(name), output(name), position))
    }

    def move(movement: Try[Moves], drone: Try[Drone]): Try[Drone] = {

      movement match {
        case Success(step) => {
          val newPosition = PositionService.move(step, drone.map(_.position))

          newPosition match {
            case Success(newPoss) => drone.map(dr => new Drone(dr.name, dr.input, dr.input, newPoss))
            case Failure(err) => Failure(new Exception("Position not allowed"))
          }

        }
        case Failure(err) => Failure(new Exception(err.getMessage))
      }
    }

    def makeDeliveries(drone: Try[Drone], delivery: Delivery) = {
      var deliveryDrone = drone
      delivery.route.map(action => deliveryDrone = DroneService.move(action, deliveryDrone))
    }

    def delivery(position: Position): Position = {
      val message = s"Delivery: (${position.x}, ${position.y} - ${position.o})"
      FileService.write("out.txt", message)
      position
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
    val newDelivery = DeliveryService.prepareDelivery(delivery, 3)
    assert(0 < newDelivery.route.length)
  }

  test("a dron can make delivers") {
    val file = Try(FileService.read("in.txt"))
    val deliveries = DeliveryService.prepareDelivery(file, 3)
    var drone = DroneService.prepareDrone("01")
    DroneService.makeDeliveries(drone, deliveries)

  }

  // TODO
  // pass cityMap to make delivers
  // make a mapService
  // validate position in the map service
  // validation return a try of position
  // refactor other functions to accept that return type


}
