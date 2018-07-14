import java.io.{BufferedWriter, File, FileWriter}

import co.com.s4n.deliveries.domain.entities.MapLimits
import co.com.s4n.deliveries.domain.services.{DeliveryService, DroneService}
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.{Failure, Success, Try}

class test extends FunSuite {

  test("can read a file") {
    val file = Try(FileAccess.read("in.txt"))
    assert(file.isSuccess)
  }

  test("if a bad file name is given should fail") {
    val file = Try(FileAccess.read("badFile.txt"))
    assert(file.isFailure)
  }

  test("can write a file") {
    val write = Try(FileAccess.write("test.txt", "test message"))
    assert(write.isSuccess)
  }

  test("writing a file should fail when i don't give a name or message") {
    val write = Try(FileAccess.write("", "test message"))
    val write2 = Try(FileAccess.write("text.txt", ""))
    assert(write.isFailure)
    assert(write2.isFailure)
  }

  test("can list files in a directory") {
    val files = Try(FileAccess.list)
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
    val file = Try(FileAccess.read("in.txt"))
    val mapLimit = new MapLimits(10, 10, -10, -10)
    val deliveries = DeliveryService.prepareDelivery(file, 3)
    val drone = DroneService.prepareDrone("01")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, mapLimit))
    assert(deliveriesResult.isSuccess)

  }

}
