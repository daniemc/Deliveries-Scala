package co.com.s4n.deliveries
import co.com.s4n.deliveries.domain.VO.N
import co.com.s4n.deliveries.domain.entities.{MapLimits, Position}
import co.com.s4n.deliveries.domain.services.{DeliveryService, DroneService, MapLimitsService}
import co.com.s4n.deliveries.infrastructure.FileAccess
import org.scalatest._

import scala.util.Try

class DroneTest extends FunSuite {

  test("an input and output file name must") {
    val fileName = DroneService.output("01")
    assert("out01.txt" == fileName)
  }

  test("a dron can be prepared") {
    val preparedDrone = DroneService.prepareDrone("01")
    assert(preparedDrone.isSuccess)
  }

  test("a dron can deliver an order at a given position") {
    val position = new Position(2, 4, N())
    val droneName = "outTest.txt"
    val deliver = DroneService.deliverOrder(position, droneName)
    val file = FileAccess.read(droneName)
    assert("Delivery: (2, 4 - N())" == file(0))
  }

  test("dron can make delivers") {
    val delivery = Try(List("ALR", "LRA"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("30")
    val deliveriesResult =  Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isSuccess)
  }

  test("you can get the drone name of input file delivery") {
    val droneName = "01"
    val fileName = "in01.txt"
    assert(droneName == DroneService.getDroneNameFromFile(fileName))
  }

  test("pass bad moves to drone will fail") {
    val delivery = Try(List("AALR", "LRAB"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("31")
    val deliveriesResult = Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isFailure)
  }

  test("go beyond map limits will fail") {
    val delivery = Try(List("ALR", "LAAAAAAAAAAAARA"))
    val deliveries = DeliveryService.prepareDelivery(delivery, 3)
    val drone = DroneService.prepareDrone("32")
    val deliveriesResult = Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isFailure)
  }

  test("a dron can make delivers from file") {
    val file = Try(FileAccess.read("in.txt"))
    val deliveries = DeliveryService.prepareDelivery(file, 3)
    val drone = DroneService.prepareDrone("33")
    val deliveriesResult = Try(DroneService.makeDeliveries(drone, deliveries, MapLimitsService.defaultMap))
    assert(deliveriesResult.isSuccess)

  }

  test("a dron can make multi deliveries from files") {
    val deliveriesList = FileAccess.list
    val deliveriesResult = DroneService.multiDroneDelivery(deliveriesList)
    assert(0 < deliveriesResult.length)
  }
}
